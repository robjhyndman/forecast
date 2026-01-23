#include <math.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>

#define NONE 0
#define ADD 1
#define MULT 2
#define TOL 1.0e-10
#define HUGEN 1.0e10

// Functions called by R
SEXP etscalc(SEXP y, SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
  SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP nmse);
SEXP etssimulate(SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
  SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP h, SEXP e);
SEXP etsforecast(SEXP x, SEXP m, SEXP trend, SEXP season, SEXP phi, SEXP h);

// Internal functions
static void forecast(double l, double b, const double *s, int m, int trend,
  int season, double phi, double *f, int h);
static void update(const double *oldl, double *l, const double *oldb, double *b,
  const double *olds, double *s, int m, int trend, int season,
  double alpha, double beta, double gamma, double phi, double y);
void etscalc_internal(const double *y, int n, double *x, int m, int error, int trend, int season,
  double alpha, double beta, double gamma, double phi,
  double *e, double *fits, double *lik, double *amse, int nmse);

// ******************************************************************

void etscalc_internal(const double *y, int n, double *x, int m, int error, int trend, int season,
                      double alpha, double beta, double gamma, double phi,
                      double *e, double *fits, double *lik, double *amse, int nmse) {
  double oldl, l, oldb = 0.0, b = 0.0, olds[24], s[24], f[30], lik2, tmp, denom[30];

  if (m > 24 && season > NONE)
    return;
  if (m < 1)
    m = 1;

  if (nmse > 30)
    nmse = 30;

  const int nstates = m * (season > NONE) + 1 + (trend > NONE);

  // Copy initial state components
  l = x[0];
  if (trend > NONE)
    b = x[1];
  if (season > NONE) {
    memcpy(s, &x[(trend > NONE) + 1], m * sizeof(double));
  }

  *lik = 0.0;
  lik2 = 0.0;
  for (int j = 0; j < nmse; j++) {
    amse[j] = 0.0;
    denom[j] = 0.0;
  }

  for (int i = 0; i < n; i++) {
    // COPY PREVIOUS STATE
    oldl = l;
    if (trend > NONE)
      oldb = b;
    if (season > NONE)
      memcpy(olds, s, m * sizeof(double));

    // ONE STEP FORECAST
    forecast(oldl, oldb, olds, m, trend, season, phi, f, nmse);
    fits[i] = f[0];
    if (R_IsNA(fits[i])) {
      *lik = NA_REAL;
      return;
    }

    if (R_IsNA(y[i]))
      e[i] = NA_REAL;
    else if (error == ADD)
      e[i] = y[i] - fits[i];
    else
      e[i] = (y[i] - fits[i]) / fits[i];

    for (int j = 0; j < nmse; j++) {
      if (i + j < n) {
        denom[j] += 1.0;
        if (R_IsNA(y[i + j]))
          tmp = 0.0;
        else
          tmp = y[i + j] - f[j];
        amse[j] = (amse[j] * (denom[j] - 1.0) + (tmp * tmp)) / denom[j];
      }
    }

    // UPDATE STATE
    update(&oldl, &l, &oldb, &b, olds, s, m, trend, season, alpha, beta, gamma, phi, y[i]);

    // STORE NEW STATE
    x[nstates * (i + 1)] = l;
    if (trend > NONE)
      x[nstates * (i + 1) + 1] = b;
    if (season > NONE) {
      memcpy(&x[(trend > NONE) + nstates * (i + 1) + 1], s, m * sizeof(double));
    }
    if (!R_IsNA(e[i]))
      *lik = *lik + e[i] * e[i];
    lik2 += log(fabs(f[0]));
  }
  *lik = n * log(*lik);
  if (error == MULT)
    *lik += 2 * lik2;
}

SEXP etscalc(SEXP y, SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
             SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP nmse) {
  const int n_val = LENGTH(y);
  const int m_val = Rf_asInteger(m);
  const int error_val = Rf_asInteger(error);
  const int trend_val = Rf_asInteger(trend);
  const int season_val = Rf_asInteger(season);
  const double alpha_val = Rf_asReal(alpha);
  const double beta_val = Rf_asReal(beta);
  const double gamma_val = Rf_asReal(gamma);
  const double phi_val = Rf_asReal(phi);
  int nmse_val = Rf_asInteger(nmse);

  if (nmse_val > 30)
    nmse_val = 30;

  const double *y_ptr = REAL_RO(y);

  SEXP x_out = PROTECT(Rf_duplicate(x));
  SEXP e_out = PROTECT(Rf_allocVector(REALSXP, n_val));
  SEXP fits_out = PROTECT(Rf_allocVector(REALSXP, n_val));
  SEXP amse_out = PROTECT(Rf_allocVector(REALSXP, nmse_val));

  double lik;

  etscalc_internal(y_ptr, n_val, REAL(x_out), m_val, error_val, trend_val, season_val,
                   alpha_val, beta_val, gamma_val, phi_val,
                   REAL(e_out), REAL(fits_out), &lik, REAL(amse_out), nmse_val);


  const char *names[] = {"e", "fitted", "lik", "amse", "states", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, e_out);
  SET_VECTOR_ELT(result, 1, fits_out);
  SET_VECTOR_ELT(result, 2, Rf_ScalarReal(lik));
  SET_VECTOR_ELT(result, 3, amse_out);
  SET_VECTOR_ELT(result, 4, x_out);

  UNPROTECT(5);
  return result;
}

// *********************************************************************************

SEXP etssimulate(SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
                 SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP h, SEXP e) {
  int m_val = Rf_asInteger(m);
  const int error_val = Rf_asInteger(error);
  const int trend_val = Rf_asInteger(trend);
  const int season_val = Rf_asInteger(season);
  const double alpha_val = Rf_asReal(alpha);
  const double beta_val = Rf_asReal(beta);
  const double gamma_val = Rf_asReal(gamma);
  const double phi_val = Rf_asReal(phi);
  const int h_val = Rf_asInteger(h);

  if (m_val > 24 && season_val > NONE)
    return R_NilValue;
  if (m_val < 1)
    m_val = 1;

  const double *e_ptr = REAL_RO(e);
  const double *x_ptr = REAL_RO(x);

  double l = x_ptr[0];
  double b = 0.0;
  if (trend_val > NONE)
    b = x_ptr[1];

  double s[24];
  if (season_val > NONE) {
    memcpy(s, &x_ptr[(trend_val > NONE) + 1], m_val * sizeof(double));
  }

  SEXP result = PROTECT(Rf_allocVector(REALSXP, h_val));
  double *y = REAL(result);

  double oldl, oldb = 0.0, olds[24], f[10];

  for (int i = 0; i < h_val; i++) {
    oldl = l;
    if (trend_val > NONE)
      oldb = b;
    if (season_val > NONE)
      memcpy(olds, s, m_val * sizeof(double));

    // ONE STEP FORECAST
    forecast(oldl, oldb, olds, m_val, trend_val, season_val, phi_val, f, 1);
    if (R_IsNA(f[0])) {
      y[0] = NA_REAL;
      UNPROTECT(1);
      return result;
    }

    if (error_val == ADD)
      y[i] = f[0] + e_ptr[i];
    else
      y[i] = f[0] * (1.0 + e_ptr[i]);

    // UPDATE STATE
    update(&oldl, &l, &oldb, &b, olds, s, m_val, trend_val, season_val,
           alpha_val, beta_val, gamma_val, phi_val, y[i]);
  }

  UNPROTECT(1);
  return result;
}

// *********************************************************************************

SEXP etsforecast(SEXP x, SEXP m, SEXP trend, SEXP season, SEXP phi, SEXP h) {
  int m_val = Rf_asInteger(m);
  const int trend_val = Rf_asInteger(trend);
  const int season_val = Rf_asInteger(season);
  const double phi_val = Rf_asReal(phi);
  const int h_val = Rf_asInteger(h);

  if (m_val > 24 && season_val > NONE)
    return R_NilValue;
  if (m_val < 1)
    m_val = 1;

  const double *x_ptr = REAL_RO(x);

  double l = x_ptr[0];
  double b = 0.0;
  if (trend_val > NONE)
    b = x_ptr[1];

  double s[24];
  if (season_val > NONE) {
    memcpy(s, &x_ptr[(trend_val > NONE) + 1], m_val * sizeof(double));
  }

  SEXP result = PROTECT(Rf_allocVector(REALSXP, h_val));
  double *f_ptr = REAL(result);

  forecast(l, b, s, m_val, trend_val, season_val, phi_val, f_ptr, h_val);

  UNPROTECT(1);
  return result;
}

// *****************************************************************

static void forecast(double l, double b, const double *s, int m, int trend,
                     int season, double phi, double *f, int h) {
  double phistar = phi;

  // FORECASTS
  for (int i = 0; i < h; i++) {
    if (trend == NONE)
      f[i] = l;
    else if (trend == ADD)
      f[i] = l + phistar * b;
    else if (b < 0)
      f[i] = NA_REAL;
    else
      f[i] = l * pow(b, phistar);
    int j = m - 1 - i;
    while (j < 0)
      j += m;
    if (season == ADD)
      f[i] = f[i] + s[j];
    else if (season == MULT)
      f[i] = f[i] * s[j];
    if (i < (h - 1)) {
      if (fabs(phi - 1.0) < TOL)
        phistar = phistar + 1.0;
      else
        phistar = phistar + pow(phi, (double)(i + 1));
    }
  }
}

// *****************************************************************

static void update(const double *oldl, double *l, const double *oldb, double *b,
                   const double *olds, double *s, int m, int trend, int season,
                   double alpha, double beta, double gamma, double phi,
                   double y) {
  double q, p, r, t, phib;

  // NEW LEVEL
  if (trend == NONE) {
    q = *oldl; // l(t-1)
    phib = 0;
  } else if (trend == ADD) {
    phib = phi * (*oldb);
    q = *oldl + phib; // l(t-1) + phi*b(t-1)
  } else if (fabs(phi - 1.0) < TOL) {
    phib = *oldb;
    q = *oldl * (*oldb); // l(t-1)*b(t-1)
  } else {
    phib = pow(*oldb, phi);
    q = (*oldl) * phib; // l(t-1)*b(t-1)^phi
  }
  if (R_IsNA(y))
    p = q;
  else if (season == NONE)
    p = y;
  else if (season == ADD)
    p = y - olds[m - 1]; // y[t] - s[t-m]
  else {
    if (fabs(olds[m - 1]) < TOL)
      p = HUGEN;
    else
      p = y / olds[m - 1]; // y[t]/s[t-m]
  }
  *l = q + alpha * (p - q);

  // NEW GROWTH
  if (trend > NONE) {
    if (trend == ADD)
      r = (*l) - (*oldl); // l[t]-l[t-1]
    else {                // if(trend==MULT)
      if (fabs(*oldl) < TOL)
        r = HUGEN;
      else
        r = (*l) / (*oldl); // l[t]/l[t-1]
    }
    *b = phib + (beta / alpha) * (r - phib); // b[t] = phi*b[t-1] + beta*(r - phi*b[t-1])
                                // b[t] = b[t-1]^phi + beta*(r - b[t-1]^phi)
  }

  // NEW SEASON
  if (season > NONE) {
    if (R_IsNA(y))
      t = olds[m - 1];
    else if (season == ADD)
      t = y - q;
    else { // if(season==MULT)
      if (fabs(q) < TOL)
        t = HUGEN;
      else
        t = y / q;
    }
    s[0] = olds[m - 1] + gamma * (t - olds[m - 1]); // s[t] = s[t-m] + gamma*(t - s[t-m])
    memcpy(&s[1], olds, (m - 1) * sizeof(double));
  }
}

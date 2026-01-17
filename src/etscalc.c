#include <math.h>
#include <R.h>
#include <Rinternals.h>

#define NONE 0
#define ADD 1
#define MULT 2
#define DAMPED 1
#define TOL 1.0e-10
#define HUGEN 1.0e10

// Functions called by R
SEXP etscalc(SEXP y, SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
  SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP nmse);
SEXP etssimulate(SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
  SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP h, SEXP e);
SEXP etsforecast(SEXP x, SEXP m, SEXP trend, SEXP season, SEXP phi, SEXP h);

// Internal functions
void forecast(double, double, double *, int, int, int, double, double *, int);
void update(double *, double *, double *, double *, double *, double *, int, int, int,
  double, double, double, double, double);
void etscalc_internal(double *y, int n, double *x, int m, int error, int trend, int season,
  double alpha, double beta, double gamma, double phi,
  double *e, double *fits, double *lik, double *amse, int nmse);

// ******************************************************************

void etscalc_internal(double *y, int n, double *x, int m, int error, int trend, int season,
                      double alpha, double beta, double gamma, double phi,
                      double *e, double *fits, double *lik, double *amse, int nmse) {
  int i, j, nstates;
  double oldl, l, oldb, b, olds[24], s[24], f[30], lik2, tmp, denom[30];

  if (m > 24 && season > NONE)
    return;
  if (m < 1)
    m = 1;

  if (nmse > 30)
    nmse = 30;

  nstates = m * (season > NONE) + 1 + (trend > NONE);

  // Copy initial state components
  l = x[0];
  if (trend > NONE)
    b = x[1];
  if (season > NONE) {
    for (j = 0; j < m; j++)
      s[j] = x[(trend > NONE) + j + 1];
  }

  *lik = 0.0;
  lik2 = 0.0;
  for (j = 0; j < nmse; j++) {
    amse[j] = 0.0;
    denom[j] = 0.0;
  }

  for (i = 0; i < n; i++) {
    // COPY PREVIOUS STATE
    oldl = l;
    if (trend > NONE)
      oldb = b;
    if (season > NONE) {
      for (j = 0; j < m; j++)
        olds[j] = s[j];
    }
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

    for (j = 0; j < nmse; j++) {
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
       for (j = 0; j < m; j++)
        x[(trend > NONE) + nstates * (i + 1) + j + 1] = s[j];
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
  int n_val = length(y);
  int m_val = asInteger(m);
  int error_val = asInteger(error);
  int trend_val = asInteger(trend);
  int season_val = asInteger(season);
  double alpha_val = asReal(alpha);
  double beta_val = asReal(beta);
  double gamma_val = asReal(gamma);
  double phi_val = asReal(phi);
  int nmse_val = asInteger(nmse);

  if (nmse_val > 30)
    nmse_val = 30;

  const double *y_ptr = REAL_RO(y);

  SEXP x_out = PROTECT(duplicate(x));
  double *x_ptr = REAL(x_out);

  SEXP e_out = PROTECT(allocVector(REALSXP, n_val));
  double *e_ptr = REAL(e_out);

  SEXP fits_out = PROTECT(allocVector(REALSXP, n_val));
  double *fits_ptr = REAL(fits_out);

  SEXP amse_out = PROTECT(allocVector(REALSXP, nmse_val));
  double *amse_ptr = REAL(amse_out);

  double lik;

  etscalc_internal((double *)y_ptr, n_val, x_ptr, m_val, error_val, trend_val, season_val,
                   alpha_val, beta_val, gamma_val, phi_val,
                   e_ptr, fits_ptr, &lik, amse_ptr, nmse_val);

  SEXP result = PROTECT(allocVector(VECSXP, 5));
  SET_VECTOR_ELT(result, 0, e_out);
  SET_VECTOR_ELT(result, 1, fits_out);
  SET_VECTOR_ELT(result, 2, ScalarReal(lik));
  SET_VECTOR_ELT(result, 3, amse_out);
  SET_VECTOR_ELT(result, 4, x_out);

  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, mkChar("e"));
  SET_STRING_ELT(names, 1, mkChar("fitted"));
  SET_STRING_ELT(names, 2, mkChar("lik"));
  SET_STRING_ELT(names, 3, mkChar("amse"));
  SET_STRING_ELT(names, 4, mkChar("states"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(6);
  return result;
}

// *********************************************************************************

SEXP etssimulate(SEXP x, SEXP m, SEXP error, SEXP trend, SEXP season,
                 SEXP alpha, SEXP beta, SEXP gamma, SEXP phi, SEXP h, SEXP e) {
  int m_val = asInteger(m);
  int error_val = asInteger(error);
  int trend_val = asInteger(trend);
  int season_val = asInteger(season);
  double alpha_val = asReal(alpha);
  double beta_val = asReal(beta);
  double gamma_val = asReal(gamma);
  double phi_val = asReal(phi);
  int h_val = asInteger(h);

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
    for (int j = 0; j < m_val; j++)
      s[j] = x_ptr[(trend_val > NONE) + j + 1];
  }

  SEXP result = PROTECT(allocVector(REALSXP, h_val));
  double *y = REAL(result);

  double oldl, oldb, olds[24], f[10];

  for (int i = 0; i < h_val; i++) {
    oldl = l;
    if (trend_val > NONE)
      oldb = b;
    if (season_val > NONE) {
      for (int j = 0; j < m_val; j++)
        olds[j] = s[j];
    }
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
  int m_val = asInteger(m);
  int trend_val = asInteger(trend);
  int season_val = asInteger(season);
  double phi_val = asReal(phi);
  int h_val = asInteger(h);

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
    for (int i = 0; i < m_val; i++) {
      s[i] = x_ptr[(trend_val > NONE) + i + 1];
    }
  }

  SEXP result = PROTECT(allocVector(REALSXP, h_val));
  double *f_ptr = REAL(result);

  forecast(l, b, s, m_val, trend_val, season_val, phi_val, f_ptr, h_val);

  UNPROTECT(1);
  return result;
}

// *****************************************************************

void forecast(double l, double b, double *s, int m, int trend, int season, double phi, double *f, int h)
{
  int i,j;
  double phistar;

  phistar = phi;

  // FORECASTS
  for(i=0; i<h; i++)
  {
    if(trend == NONE)
      f[i] = l;
    else if(trend == ADD)
      f[i] = l + phistar*b;
    else if(b<0)
      f[i] = NA_REAL;
    else
      f[i] = l * pow(b,phistar);
    j = m-1-i;
    while(j < 0)
       j += m;
    if(season == ADD)
      f[i] = f[i] + s[j];
    else if(season == MULT)
      f[i] = f[i] * s[j];
    if(i < (h-1))
    {
      if(fabs(phi-1.0) < TOL)
        phistar = phistar + 1.0;
      else
        phistar = phistar + pow(phi, (double) (i+1));
    }
  }
}

// *****************************************************************

void update(double *oldl, double *l, double *oldb, double *b, double *olds, double *s, int m, int trend, int season,
  double alpha, double beta, double gamma, double phi, double y)
{
  int j;
  double q, p, r, t, phib;

  // NEW LEVEL
  if(trend==NONE)
  {
    q = *oldl;         // l(t-1)
    phib = 0;
  }
  else if(trend==ADD)
  {
    phib = phi*(*oldb);
    q = *oldl + phib;      // l(t-1) + phi*b(t-1)
  }
  else if(fabs(phi-1.0) < TOL)
  {
    phib = *oldb;
    q = *oldl * (*oldb);     // l(t-1)*b(t-1)
  }
  else
  {
    phib = pow(*oldb,phi);
    q = (*oldl) * phib;      // l(t-1)*b(t-1)^phi
  }
  if(R_IsNA(y))
    p = q;
  else if(season==NONE)
    p = y;
  else if(season==ADD)
    p = y - olds[m-1];     // y[t] - s[t-m]
  else
  {
    if(fabs(olds[m-1]) < TOL)
      p = HUGEN;
    else
      p = y / olds[m-1];   // y[t]/s[t-m]
  }
  *l = q + alpha*(p-q);

  // NEW GROWTH
  if(trend > NONE)
  {
    if(trend==ADD)
       r = (*l) - (*oldl);     // l[t]-l[t-1]
    else //if(trend==MULT)
    {
      if(fabs(*oldl) < TOL)
        r = HUGEN;
      else
        r = (*l)/(*oldl);  // l[t]/l[t-1]
    }
    *b = phib + (beta/alpha)*(r - phib);   // b[t] = phi*b[t-1] + beta*(r - phi*b[t-1])
                         // b[t] = b[t-1]^phi + beta*(r - b[t-1]^phi)
  }

  // NEW SEASON
  if(season > NONE)
  {
    if(R_IsNA(y))
      t = olds[m-1];
    else if(season==ADD)
      t = y - q;
    else //if(season==MULT)
    {
      if(fabs(q) < TOL)
        t = HUGEN;
      else
        t = y / q;
    }
    s[0] = olds[m-1] + gamma*(t - olds[m-1]); // s[t] = s[t-m] + gamma*(t - s[t-m])
    for(j=1; j<m; j++)
      s[j] = olds[j-1];           // s[t] = s[t]
  }
}

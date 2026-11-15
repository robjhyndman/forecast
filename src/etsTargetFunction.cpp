#include "etsTargetFunction.h"

#include <R.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <numeric>

EtsTargetFunction::EtsTargetFunction(const std::vector<double> &p_y,
                                     int p_nstate,
                                     int p_errortype,
                                     int p_trendtype,
                                     int p_seasontype,
                                     const std::vector<double> &p_lower,
                                     const std::vector<double> &p_upper,
                                     const std::string &p_opt_crit,
                                     int p_nmse,
                                     const std::string &p_bounds,
                                     int p_m,
                                     bool p_optAlpha,
                                     bool p_optBeta,
                                     bool p_optGamma,
                                     bool p_optPhi,
                                     bool p_givenBeta,
                                     bool p_givenGamma,
                                     double p_alpha,
                                     double p_beta,
                                     double p_gamma,
                                     double p_phi)
    : y(p_y),
      nstate(p_nstate),
      errortype(p_errortype),
      trendtype(p_trendtype),
      seasontype(p_seasontype),
      lower(p_lower),
      upper(p_upper),
      opt_crit(p_opt_crit),
      nmse(p_nmse),
      bounds(p_bounds),
      m(p_m),
      n(static_cast<int>(p_y.size())),
      alpha(p_alpha),
      beta(p_beta),
      gamma(p_gamma),
      phi(p_phi),
      e(n, 0),
      fits(n, 0),
      amse(30, 0),
      lik(0),
      objval(0),
      optAlpha(p_optAlpha),
      optBeta(p_optBeta),
      optGamma(p_optGamma),
      optPhi(p_optPhi),
      givenBeta(p_givenBeta),
      givenGamma(p_givenGamma) {}

void EtsTargetFunction::eval(const double *p_par, int p_par_length) {
  // Check if the parameter configuration has changed, if not, just return.
  if (par.size() == static_cast<std::size_t>(p_par_length) && std::equal(par.begin(), par.end(), p_par)) {
    return;
  }

  this->par.assign(p_par, p_par + p_par_length);

  int j = 0;
  if (optAlpha) this->alpha = par[j++];
  if (optBeta) this->beta = par[j++];
  if (optGamma) this->gamma = par[j++];
  if (optPhi) this->phi = par[j++];

  if (!this->check_params()) {
    this->objval = R_PosInf;
    return;
  }

  this->state.assign(par.end() - nstate, par.end());

  if (seasontype != 0) {
    // Seasonal states follow the level and, if present, the trend
    const int season_start = (trendtype != 0) ? 2 : 1;

    // Add extra state so that the seasonal states sum to m (multiplicative) or 0 (additive)
    const double sum = std::accumulate(state.begin() + season_start, state.end(), 0.0);
    const double new_state = (seasontype == 2 ? m : 0) - sum;

    state.push_back(new_state);

    // Check states
    if (seasontype == 2 &&
        std::any_of(state.begin() + season_start, state.end(), [](double s) { return s < 0; })) {
      this->objval = R_PosInf;
      return;
    }
  }

  etscalc_internal(this->y.data(), this->n, this->state.data(), nullptr, this->m,
                   this->errortype, this->trendtype, this->seasontype,
                   this->alpha, this->beta, this->gamma, this->phi,
                   this->e.data(), this->fits.data(), &this->lik,
                   this->amse.data(), this->nmse);

  // Avoid perfect fits
  this->lik = std::max(this->lik, -1e10);

  if (ISNAN(this->lik)) {
    this->objval = R_PosInf;
    return;
  }

  if (this->opt_crit == "lik")
    this->objval = this->lik;
  else if (this->opt_crit == "mse")
    this->objval = this->amse[0];
  else if (this->opt_crit == "amse") {
    double mean = 0;
    for (int i = 0; i < this->nmse; i++) {
      mean += amse[i] / this->nmse;
    }
    this->objval = mean;

  } else if (this->opt_crit == "sigma") {
    double mean = 0;
    const int ne = e.size();
    for (int i = 0; i < ne; i++) {
      mean += e[i] * e[i] / ne;
    }
    this->objval = mean;

  } else if (this->opt_crit == "mae") {
    double mean = 0;
    const int ne = e.size();
    for (int i = 0; i < ne; i++) {
      mean += std::fabs(e[i]) / ne;
    }
    this->objval = mean;
  }
}

bool EtsTargetFunction::check_params() {
  if (bounds != "admissible") {
    if (optAlpha) {
      if (alpha < lower[0] || alpha > upper[0]) return false;
    }
    if (optBeta) {
      if (beta < lower[1] || beta > alpha || beta > upper[1]) return false;
    }
    if (optPhi) {
      if (phi < lower[3] || phi > upper[3]) return false;
    }
    if (optGamma) {
      if (gamma < lower[2] || gamma > 1 - alpha || gamma > upper[2])
        return false;
    }
  }
  if (bounds != "usual") {
    if (!admissible()) return false;
  }
  return true;
}

bool EtsTargetFunction::admissible() {
  if (phi < 0 || phi > 1 + 1e-8) return false;

  // If gamma was set by the user or it is optimized, the bounds need to be
  // enforced
  if (!optGamma && !givenGamma) {
    if (alpha < 1 - 1 / phi || alpha > 1 + 1 / phi) return false;

    if (optBeta || givenBeta) {
      if (beta < alpha * (phi - 1) || beta > (1 + phi) * (2 - alpha))
        return false;
    }
  } else if (m > 1) {  // Seasonal model
    if (!optBeta && !givenBeta) beta = 0;

    // max(1-1/phi-alpha,0)
    const double d = 1 - 1 / phi - alpha;
    if (gamma < ((d > 0) ? d : 0) || gamma > 1 + 1 / phi - alpha)
      return false;

    if (alpha < 1 - 1 / phi - gamma * (1 - m + phi + phi * m) / (2 * phi * m))
      return false;

    if (beta < -(1 - phi) * (gamma / m + alpha)) return false;

    // End of easy tests. Now use characteristic equation

    std::vector<double> opr;
    opr.push_back(1);
    opr.push_back(alpha + beta - phi);

    for (int i = 0; i < m - 2; i++) opr.push_back(alpha + beta - alpha * phi);

    opr.push_back(alpha + beta - alpha * phi + gamma - 1);
    opr.push_back(phi * (1 - alpha - gamma));

    int degree = opr.size() - 1;

    std::vector<double> opi;
    opi.resize(opr.size(), 0);

    std::vector<double> zeror(degree);
    std::vector<double> zeroi(degree);

    bool fail;

    R_cpolyroot(opr.data(), opi.data(), &degree, zeror.data(), zeroi.data(),
                &fail);

    if (fail) return false;

    double max = 0;
    for (std::size_t i = 0; i < zeror.size(); i++) {
      const double abs_val = std::sqrt(zeror[i] * zeror[i] + zeroi[i] * zeroi[i]);
      max = std::max(max, abs_val);
    }

    if (max > 1 + 1e-10) return false;
  }

  return true;
}

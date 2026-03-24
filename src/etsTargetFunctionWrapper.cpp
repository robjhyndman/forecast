#include <string>
#include <vector>

// For R's Nelder-Mead solver
#include <R_ext/Applic.h>
#include <Rcpp.h>

#include "etsTargetFunction.h"

// This function initializes all the parameters, constructs an
// object of type EtsTargetFunction and adds an external pointer
// to this object with name "ets.xptr"
// to the environment submitted as rho
//
// [[Rcpp::export]]
Rcpp::Environment etsTargetFunctionInit(const Rcpp::NumericVector &y,
                                        int nstate,
                                        int errortype,
                                        int trendtype,
                                        int seasontype,
                                        bool damped,
                                        const Rcpp::NumericVector &lower,
                                        const Rcpp::NumericVector &upper,
                                        const std::string &opt_crit,
                                        int nmse,
                                        const std::string &bounds,
                                        int m,
                                        bool optAlpha,
                                        bool optBeta,
                                        bool optGamma,
                                        bool optPhi,
                                        bool givenAlpha,
                                        bool givenBeta,
                                        bool givenGamma,
                                        bool givenPhi,
                                        double alpha,
                                        double beta,
                                        double gamma,
                                        double phi,
                                        Rcpp::Environment &rho) {
  EtsTargetFunction* sp = new EtsTargetFunction();

  sp->init(Rcpp::as<std::vector<double>>(y), nstate, errortype, trendtype,
           seasontype, damped, Rcpp::as<std::vector<double>>(lower),
           Rcpp::as<std::vector<double>>(upper), opt_crit, nmse, bounds, m,
           optAlpha, optBeta, optGamma, optPhi, givenAlpha, givenBeta,
           givenGamma, givenPhi, alpha, beta, gamma, phi);

  rho["ets.xptr"] = Rcpp::XPtr<EtsTargetFunction>(sp, true);
  return rho;
}

static double targetFunctionEtsNelderMead(int n, double *par, void *ex) {
  EtsTargetFunction* sp = static_cast<EtsTargetFunction*>(ex);
  sp->eval(par, n);
  return sp->getObjVal();
}

// [[Rcpp::export]]
Rcpp::List etsNelderMead(Rcpp::NumericVector par,
                         const Rcpp::Environment &env,
                         double abstol,
                         double intol,
                         double alpha,
                         double beta,
                         double gamma,
                         int trace,
                         int maxit) {
  int fncount = 0, fail = 0;
  double Fmin = 0.0;
  Rcpp::NumericVector opar(par.size());

  Rcpp::XPtr<EtsTargetFunction> sp(env.get("ets.xptr"));

  nmmin(par.size(), par.begin(), opar.begin(), &Fmin,
        targetFunctionEtsNelderMead, &fail, abstol, intol, sp, alpha, beta,
        gamma, trace, &fncount, maxit);

  return Rcpp::List::create(
    Rcpp::Named("value") = Fmin,
    Rcpp::Named("par") = opar,
    Rcpp::Named("fail") = fail,
    Rcpp::Named("fncount") = fncount
  );
}


#include <vector>
#include <string>

#include <R_ext/Error.h>

//For R's Nelder-Mead solver
#include <R_ext/Applic.h>

#include <Rcpp.h>

#include "etsTargetFunction.h"

// This function initializes all the parameters, constructs an
// object of type EtsTargetFunction and adds an external pointer
// to this object with name "ets.xptr"
// to the environment submitted as p_rho
//
RcppExport SEXP etsTargetFunctionInit(SEXP p_y, SEXP p_nstate, SEXP p_errortype, SEXP p_trendtype,
		SEXP p_seasontype, SEXP p_damped, SEXP p_lower, SEXP p_upper,
		SEXP p_opt_crit, SEXP p_nmse, SEXP p_bounds, SEXP p_m,
		SEXP p_optAlpha, SEXP p_optBeta, SEXP p_optGamma, SEXP p_optPhi,
		SEXP p_givenAlpha, SEXP p_givenBeta, SEXP p_givenGamma, SEXP p_givenPhi,
		SEXP p_alpha, SEXP p_beta, SEXP p_gamma, SEXP p_phi, SEXP p_rho) {

	BEGIN_RCPP;

	EtsTargetFunction* sp = new EtsTargetFunction();

	std::vector<double> y = Rcpp::as< std::vector<double> >(p_y);

	int nstate = Rcpp::as<int>(p_nstate);

	int errortype = Rcpp::as<int>(p_errortype);
	int trendtype = Rcpp::as<int>(p_trendtype);
	int seasontype = Rcpp::as<int>(p_seasontype);

	bool damped = Rcpp::as<bool>(p_damped);

	std::vector<double> lower = Rcpp::as< std::vector<double> >(p_lower);
	std::vector<double> upper = Rcpp::as< std::vector<double> >(p_upper);

	std::string opt_crit = Rcpp::as<std::string>(p_opt_crit);
	double nmse = Rcpp::as<double>(p_nmse);

	std::string bounds = Rcpp::as< std::string >(p_bounds);
	int m = Rcpp::as<int>(p_m);

	bool optAlpha = Rcpp::as<bool>(p_optAlpha);
	bool optBeta = Rcpp::as<bool>(p_optBeta);
	bool optGamma = Rcpp::as<bool>(p_optGamma);
	bool optPhi = Rcpp::as<bool>(p_optPhi);

	bool givenAlpha = Rcpp::as<bool>(p_givenAlpha);
	bool givenBeta = Rcpp::as<bool>(p_givenBeta);
	bool givenGamma = Rcpp::as<bool>(p_givenGamma);
	bool givenPhi = Rcpp::as<bool>(p_givenPhi);

	double alpha = Rcpp::as<double>(p_alpha);
	double beta = Rcpp::as<double>(p_beta);
	double gamma = Rcpp::as<double>(p_gamma);
	double phi = Rcpp::as<double>(p_phi);


	sp->init(y, nstate, errortype, trendtype, seasontype, damped, lower, upper, opt_crit,
			nmse, bounds, m, optAlpha, optBeta, optGamma, optPhi,
			givenAlpha, givenBeta, givenGamma, givenPhi,
			alpha, beta, gamma, phi);

	Rcpp::Environment e(p_rho);
	e["ets.xptr"] = Rcpp::XPtr<EtsTargetFunction>( sp, true );

	return Rcpp::wrap(e);

	END_RCPP;
}

RcppExport double targetFunctionRmalschains(SEXP p_par, SEXP p_env)
{
	Rcpp::NumericVector par(p_par);

	Rcpp::Environment e(p_env);
	Rcpp::XPtr<EtsTargetFunction> sp(e.get("ets.xptr"));

	sp->eval(par.begin(), par.size());

	//return Rcpp::wrap(sp->getObjVal());
	return sp->getObjVal();

}

RcppExport SEXP etsGetTargetFunctionRmalschainsPtr() {

	typedef double (*funcPtr)(SEXP, SEXP);
	return (Rcpp::XPtr<funcPtr>(new funcPtr(&targetFunctionRmalschains)));
}

/*
RcppExport SEXP targetFunctionRdonlp2(SEXP p_var, SEXP p_env)
{

	Rcpp::Environment e(p_env);
	Rcpp::XPtr<EtsTargetFunction> sp(e.get("ets.xptr"));

	Rcpp::NumericVector var(p_var);

	int mode = var[0];
	int fun_id = var[1];

	sp->eval(var.begin()+2, var.size()-2);

	if(mode == 0) {
		if(fun_id == 0) {
			return Rcpp::wrap(sp->getObjVal());
		} else {
			return Rcpp::wrap(0);
			//return Rcpp::wrap(sp->restrictions[fun_id-1]);
		}
	} else if(mode==1) {
		//		error("Gradients are not implemented, exiting.");
	};

	return R_NilValue;
}

RcppExport SEXP etsGetTargetFunctionRdonlp2Ptr() {

	typedef SEXP (*funcPtr)(SEXP, SEXP);
	return (Rcpp::XPtr<funcPtr>(new funcPtr(&targetFunctionRdonlp2)));
}
*/


double targetFunctionEtsNelderMead(int n, double *par, void *ex)
{
	EtsTargetFunction* sp = (EtsTargetFunction*) ex;

	sp->eval(par, n);
	return sp->getObjVal();

}


RcppExport SEXP etsNelderMead(SEXP p_var, SEXP p_env, SEXP p_abstol,
		SEXP p_intol, SEXP p_alpha, SEXP p_beta, SEXP p_gamma,
		SEXP p_trace, SEXP p_maxit)
{

	double abstol = Rcpp::as<double>(p_abstol);
	double intol = Rcpp::as<double>(p_intol);
	double alpha = Rcpp::as<double>(p_alpha);
	double beta= Rcpp::as<double>(p_beta);
	double gamma= Rcpp::as<double>(p_gamma);

	int trace = Rcpp::as<int>(p_trace);
	int maxit = Rcpp::as<int>(p_maxit);

	int fncount = 0, fail=0;
	double Fmin = 0.0;

	Rcpp::NumericVector dpar(p_var);
	Rcpp::NumericVector opar(dpar.size());

	Rcpp::Environment e(p_env);
	Rcpp::XPtr<EtsTargetFunction> sp(e.get("ets.xptr"));

	double (*funcPtr)(int n, double *par, void *ex) = targetFunctionEtsNelderMead;

	nmmin(dpar.size(), dpar.begin(), opar.begin(), &Fmin, funcPtr,
			&fail, abstol, intol, sp, alpha, beta, gamma, trace, &fncount, maxit);

	return Rcpp::List::create(Rcpp::Named("value") = Fmin,
			Rcpp::Named("par") = opar,
			Rcpp::Named("fail") = fail,
			Rcpp::Named("fncount") = fncount);

}




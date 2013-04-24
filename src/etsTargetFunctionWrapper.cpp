
#include <vector>
#include <string>

#include <R_ext/Error.h>

#include <Rcpp.h>

#include "etsTargetFunction.h"

/*
int findNameInVector(std::vector< std::string > names, std::string name) {

	int j=-1;

	for(int i=0; i< names.size(); i++) {
		if(names[i] == name) j=i;      
	}

	return(j);

}
*/

RcppExport SEXP EtsTargetFunctionObjectFactory(){
	return Rcpp::XPtr<EtsTargetFunction>( new EtsTargetFunction, true ) ;
}


RcppExport SEXP EtsTargetFunctionInit(SEXP p_y, SEXP p_nstate, SEXP p_errortype, SEXP p_trendtype,
		SEXP p_seasontype, SEXP p_damped, SEXP p_lower, SEXP p_upper,
		SEXP p_opt_crit, SEXP p_nmse, SEXP p_bounds, SEXP p_m,
		SEXP p_useAlpha, SEXP p_useBeta, SEXP p_useGamma, SEXP p_usePhi,
		SEXP p_alpha, SEXP p_beta, SEXP p_gamma, SEXP p_phi) {

	BEGIN_RCPP;

	EtsTargetFunction::deleteTargetFunctionSingleton();

	EtsTargetFunction* sp = EtsTargetFunction::getTargetFunctionSingleton();

	//std::vector<double> par = Rcpp::as< std::vector<double> >(p_par);
	std::vector<double> y = Rcpp::as< std::vector<double> >(p_y);

	int nstate = Rcpp::as<int>(p_nstate);

	int errortype = Rcpp::as<int>(p_errortype);
	int trendtype = Rcpp::as<int>(p_trendtype);
	int seasontype = Rcpp::as<int>(p_seasontype);

	bool damped = Rcpp::as<bool>(p_damped);

	//if (p_par_noopt != R_NilValue)
	//	par_noopt = Rcpp::as< std::vector<double> >(p_par_noopt);
	std::vector<double> lower = Rcpp::as< std::vector<double> >(p_lower);
	std::vector<double> upper = Rcpp::as< std::vector<double> >(p_upper);

	std::string opt_crit = Rcpp::as<std::string>(p_opt_crit);
	double nmse = Rcpp::as<double>(p_nmse);

	std::string bounds = Rcpp::as< std::string >(p_bounds);
	int m = Rcpp::as<int>(p_m);

	bool useAlpha = Rcpp::as<bool>(p_useAlpha);
	bool useBeta = Rcpp::as<bool>(p_useBeta);
	bool useGamma = Rcpp::as<bool>(p_useGamma);
	bool usePhi = Rcpp::as<bool>(p_usePhi);

	double alpha = Rcpp::as<double>(p_alpha);
	double beta = Rcpp::as<double>(p_beta);
	double gamma = Rcpp::as<double>(p_gamma);
	double phi = Rcpp::as<double>(p_phi);


	sp->init(y, nstate, errortype, trendtype, seasontype, damped, lower, upper, opt_crit,
			nmse, bounds, m, useAlpha, useBeta, useGamma, usePhi, alpha, beta, gamma, phi);

	return R_NilValue;

	END_RCPP;
}

RcppExport SEXP targetFunctionRmalschains(SEXP p_par)
{
	Rcpp::NumericVector par(p_par);

	EtsTargetFunction* sp = EtsTargetFunction::getTargetFunctionSingleton();

	sp->eval(par.begin(), par.size());

	/*
	Rcpp::NumericVector res(5);

	res[0] = sp->objval;

	res[1] = sp->restrictions[0];
	res[2] = sp->restrictions[1];
	res[3] = sp->restrictions[2];
	res[4] = sp->restrictions[3];

	return res;
	 */

	return Rcpp::wrap(sp->getObjVal());

}

RcppExport SEXP EtsGetTargetFunctionRmalschainsPtr() {
	//Rcpp::XPtr<EtsTargetFunction> sp(xp);

	typedef SEXP (*funcPtr)(SEXP);

	return (Rcpp::XPtr<funcPtr>(new funcPtr(&targetFunctionRmalschains)));
}


RcppExport SEXP targetFunctionRdonlp2(SEXP p_var)
{

	Rcpp::NumericVector var(p_var);

	int mode = var[0];
	int fun_id = var[1];

	EtsTargetFunction* sp = EtsTargetFunction::getTargetFunctionSingleton();

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

RcppExport SEXP EtsGetTargetFunctionRdonlp2Ptr() {
	//Rcpp::XPtr<EtsTargetFunction> sp(xp);

	typedef SEXP (*funcPtr)(SEXP);

	return (Rcpp::XPtr<funcPtr>(new funcPtr(&targetFunctionRdonlp2)));
}

//Environment env(p_env); // save just SEXP as hash and compare on SEXP?


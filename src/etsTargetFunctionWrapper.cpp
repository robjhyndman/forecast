//    #include <algorithm>
#include<vector>
#include<string>

#include <R_ext/Error.h>

#include <Rcpp.h>

#include "etsTargetFunction.h"


int findNameInVector(std::vector< std::string > names, std::string name) {

	int j=-1;

	for(int i=0; i< names.size(); i++) {
		if(names[i] == name) j=i;      
	}

	return(j);

}


RcppExport SEXP EtsTargetFunctionObjectFactory(){
	return Rcpp::XPtr<EtsTargetFunction>( new EtsTargetFunction, true ) ;
}


//lik <- function(par,y,nstate,errortype,trendtype,seasontype,damped,par.noopt,lower,upper,
//    opt.crit,nmse,bounds,m,pnames,pnames2)

RcppExport SEXP EtsTargetFunctionInit(SEXP p_par, SEXP p_y, SEXP p_nstate, SEXP p_errortype, SEXP p_trendtype, 
		SEXP p_seasontype, SEXP p_damped, SEXP p_par_noopt, SEXP p_lower, SEXP p_upper, 
		SEXP p_opt_crit, SEXP p_nmse, SEXP p_bounds, SEXP p_m, SEXP p_pnames, SEXP p_pnames2, 
                SEXP p_useAlpha, SEXP p_useBeta, SEXP p_useGamma, SEXP p_usePhi, SEXP p_alpha, SEXP p_beta, SEXP p_gamma, SEXP p_phi) {

	BEGIN_RCPP;

	EtsTargetFunction::deleteTargetFunctionSingleton();

	EtsTargetFunction* sp = EtsTargetFunction::getTargetFunctionSingleton();

	//Rcpp::NumericVector par(p_par);
	//Rcpp::NumericVector y(p_y);
	
//TODO: Add some more checks for NULL etc.

//Rprintf("1\n");

	std::vector<double> par = Rcpp::as< std::vector<double> >(p_par);
	std::vector<double> y = Rcpp::as< std::vector<double> >(p_y);

//Rprintf("2\n");
	
	int nstate = Rcpp::as<int>(p_nstate);
	
int errortype = Rcpp::as<int>(p_errortype);
int trendtype = Rcpp::as<int>(p_trendtype);
int seasontype = Rcpp::as<int>(p_seasontype);

//	std::string errortype = Rcpp::as<std::string>(p_errortype);
//	std::string trendtype = Rcpp::as<std::string>(p_trendtype);
//	std::string seasontype = Rcpp::as<std::string>(p_seasontype);
//Rprintf("3\n");
	
	bool damped = Rcpp::as<bool>(p_damped);
//Rprintf("4\n");	
	//Rcpp::NumericVector par_noopt(p_par_noopt);
	//Rcpp::NumericVector lower(p_lower);
	//Rcpp::NumericVector upper(p_upper);

        std::vector<double> par_noopt;
//Rprintf("4a\n");
        if (p_par_noopt != R_NilValue)	
          par_noopt = Rcpp::as< std::vector<double> >(p_par_noopt);
//Rprintf("4b\n");
	std::vector<double> lower = Rcpp::as< std::vector<double> >(p_lower);
//Rprintf("4c\n");
	std::vector<double> upper = Rcpp::as< std::vector<double> >(p_upper);
//Rprintf("5\n");
	
	std::string opt_crit = Rcpp::as<std::string>(p_opt_crit);
//Rprintf("6\n");	
	double nmse = Rcpp::as<double>(p_nmse);
//Rprintf("7\n");	
	
	std::string bounds = Rcpp::as< std::string >(p_bounds);
	int m = Rcpp::as<int>(p_m);
//Rprintf("8\n");	

std::vector<std::string> pnames;
std::vector<std::string> pnames2;

if (p_pnames != R_NilValue)
 pnames = Rcpp::as< std::vector<std::string> >(p_pnames);

if (p_pnames2 != R_NilValue)
	pnames2 = Rcpp::as< std::vector<std::string> >(p_pnames2);

//Rprintf("9\n");	
	bool useAlpha = Rcpp::as<bool>(p_useAlpha);
	bool useBeta = Rcpp::as<bool>(p_useBeta);
	bool useGamma = Rcpp::as<bool>(p_useGamma);
	bool usePhi = Rcpp::as<bool>(p_usePhi);

//        double alpha = Rcpp::as<double>(p_alpha);
//        double beta = Rcpp::as<double>(p_beta);
//        double gamma = Rcpp::as<double>(p_gamma);
//        double phi = Rcpp::as<double>(p_phi);

//Rprintf("10\n");	
	sp->init(par, y, nstate, errortype, trendtype, seasontype, damped, par_noopt, lower, upper, opt_crit,
			nmse, bounds, m, pnames, pnames2, useAlpha, useBeta, useGamma, usePhi);
//Rprintf("11\n");
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

//Rprintf("I'm here, returning 1...");

        return Rcpp::wrap(sp->getLik());

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
			return Rcpp::wrap(sp->getLik());
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









RcppExport SEXP etsLikFunction(SEXP p_alpha, SEXP p_beta, SEXP p_gamma, SEXP p_phi, SEXP p_m, SEXP p_env, SEXP p_names) {

	BEGIN_RCPP;

	double alpha = Rcpp::as<double>(p_alpha);
	double beta = Rcpp::as<double>(p_beta);
	double gamma = Rcpp::as<double>(p_gamma);
	double phi = Rcpp::as<double>(p_phi);
	unsigned int m = Rcpp::as<unsigned int>(p_m);

	std::vector< std::string > names = Rcpp::as<std::vector< std::string > >(p_names);


	int pos1 = findNameInVector(names, "alpha"); //std::find(names.begin(), names.end(), "alpha");
	int pos2 = findNameInVector(names, "alphsdfsdfa");

	Rprintf("%d\n", pos1);
	Rprintf("%d\n", pos2);
	//Rprintf("%d", names.end());

	/*    
if (pos != names.end())
    {
      // Element in vector.
      Rprintf("%d", pos);
    }
	 */

	Rprintf("the first name is:\n");
	Rprintf(names[0].c_str());
	Rprintf("\n");


	//Environment env(p_env); // save just SEXP as hash and compare on SEXP? 



	/*
    if(is.null(phi))
        phi <- 1
    if(phi < 0 | phi > 1+1e-8)
        return(0)
    if(is.null(gamma))
    {
        if(alpha < 1-1/phi | alpha > 1+1/phi)
            return(0)
        if(!is.null(beta))
        {
            if(beta < alpha * (phi-1) | beta > (1+phi)*(2-alpha))
                return(0)
        }
    }
    else if(m > 1) # Seasonal model
    {
        if(is.null(beta))
            beta <- 0
        if(gamma < max(1-1/phi-alpha,0) | gamma > 1+1/phi-alpha)
            return(0)
        if(alpha < 1-1/phi-gamma*(1-m+phi+phi*m)/(2*phi*m))
            return(0)
        if(beta < -(1-phi)*(gamma/m+alpha))
            return(0)

        # End of easy tests. Now use characteristic equation
        P <- c(phi*(1-alpha-gamma),alpha+beta-alpha*phi+gamma-1,rep(alpha+beta-alpha*phi,m-2),(alpha+beta-phi),1)
        roots <- polyroot(P)
        if(max(abs(roots)) > 1+1e-10)
            return(0)
    }
    # Passed all tests
    return(1)
	 */


	//Rcpp::NumericVector alpha(xalpha);
	//Rcpp::NumericVector alpha(xalpha);
	//Rcpp::NumericVector xa = sapply( x, ::fabs );

	Rcpp::NumericVector res(4);
	res[0] = alpha;
	res[1] = beta;
	res[2] = m;

	return(res);

	END_RCPP;
}


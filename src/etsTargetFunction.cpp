#include <cmath>
#include <R.h>

//for isnan, math.h is needed
//#include <math.h>

#include "etsTargetFunction.h"

#include <R_ext/Print.h>

void EtsTargetFunction::init(std::vector<double> & p_y, int p_nstate, int p_errortype,
		int p_trendtype, int p_seasontype, bool p_damped,
		std::vector<double> & p_lower, std::vector<double> & p_upper, std::string p_opt_crit,
		int p_nmse, std::string p_bounds, int p_m,
		bool p_optAlpha, bool p_optBeta, bool p_optGamma, bool p_optPhi,
		bool p_givenAlpha, bool p_givenBeta, bool p_givenGamma, bool p_givenPhi,
		double alpha, double beta, double gamma, double phi) {

	this->y = p_y;
	this->n = this->y.size();
	this->nstate = p_nstate;
	this->errortype = p_errortype;

	this->trendtype = p_trendtype;
	this->seasontype = p_seasontype;
	this->damped = p_damped;

	this->lower = p_lower;
	this->upper = p_upper;

	this->opt_crit = p_opt_crit;
	this->nmse = p_nmse;
	this->bounds = p_bounds;

	this->m = p_m;

	this->optAlpha = p_optAlpha;
	this->optBeta = p_optBeta;
	this->optGamma = p_optGamma;
	this->optPhi = p_optPhi;

	this->givenAlpha = p_givenAlpha;
	this->givenBeta = p_givenBeta;
	this->givenGamma = p_givenGamma;
	this->givenPhi = p_givenPhi;

	this->alpha = alpha;
	this->beta = beta;
	this->gamma = gamma;
	this->phi = phi;

	this->lik = 0;
	this->objval = 0;

	//	for(int i=0; i < 10; i++) this->amse.push_back(0);
	//	for(int i=0; i < n; i++) this->e.push_back(0);
	this->amse.resize(30, 0);
	this->e.resize(n, 0);
	this->fits.resize(n, 0);

}

void EtsTargetFunction::eval(const double* p_par, int p_par_length) {

	bool equal=true;

	//	---------show params----------
	//	Rprintf("par: ");
	//	for(int j=0;j < p_par_length;j++) {
	//		Rprintf("%f ", p_par[j]);
	//	}
	//	Rprintf(" objval: %f\n", this->objval);
	//Rprintf("\n");
	//	---------show params----------

	// Check if the parameter configuration has changed, if not, just return.
	if(p_par_length != this->par.size()) {
		equal=false;
	} else {
		for(int j=0;j < p_par_length;j++) {
			if(p_par[j] != this->par[j]) {
				equal=false;
				break;
			}
		}
	}

	if(equal) return;

	this->par.clear();

	for(int j=0;j < p_par_length;j++) {
		this->par.push_back(p_par[j]);
	}

	int j=0;
	if(optAlpha) this->alpha = par[j++];
	if(optBeta) this->beta = par[j++];
	if(optGamma) this->gamma = par[j++];
	if(optPhi) this->phi = par[j++];

	if(!this->check_params()) {
		this->objval = R_PosInf;
		return;
	}

	this->state.clear();

	for(int i=par.size()-nstate; i < par.size(); i++) {

		this->state.push_back(par[i]);
	}

	// Add extra state
	if(seasontype!=0) {//"N"=0, "M"=2
		//init.state <- c(init.state, m*(seasontype==2) - sum(init.state[(2+(trendtype!=0)):nstate]))

		double sum=0;

		for(int i=(1+((trendtype!=0) ? 1 : 0));i<nstate;i++) {
			sum += state[i];
		}

		double new_state = m*((seasontype==2) ? 1 : 0) - sum;

		state.push_back(new_state);
	}

	// Check states
	if(seasontype==2)
	{

		double min = R_PosInf;
		int start = 1;
		if(trendtype!=0) start=2;

		for(int i=start; i<state.size(); i++) {
			if(state[i] < min) min = state[i];
		}

		if(min < 0) {
			this->objval = R_PosInf;
			return;
		}

		//  seas.states <- init.state[-(1:(1+(trendtype!=0)))]
		//if(min(seas.states) < 0)
		//  return(1e8)
	};

	int p = state.size();

	for(int i=0; i <= p*this->y.size(); i++) state.push_back(0);

	etscalc(&this->y[0], &this->n, &this->state[0], &this->m, &this->errortype, &this->trendtype, &this->seasontype,
			&this->alpha, &this->beta, &this->gamma, &this->phi, &this->e[0], &this->fits[0], &this->lik, &this->amse[0], &this->nmse);

	// Avoid perfect fits
	if (this->lik < -1e10) this->lik = -1e10;

	// isnan() is a C99 function
	//if (isnan(this->lik)) this->lik = 1e8;
	if (ISNAN(this->lik)) this->lik = R_PosInf;

	if(fabs(this->lik+99999) < 1e-7) this->lik = R_PosInf;

	if(this->opt_crit=="lik") this->objval = this->lik;
	else if(this->opt_crit=="mse") this->objval = this->amse[0];
	else if(this->opt_crit=="amse") {

		//return(mean(e$amse[1:nmse]))
		double mean=0;
		for(int i=0;i < this->nmse;i++) {
			mean+=amse[i]/this->nmse;
		}
		this->objval=mean;

	}
	else if(this->opt_crit=="sigma") {
		//return(mean(e$e^2))
		double mean=0;
		int ne=e.size();
		for(int i=0;i<ne;i++) {
			mean+=e[i]*e[i]/ne;
		}
		this->objval=mean;

	}
	else if(this->opt_crit=="mae") {
		//return(mean(abs(e$e)))

		double mean=0;
		int ne=e.size();
		for(int i=0;i<ne;i++) {
			mean+=fabs(e[i])/ne;
		}
		this->objval=mean;

	}

}


bool EtsTargetFunction::check_params() {

	if(bounds != "admissible")
	{
		if(optAlpha)
		{
			if(alpha < lower[0] || alpha > upper[0])
				return(false);
		}
		if(optBeta)
		{
			if(beta < lower[1] || beta > alpha || beta > upper[1])
				return(false);
		}
		if(optPhi)
		{
			if(phi < lower[3] || phi > upper[3])
				return(false);
		}
		if(optGamma)
		{
			if(gamma < lower[2] || gamma > 1-alpha || gamma > upper[2])
				return(false);
		}
	}
	if(bounds != "usual")
	{
		if(!admissible()) return(false);
	}
	return(TRUE);

}

bool EtsTargetFunction::admissible() {

	if(phi < 0 || phi > 1+1e-8) return(false);

	//If gamma was set by the user or it is optimized, the bounds need to be enforced
	if(!optGamma && !givenGamma) {
		if(alpha < 1-1/phi || alpha > 1+1/phi) return(false);

		if(optBeta || givenBeta)
		{
			if(beta < alpha * (phi-1) || beta > (1+phi)*(2-alpha)) return(false);
		}
	}

	else if(m > 1) //Seasonal model
	{

		if(!optBeta && !givenBeta) beta = 0;

		//max(1-1/phi-alpha,0)
		double d = 1-1/phi-alpha;
		if(gamma < ((d > 0) ? d : 0) || gamma > 1+1/phi-alpha) return(false);

		if(alpha < 1-1/phi-gamma*(1-m+phi+phi*m)/(2*phi*m)) return(false);

		if(beta < -(1-phi)*(gamma/m+alpha)) return(false);

		// End of easy tests. Now use characteristic equation

		std::vector<double> opr;
		opr.push_back(1);
		opr.push_back(alpha+beta-phi);

		for(int i=0;i<m-2;i++) opr.push_back(alpha+beta-alpha*phi);

		opr.push_back(alpha+beta-alpha*phi+gamma-1);
		opr.push_back(phi*(1-alpha-gamma));

		int degree = opr.size()-1;

		std::vector<double> opi;
		opi.resize(opr.size(),0);

		std::vector<double> zeror(degree);
		std::vector<double> zeroi(degree);

		Rboolean fail;

		cpolyroot(&opr[0], &opi[0], &degree, &zeror[0], &zeroi[0], &fail);

		double max = 0;
		for(int i=0;i<zeror.size();i++) {
		  double abs_val = sqrt(zeror[i]*zeror[i] + zeroi[i]*zeroi[i]);
		  if(abs_val>max) max = abs_val;
		}

		//Rprintf("maxpolyroot: %f\n", max);

		if(max > 1+1e-10) return(false);

		// P <- c(phi*(1-alpha-gamma),alpha+beta-alpha*phi+gamma-1,rep(alpha+beta-alpha*phi,m-2),(alpha+beta-phi),1)
		// roots <- polyroot(P)
		// if(max(abs(roots)) > 1+1e-10) return(false);
	}

	//Passed all tests
	return(true);
}

#include <cmath>

#include "etsTargetFunction.h"

#include <R_ext/Print.h>

using namespace std;

EtsTargetFunction* EtsTargetFunction::EtsTargetFunctionSingleton = 0;

void EtsTargetFunction::deleteTargetFunctionSingleton() {
	if( EtsTargetFunctionSingleton == 0 ) return;
	else {
          delete EtsTargetFunctionSingleton;
          EtsTargetFunctionSingleton = 0;
        }
	return;
}

EtsTargetFunction* EtsTargetFunction::getTargetFunctionSingleton() {
	if( EtsTargetFunctionSingleton == 0 )
		EtsTargetFunctionSingleton = new EtsTargetFunction();
	return EtsTargetFunctionSingleton;
}


//etsInitFunction(SEXP p_par, SEXP p_y, SEXP p_nstate, SEXP p_errortype, SEXP p_trendtype, 
//		SEXP p_seasontype, SEXP p_damped, SEXP p_par_noopt, SEXP p_lower, SEXP p_upper, 
//		SEXP p_opt_crit, SEXP p_nmse, SEXP p_bounds, SEXP p_m, SEXP p_pnames, SEXP p_pnames2 )

void EtsTargetFunction::init(std::vector<double> & p_par, std::vector<double> & p_y, int p_nstate, int p_errortype, 
                             int p_trendtype, int p_seasontype, bool p_damped, std::vector<double> & p_par_noopt, 
                             std::vector<double> & p_lower, std::vector<double> & p_upper, std::string p_opt_crit, 
                             double p_nmse, std::string p_bounds, int p_m, std::vector<std::string> & p_pnames, 
                             std::vector<std::string> & p_pnames2, bool p_useAlpha, bool p_useBeta, bool p_useGamma, bool p_usePhi
//, double alpha, double beta, double gamma, double phi
) {

//Rprintf("1\n");
	this->par = p_par;
	this->y = p_y;
this->n = this->y.size();
        this->nstate = p_nstate;
        this->errortype = p_errortype;

//Rprintf("2\n");

this->trendtype = p_trendtype;
this->seasontype = p_seasontype;
this->damped = p_damped;

//Rprintf("3\n");

this->par_noopt = p_par_noopt;
this->lower = p_lower;
this->upper = p_upper;

//Rprintf("4\n");

this->opt_crit = p_opt_crit;
this->nmse = p_nmse;
this->bounds = p_bounds;

//Rprintf("5\n");

this->m = p_m;
this->pnames = p_pnames;
this->pnames2 = p_pnames2;

//Rprintf("6\n");

this->useAlpha = p_useAlpha;
this->useBeta = p_useBeta;
this->useGamma = p_useGamma;
this->usePhi = p_usePhi;

int j=0;
if(useAlpha) this->alpha = par[j++];
if(useBeta) this->beta = par[j++];
if(useGamma) this->gamma = par[j++];
if(usePhi) this->phi = par[j++];


this->lik = 0;
for(int i=0; i < 10; i++) this->amse.push_back(0);
for(int i=0; i < n; i++) this->e.push_back(0);


//Rprintf("7\n");

/*
	this->n = D.size();

	this->i.resize(n+p,0);

	this->e.resize(n, 0);
	this->e_mas.resize(n, 0);
	this->F.resize(n, 0);
	this->b.resize(n, 0);

	this->fpe.resize(n, 0);
	this->ape.resize(n, 0);
*/

}

void EtsTargetFunction::eval(const double* p_par, int p_par_length) {

	bool equal=true;

	//Rprintf("\nEtsTargetFunction: 1,%d\n", p_par_length);
	//Rprintf("\nEtsTargetFunction: 2,%d\n", par.size());

//---------show params----------
//        Rprintf("par: ");
//	for(int j=0;j < p_par_length;j++) {
//          Rprintf("%f ", p_par[j]);
//	}
//        Rprintf("\n");
//---------show params----------


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

	//Rprintf("\nEtsTargetFunction: 2\n");

	if(equal) return;

	//Rprintf("\nEtsTargetFunction: 3\n");

	this->par.clear();

	for(int j=0;j < p_par_length;j++) {
		this->par.push_back(p_par[j]);
	}

int j=0;
if(useAlpha) this->alpha = par[j++];
if(useBeta) this->beta = par[j++];
if(useGamma) this->gamma = par[j++];
if(usePhi) this->phi = par[j++];


if(!this->check_params()) {
//TODO: What happens for other measures?
        this->lik = 1e12;
        return;
      }


//int np = par.size();
//std::vector<double> init_state;
//std::copy(par.end()-nstate+1, par.end(), init_state);

this->state.clear();

for(int i=par.size()-nstate; i < par.size(); i++) {

this->state.push_back(par[i]);
}


//std::vector<double>::const_iterator first = par.end()-nstate+1;
//std::vector<double>::const_iterator last = par.end();
//std::vector<double> init_state(first, last);

    // Add extra state
    if(seasontype!=0) {//"N"=0, "M"=2
        //init.state <- c(init.state, m*(seasontype==2) - sum(init.state[(2+(trendtype!=0)):nstate]))

      int sum=0;

        for(int i=(2+(trendtype!=0));i<nstate;i++) {
          sum += state[i];
        }

        double new_state = m*(seasontype==2) - sum;

        state.push_back(new_state);
    }

    // Check states
    if(seasontype==2)
    {

      double min = state[0];
      int leaveOut = 1;
      if(trendtype!=0) leaveOut++;

      for(int i=0; i<(state.size()-leaveOut); i++) {
        if(state[i] < min) min = state[i];
      }

      if(min < 0) {
//TODO: What happens for other measures?
        this->lik = 1e8;
        return;
      }

      //  seas.states <- init.state[-(1:(1+(trendtype!=0)))]
        //if(min(seas.states) < 0)
          //  return(1e8)
    };


int p = state.size();

for(int i=0; i <= p*this->y.size(); i++) state.push_back(0);


/*
int lenx = nstate*(y.size()+1);
std::vector<double> x(lenx);

int j=0;
for(int i=par.end()-nstate+1; i!=par.end();i++) {

x[j++] = par[i];

}
*/


//    p <- length(init.state)
//    x <- numeric(p*(n+1))

/*
    np <- length(par)

    init.state <- par[(np-nstate+1):np]
    # Add extra state
    if(seasontype!="N")
        init.state <- c(init.state, m*(seasontype=="M") - sum(init.state[(2+(trendtype!="N")):nstate]))
    # Check states
    if(seasontype=="M")
    {
        seas.states <- init.state[-(1:(1+(trendtype!="N")))]
        if(min(seas.states) < 0)
            return(1e8)
    }

    e <- pegelsresid.C(y,m,init.state,errortype,trendtype,seasontype,damped,alpha,beta,gamma,phi)

    p <- length(init.state)
    x <- numeric(p*(n+1))
    x[1:p] <- init.state

*/


//void etscalc(double *y, int *n, double *x, int *m, int *error, int *trend, int *season,
  //  double *alpha, double *beta, double *gamma, double *phi, double *e, double *lik, double *amse)




etscalc(&this->y[0], &this->n, &this->state[0], &this->m, &this->errortype, &this->trendtype, &this->seasontype,
    &this->alpha, &this->beta, &this->gamma, &this->phi, &this->e[0], &this->lik, &this->amse[0]);

if (this->lik < -1e10) this->lik = -1e10; // Avoid perfect fits
//TODO:
if (this->lik == NAN) this->lik = 1e8;


//Rprintf("lik: %f\n", this->lik);




/*
double *x, 
int *m, 
int *error, 
int *trend, 
int *season,
double *alpha, 
double *beta, 
double *gamma, 
double *phi, 
double *e, 
double *lik, 
double *amse

*/


/**********************************

pegelsresid.C <- function(y,m,init.state,errortype,trendtype,seasontype,damped,alpha,beta,gamma,phi)
{
    n <- length(y)
    p <- length(init.state)
    x <- numeric(p*(n+1))
    x[1:p] <- init.state
    e <- numeric(n)
    lik <- 0;
    if(!damped)
        phi <- 1;
    if(trendtype == "N")
        beta <- 0;
    if(seasontype == "N")
        gamma <- 0;

        as.integer(switch(errortype,"A"=1,"M"=2)),
        as.integer(switch(trendtype,"N"=0,"A"=1,"M"=2)),
        as.integer(switch(seasontype,"N"=0,"A"=1,"M"=2)),

    Cout <- .C("etscalc",
        as.double(y),
        as.integer(n),
        as.double(x),
        as.integer(m),
        as.integer(switch(errortype,"A"=1,"M"=2)),
        as.integer(switch(trendtype,"N"=0,"A"=1,"M"=2)),
        as.integer(switch(seasontype,"N"=0,"A"=1,"M"=2)),
        as.double(alpha),
        as.double(beta),
        as.double(gamma),
        as.double(phi),
        as.double(e),
        as.double(lik),
        as.double(numeric(10)),
        PACKAGE="forecast")
    if(!is.na(Cout[[13]]))
    {
        if(abs(Cout[[13]]+99999) < 1e-7)
            Cout[[13]] <- NA
    }
    tsp.y <- tsp(y)
    e <- ts(Cout[[12]])
    tsp(e) <- tsp.y

    return(list(lik=Cout[[13]], amse=Cout[[14]], e=e, states=matrix(Cout[[3]], nrow=n+1, ncol=p, byrow=TRUE)))
}


**********************************/



}



/*
check.param <- function(alpha,beta,gamma,phi,lower,upper,bounds,m)
{
}
*/

bool EtsTargetFunction::check_params() {

    if(bounds != "admissible")
    {
        if(useAlpha)
        {
            if(alpha < lower[1] || alpha > upper[1])
                return(FALSE);
        }
        if(useBeta)
        {
            if(beta < lower[2] || beta > alpha || beta > upper[2])
                return(FALSE);
        }
        if(usePhi)
        {
            if(phi < lower[4] || phi > upper[4])
                return(FALSE);
        }
        if(useGamma)
        {
            if(gamma < lower[3] || gamma > 1-alpha || gamma > upper[3])
                return(FALSE);
        }
    }
    if(bounds != "usual")
    {
//        if(!admissible(alpha,beta,gamma,phi,m))
  //          return(FALSE);
    }
    return(TRUE);

}



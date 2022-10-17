#include "calcBATS.h"

using namespace Rcpp ;

SEXP updateTBATSGammaBold(SEXP gammaBold_s, SEXP kVector_s, SEXP gammaOne_s, SEXP gammaTwo_s) {
	BEGIN_RCPP

	NumericMatrix gammaBold(gammaBold_s);
	IntegerVector kVector(kVector_s);
	NumericVector gammaOne(gammaOne_s);
	NumericVector gammaTwo(gammaTwo_s);

	int endPos = 0;
	int numK = kVector.size();

	for(int i =0; i < numK; i++) {
		for(int j = endPos; j < (kVector(i) + endPos); j++) {
			gammaBold(0,j)=gammaOne(i);
		}
		for(int j = (kVector(i) + endPos); j < ((2*kVector(i)) + endPos); j++) {
			gammaBold(0,j)=gammaTwo(i);
		}
		endPos += 2 * kVector(i);
	}

	return R_NilValue;

	END_RCPP

}

SEXP updateTBATSGMatrix(SEXP g_s, SEXP gammaBold_s, SEXP alpha_s, SEXP beta_s) {
	BEGIN_RCPP

	int adjBeta = 0;

	double *gammaVector;

	NumericMatrix g_r(g_s);

	//Rprintf("one\n");
	g_r(0,0) = REAL(alpha_s)[0];
	//Rprintf("two\n");
	if(!Rf_isNull(beta_s)) {
		//Rprintf("three\n");
		g_r(1,0) = REAL(beta_s)[0];
		adjBeta = 1;
	}
	//Rprintf("four\n");
	if(!Rf_isNull(gammaBold_s)) {
		NumericMatrix gammaBold_r(gammaBold_s);
		arma::mat gammaBold(gammaBold_r.begin(), gammaBold_r.nrow(), gammaBold_r.ncol(), false);
		arma::mat g(g_r.begin(), g_r.nrow(), g_r.ncol(), false);
		g.submat((adjBeta+1), 0,(adjBeta+gammaBold.n_cols), 0) = trans(gammaBold);
	}
	//Rprintf("five\n");
	return R_NilValue;

	END_RCPP
}

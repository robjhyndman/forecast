#include "calcBATS.h"

using namespace Rcpp ;

SEXP calcTBATSFaster(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es, SEXP xNought_s) {
	BEGIN_RCPP


	NumericMatrix yr(ys);
	NumericMatrix yHatr(yHats);
	NumericMatrix wTransposer(wTransposes);
	NumericMatrix Fr(Fs);
	NumericMatrix xr(xs);
	NumericMatrix gr(gs);
	NumericMatrix er(es);
	NumericMatrix xNought_r(xNought_s);


	arma::mat y(yr.begin(), yr.nrow(), yr.ncol(), false);
	arma::mat yHat(yHatr.begin(), yHatr.nrow(), yHatr.ncol(), false);
	arma::mat wTranspose(wTransposer.begin(), wTransposer.nrow(), wTransposer.ncol(), false);
	arma::mat F(Fr.begin(), Fr.nrow(), Fr.ncol(), false);
	arma::mat x(xr.begin(), xr.nrow(), xr.ncol(), false);
	arma::mat g(gr.begin(), gr.nrow(), gr.ncol(), false);
	arma::mat e(er.begin(), er.nrow(), er.ncol(), false);
	arma::mat xNought(xNought_r.begin(), xNought_r.nrow(), xNought_r.ncol(), false);





	yHat.col(0) = wTranspose * xNought;
	e(0,0) = y(0, 0) - yHat(0, 0);
	x.col(0) = F * xNought + g * e(0,0);

	for(int t = 1; t < yr.ncol(); t++) {
		yHat.col(t) = wTranspose * x.col((t-1));
		e(0,t) = y(0, t) - yHat(0, t);
		x.col(t) = F * x.col((t-1)) + g * e(0,t);
	}


	return R_NilValue;

	END_RCPP
}



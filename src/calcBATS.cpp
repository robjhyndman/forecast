#include "calcBATS.h"

using namespace Rcpp ;

SEXP calcBATS(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es ){
	BEGIN_RCPP
	

	NumericMatrix yr(ys);
	NumericMatrix yHatr(yHats);
	NumericMatrix wTransposer(wTransposes);
	NumericMatrix Fr(Fs);
	NumericMatrix xr(xs);
	NumericMatrix gr(gs);
	NumericMatrix er(es);

	int t;

	arma::mat y(yr.begin(), yr.nrow(), yr.ncol(), false);
	arma::mat yHat(yHatr.begin(), yHatr.nrow(), yHatr.ncol(), false);
	arma::mat wTranspose(wTransposer.begin(), wTransposer.nrow(), wTransposer.ncol(), false);
	arma::mat F(Fr.begin(), Fr.nrow(), Fr.ncol(), false);
	arma::mat x(xr.begin(), xr.nrow(), xr.ncol(), false);
	arma::mat g(gr.begin(), gr.nrow(), gr.ncol(), false);
	arma::mat e(er.begin(), er.nrow(), er.ncol(), false);


	for(t = 1; t < yr.ncol(); t++) {
		yHat.col(t) = wTranspose * x.col((t-1));
		e(0,t) = y(0, t) - yHat(0, t);
		x.col(t) = F * x.col((t-1)) + g * e(0,t);
	}

	return List::create(
			Named("y.hat") = yHat,
			Named("e") = e,
			Named("x") = x
	);

	END_RCPP
}

SEXP calcBATSFaster(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es, SEXP xNought_s, SEXP sPeriods_s, SEXP betaV, SEXP tau_s, SEXP p_s, SEXP q_s ) {
	BEGIN_RCPP


	NumericMatrix yr(ys);
	NumericMatrix yHatr(yHats);
	NumericMatrix wTransposer(wTransposes);
	NumericMatrix Fr(Fs);
	NumericMatrix xr(xs);
	NumericMatrix gr(gs);
	NumericMatrix er(es);
	NumericMatrix xNought_r(xNought_s);
	//IntegerVector sPeriodsR(sPeriods);

	int adjBeta, previousS, lengthArma, *tau, *p, *q, *sPeriods;
	R_len_t lengthSeasonal;

	tau = &INTEGER(tau_s)[0];
	p = &INTEGER(p_s)[0];
	q = &INTEGER(q_s)[0];
	lengthArma = *p + *q;
	if(!Rf_isNull(sPeriods_s)) {
		sPeriods = INTEGER(sPeriods_s);
		lengthSeasonal = LENGTH(sPeriods_s);
	}

	if(!Rf_isNull(betaV))  {
		adjBeta = 1;
	} else {
		adjBeta = 0;
	}

	arma::mat y(yr.begin(), yr.nrow(), yr.ncol(), false);
	arma::mat yHat(yHatr.begin(), yHatr.nrow(), yHatr.ncol(), false);
	arma::mat wTranspose(wTransposer.begin(), wTransposer.nrow(), wTransposer.ncol(), false);
	arma::mat F(Fr.begin(), Fr.nrow(), Fr.ncol(), false);
	arma::mat x(xr.begin(), xr.nrow(), xr.ncol(), false);
	arma::mat g(gr.begin(), gr.nrow(), gr.ncol(), false);
	arma::mat e(er.begin(), er.nrow(), er.ncol(), false);
	arma::mat xNought(xNought_r.begin(), xNought_r.nrow(), xNought_r.ncol(), false);



	if(!Rf_isNull(sPeriods_s)) {
		//One
		//Rprintf("one-1\n");
		yHat.col(0) = wTranspose.cols(0, adjBeta) * xNought.rows(0, adjBeta);
		//Rprintf("one-2\n");
		previousS = 0;
		for(R_len_t i = 0; i < lengthSeasonal; i++) {
			//Rprintf("one-3\n");
			yHat(0,0) = yHat(0,0) +  xNought( (previousS + sPeriods[i] + adjBeta), 0);
			previousS += sPeriods[i];
		}
		if(lengthArma > 0) {
			//Rprintf("bg-1");
			yHat.col(0) = yHat(0,0) + wTranspose.cols((*tau + adjBeta + 1), (xNought.n_rows-1)) * xNought.rows((*tau + adjBeta + 1), (xNought.n_rows-1));
		}
		//Two
		e(0,0) = y(0, 0) - yHat(0, 0);
		//Three
		//Rprintf("three-5\n");
		x.submat(0, 0, adjBeta, 0) = F.submat(0,0,adjBeta,adjBeta) * xNought.rows(0,adjBeta);
		if(lengthArma > 0) {
			//Rprintf("bg-2");
			x.submat(0, 0, adjBeta, 0) += F.submat(0,(adjBeta+ *tau + 1),adjBeta,(F.n_cols - 1)) * xNought.rows((adjBeta+ *tau + 1),(F.n_cols - 1));
		}
		previousS = 0;
		for(R_len_t i = 0; i < lengthSeasonal; i++) {
			//Rprintf("three-7\n");
			x((adjBeta+previousS+1),0) = xNought((adjBeta+previousS+sPeriods[i]),0);
			if(lengthArma > 0) {
				//Rprintf("bg-3");
				x.submat((adjBeta+previousS+1),0, (adjBeta+previousS+1),0) = x.submat((adjBeta+previousS+1),0, (adjBeta+previousS+1),0) + F.submat((adjBeta + previousS + 1), (adjBeta+*tau+1), (adjBeta + previousS + 1), (F.n_cols-1)) * xNought.rows((adjBeta + *tau +1), (F.n_cols-1));
			}
			//Rprintf("three-9\n");
			x.submat((adjBeta + previousS + 2), 0, (adjBeta + previousS + sPeriods[i]), 0) = xNought.rows((adjBeta + previousS + 1), (adjBeta + previousS + sPeriods[i] -1));
			previousS += sPeriods[i];
		}
		if(*p > 0) {
			//Rprintf("bg-4");
			x.submat((adjBeta+ *tau + 1),0,(adjBeta+ *tau + 1),0) = F.submat((adjBeta + *tau +1), (adjBeta + *tau +1), (adjBeta + *tau + 1), (F.n_cols-1)) * xNought.rows((adjBeta+*tau+1), (F.n_cols-1));
			//Rprintf("bg-5"); ////error is HERE!!!
			if(*p > 1) {
				x.submat((adjBeta + *tau + 2),0,(adjBeta + *tau + *p),0) = xNought.rows((adjBeta + *tau + 1),(adjBeta + *tau + *p-1));
			}
		}
		if(*q > 0) {
			//Rprintf("three-12\n");
			x((adjBeta+ *tau + *p + 1),0) = 0;
			if(*q > 1) {
				//Rprintf("three-13\n");
				x.submat((adjBeta+ *tau + *p + 2), 0, (adjBeta + *tau + *p + *q) , 0) = xNought.rows((adjBeta + *tau + *p + 1),(adjBeta + *tau + *p + *q - 1));
			}
		}
		///Temporary fix!
		//x.col(0) += g * e(0,0);
		//End

		///////////
		x(0,0) += g(0,0) * e(0,0);
		if(adjBeta == 1) {
			x(1,0) += g(1,0) * e(0,0);
		}
		previousS = 0;
		for(R_len_t i = 0; i < lengthSeasonal; i++) {
			x((adjBeta+previousS+1),0) += g((adjBeta+previousS+1),0) * e(0,0);
			previousS += sPeriods[i];
		}
		if(*p > 0) {
			x((adjBeta + *tau + 1),0) += e(0,0);
			if(*q > 0) {
				x((adjBeta + *tau + *p + 1),0) += e(0,0);
			}
		} else if(*q > 0) {
			x((adjBeta + *tau + 1),0) += e(0,0);
		}
		/////////////////////////////////

		for(int t = 1; t < yr.ncol(); t++) {
			//Rprintf("point-x\n");
			//One
			yHat.col(t) = wTranspose.cols(0, adjBeta) * x.submat(0, (t-1), adjBeta, (t-1));
			previousS = 0;
			for(R_len_t i = 0; i < lengthSeasonal; i++) {
				//mod here
				//Rprintf("point-xx\n");
				yHat(0,t) += x((previousS + sPeriods[i] + adjBeta), (t-1));
				previousS += sPeriods[i];
			}
			if(lengthArma > 0) {
				//Rprintf("bg-6");
				yHat.col(t) += wTranspose.cols((*tau + adjBeta + 1), (xNought.n_rows-1)) * x.submat((*tau + adjBeta + 1), (t-1), (x.n_rows-1), (t-1));
			}
			//Two
			//Rprintf("point-x4\n");
			e(0,t) = y(0, t) - yHat(0, t);
			//Three
			//Rprintf("point-x5\n");
			x.submat(0, t, adjBeta, t) = F.submat(0,0,adjBeta,adjBeta) * x.submat(0, (t-1), adjBeta, (t-1));
			if(lengthArma > 0) {
				//Rprintf("bg-7");
				x.submat(0, t, adjBeta, t) += F.submat(0,(adjBeta+ *tau + 1),adjBeta,(F.n_cols - 1)) * x.submat((adjBeta+ *tau + 1), (t-1), (F.n_cols - 1), (t-1));
			}
			previousS = 0;
			for(R_len_t i = 0; i < lengthSeasonal; i++) {
				//Rprintf("point-x7\n");
				x((adjBeta+previousS+1),t) = x((adjBeta+previousS+sPeriods[i]),(t-1));
				if(lengthArma > 0) {
					//Rprintf("bg-8");
					x.submat((adjBeta+previousS+1),t, (adjBeta+previousS+1),t) += F.submat((adjBeta + previousS + 1), (adjBeta+*tau+1), (adjBeta + previousS + 1), (F.n_cols-1)) * x.submat((adjBeta + *tau +1), (t-1), (F.n_cols-1), (t-1));
				}
				//Rprintf("Three-L-9\n");
				x.submat((adjBeta + previousS + 2), t, (adjBeta + previousS + sPeriods[i]), t) = x.submat((adjBeta + previousS + 1), (t-1), (adjBeta + previousS + sPeriods[i] -1), (t-1));
				previousS += sPeriods[i];
			}
/*
			if(lengthArma > 0) {
				x.submat((adjBeta+ *tau + 1),t, (x.n_rows-1),t) = F.submat((adjBeta+ *tau + 1), (adjBeta+ *tau + 1), (F.n_rows - 1), (F.n_rows - 1)) * x.submat((adjBeta+ *tau + 1),(t-1), (x.n_rows-1),(t-1));
			}
*/
			if(*p > 0) {
				//Rprintf("bg-9");

				x.submat((adjBeta+ *tau + 1),t, (adjBeta+ *tau + 1),t) = F.submat((adjBeta + *tau +1), (adjBeta + *tau +1), (adjBeta + *tau + 1), (F.n_cols-1)) * x.submat((adjBeta+*tau+1), (t-1), (F.n_cols-1), (t-1));
				if(*p > 1) {

					x.submat((adjBeta + *tau + 2),t,(adjBeta + *tau + *p),t) = x.submat((adjBeta + *tau + 1), (t-1), (adjBeta + *tau + *p -1), (t-1));
				}
			}
			if(*q > 0) {
				x((adjBeta+ *tau + *p + 1),t) = 0;
				if(*q > 1) {
					x.submat((adjBeta+ *tau + *p + 2), t, (adjBeta + *tau + *p + *q) , t) = x.submat((adjBeta + *tau + *p + 1), (t-1), (adjBeta + *tau + *p + *q - 1), (t-1));
				}
			}
			///Temporary fix!
			//x.col(t) += g * e(0,t);
			//End
			///////////
			x(0,t) += g(0,0) * e(0,t);
			if(adjBeta == 1) {
				x(1,t) += g(1,0) * e(0,t);
			}
			previousS = 0;
			for(R_len_t i = 0; i < lengthSeasonal; i++) {
				x((adjBeta+previousS+1),t) += g((adjBeta+previousS+1),0) * e(0,t);
				previousS += sPeriods[i];
			}
			if(*p > 0) {
				x((adjBeta + *tau + 1),t) += e(0,t);
				if(*q > 0) {
					x((adjBeta + *tau + *p + 1),t) += e(0,t);
				}
			} else if(*q > 0) {
				x((adjBeta + *tau + 1),t) += e(0,t);
			}
			/////////////////////////////////


		}

	} else {
		yHat.col(0) = wTranspose * xNought;
		e(0,0) = y(0, 0) - yHat(0, 0);
		x.col(0) = F * xNought + g * e(0,0);

		for(int t = 1; t < yr.ncol(); t++) {
			yHat.col(t) = wTranspose * x.col((t-1));
			e(0,t) = y(0, t) - yHat(0, t);
			x.col(t) = F * x.col((t-1)) + g * e(0,t);
		}
	}

	return R_NilValue;

	END_RCPP
}

SEXP calcWTilda(SEXP wTildaTransposes, SEXP Ds) {
	BEGIN_RCPP

	NumericMatrix wTildaTransposer(wTildaTransposes);
	NumericMatrix Dr(Ds);

	int t;

	arma::mat wTildaTranspose(wTildaTransposer.begin(), wTildaTransposer.nrow(), wTildaTransposer.ncol(), false);
	arma::mat D(Dr.begin(), Dr.nrow(), Dr.ncol(), false);

	for(t = 1; t < wTildaTransposer.nrow(); t++) {
		wTildaTranspose.row(t) = wTildaTranspose.row((t-1)) * D;
	}

	return wTildaTransposer;

	END_RCPP
}



#include "calcBATS.h"

using namespace Rcpp ;

SEXP makeBATSWMatrix(SEXP smallPhi_s, SEXP sPeriods_s, SEXP arCoefs_s, SEXP maCoefs_s) {
	BEGIN_RCPP
	double *smallPhi, *arCoefs, *maCoefs;
	int *seasonalPeriods;
	int adjustPhi = 0;
	R_len_t numCols = 1, numSeasonal = 0, p = 0, q = 0;
	int lengthSeasonal = 0;

	if(!Rf_isNull(smallPhi_s)) {
		smallPhi = REAL(smallPhi_s);
		adjustPhi = 1;
		numCols = numCols + 1;
	}
	if(!Rf_isNull(sPeriods_s)) {
		seasonalPeriods = INTEGER(sPeriods_s);
		numSeasonal = LENGTH(sPeriods_s);
		for(R_len_t s = 0; s < numSeasonal; s++) {
			lengthSeasonal = lengthSeasonal + seasonalPeriods[s];
		}
		numCols = numCols + lengthSeasonal;
	} else {
		lengthSeasonal = 0;
	}
	if(!Rf_isNull(arCoefs_s)) {
			arCoefs = REAL(arCoefs_s);
			p = LENGTH(arCoefs_s);
			numCols = numCols + p;
	}
	if(!Rf_isNull(maCoefs_s)) {
			maCoefs = REAL(maCoefs_s);
			q = LENGTH(maCoefs_s);
			numCols = numCols + q;
	}

	NumericMatrix wTranspose_r(1, numCols);
	arma::mat wTranspose(wTranspose_r.begin(), wTranspose_r.nrow(), wTranspose_r.ncol(), false);

	if(!Rf_isNull(sPeriods_s)) {
			wTranspose.zeros();

			int position = adjustPhi;

			for(R_len_t s = 0; s < numSeasonal; s++) {
				position = position + seasonalPeriods[s];
				wTranspose(0,position) = 1;
			}

	}


	wTranspose(0,0) = 1;

	if(adjustPhi == 1) {
		wTranspose(0,1) = *smallPhi;
	}


	if(!Rf_isNull(arCoefs_s)) {
		for(R_len_t i = 1; i <= p; i++) {
			wTranspose(0,(adjustPhi + lengthSeasonal +i)) = arCoefs[(i-1)];
		}
	}

	if(!Rf_isNull(maCoefs_s)) {
			for(R_len_t i = 1; i <= q; i++) {
				wTranspose(0,(adjustPhi + lengthSeasonal + p + i)) = maCoefs[(i-1)];
			}
	}
	arma::mat w = arma::trans(wTranspose);

	smallPhi = 0;
	arCoefs = 0;
	maCoefs = 0;
	seasonalPeriods = 0;

	return List::create(
			Named("w") = w,
			Named("w.transpose") = wTranspose
			);

	END_RCPP
}


SEXP makeBATSGMatrix(SEXP alpha_s, SEXP beta_s, SEXP gammaVector_s, SEXP seasonalPeriods_s, SEXP p_s, SEXP q_s) {
	BEGIN_RCPP

	double *gammaVector;
	int *seasonalPeriods, *p, *q;
	int numCols, gammaLength = 0;
	int adjustBeta = 0;

	p = INTEGER(p_s);
	q = INTEGER(q_s);

	numCols = 1 + *p + *q;

	if(!Rf_isNull(beta_s)) {
		numCols = numCols + 1;
		adjustBeta = 1;
	}

	//Find the length of the gamma/seasonal bit
	if((!Rf_isNull(gammaVector_s))&&(!Rf_isNull(seasonalPeriods_s))) {
		gammaVector = REAL(gammaVector_s);
		seasonalPeriods = INTEGER(seasonalPeriods_s);
		for(R_len_t i = 0; i < LENGTH(seasonalPeriods_s); i++) {
			gammaLength = gammaLength + seasonalPeriods[i];
		}
		numCols = numCols + gammaLength;
	} else {
		gammaLength = 0;
	}

	NumericMatrix gTranspose_r(1, numCols);
	arma::mat gTranspose(gTranspose_r.begin(), gTranspose_r.nrow(), gTranspose_r.ncol(), false);
	gTranspose.zeros();

	gTranspose(0,0) = REAL(alpha_s)[0];
	if(!Rf_isNull(beta_s)) {
		gTranspose(0,1) = REAL(beta_s)[0];
	}



	//Copy the gamma/seasonal bits
	if((!Rf_isNull(gammaVector_s))&&(!Rf_isNull(seasonalPeriods_s))) {
		int position = adjustBeta + 1;

		gTranspose(0, position) = gammaVector[0];
		if(LENGTH(gammaVector_s) > 1) {
			for(R_len_t s = 0; s < (LENGTH(seasonalPeriods_s)-1); s++) {
				position = position + seasonalPeriods[s];
				gTranspose(0, position) = gammaVector[(s+1)];
			}
		}
	}

	if(*p != 0) {
		gTranspose(0, (adjustBeta+gammaLength+1)) = 1;
	}
	if(*q != 0) {
		gTranspose(0, (adjustBeta+gammaLength+ *p +1)) = 1;
	}
	arma::mat g(arma::trans(gTranspose));

	seasonalPeriods = 0;
	p = 0;
	q = 0;
	gammaVector = 0;

	if((!Rf_isNull(gammaVector_s))&&(!Rf_isNull(seasonalPeriods_s))) {
		arma::mat gammaBold = gTranspose.cols((1+adjustBeta), (adjustBeta+gammaLength));
		return List::create(
				Named("g") = g,
				Named("g.transpose") = gTranspose,
				Named("gamma.bold.matrix") = gammaBold
			);

	} else {
		return List::create(
				Named("g") = g,
				Named("g.transpose") = gTranspose,
				Named("gamma.bold.matrix") = R_NilValue
			);
	}

	END_RCPP
}

/*
SEXP makeFMatrix(SEXP alpha_s, SEXP beta_s, SEXP smallPhi_s, SEXP seasonalPeriods_s, SEXP gammaBoldMatrix_s, SEXP arCoefs_s, SEXP maCoefs_s) {
	BEGIN_RCPP

	NumericMatrix alpha_r(alpha_s);
	if(!Rf_isNull(beta_s)) {
		NumericMatrix beta_r(beta_s);
		bool indBeta = true;
	} else {
		bool indBeta = false;
	}
	if(!Rf_isNull(smallPhi_s)) {
		NumericMatrix smallPhi_r(smallPhi_s);
		bool indSmallPhi = true;
	} else {
		bool indSmallPhi = false;
	}
	if(!Rf_isNull(seasonalPeriods_s)) {
		NumericMatrix seasonalPeriods_r(seasonalPeriods_s);
		bool indSeasonalPeriods = true;
	} else {
		bool indSeasonalPeriods = false;
	}
	if(!Rf_isNull(gammaBoldMatrix_s)) {
		NumericMatrix gammaBoldMatrix_r(gammaBoldMatrix_s);
		bool indGammaBoldMatrix = true;
	} else {
		bool indGammaBoldMatrix = false;
	}
	if(!Rf_isNull(arCoefs_s)) {
		NumericMatrix arCoefs_r(arCoefs_s);
		bool indArCoefs = true;
	} else {
		bool indArCoefs = false;
	}
	if(!Rf_isNull(maCoefs_s)) {
		NumericMatrix maCoefs_r(maCoefs_s);
		bool indMaCoefs = true;
	} else {
		bool indMaCoefs = false;
	}
	arma::mat

	END_RCPP
}

*/

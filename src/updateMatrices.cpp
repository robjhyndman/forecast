/*
 * updateMatrices.cpp
 *
 *  Created on: 03/11/2011
 *      Author: srazbash
 */

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
SEXP updateFMatrix(SEXP F_s, SEXP smallPhi_s, SEXP alpha_s, SEXP beta_s, SEXP gammaBold_s, SEXP ar_s, SEXP ma_s, SEXP tau_s) {
	NumericMatrix F_r(F_s);
	arma::mat F(F_r.begin(), F_r.nrow(), F_r.ncol(), false);

	double *beta, *alpha = &REAL(alpha_s)[0];
	int *tau, p, q, betaAdjust;
	int zero = 0;

	if(!Rf_isNull(gammaBold_s)) {
		tau = &INTEGER(tau_s)[0];
	} else {
		tau = &zero;
	}

	if(!Rf_isNull(beta_s)) {
		beta = &REAL(beta_s)[0];
		double *smallPhi = &REAL(smallPhi_s)[0];
		F(0,1) = *smallPhi;
		F(1,1) = *smallPhi;
		betaAdjust = 1;
	} else {
		betaAdjust = 0;
	}

	if(!Rf_isNull(ar_s)) {
		//Rprintf("before arma::mat ar\n");
		NumericMatrix ar_r(ar_s);
		arma::mat ar(ar_r.begin(), ar_r.nrow(), ar_r.ncol(), false);
		//Rprintf("after arma::mat ar\n");
		p = ar.n_cols;
		//Rprintf("line-a-before\n");
		F.submat(0,(betaAdjust+ *tau+1),0,(betaAdjust+ *tau+p)) = *alpha * ar;
		//Rprintf("line-a-after\n");
		if(betaAdjust == 1) {
			//Rprintf("line-b-before\n");
			F.submat(1,(betaAdjust+ *tau+1),1,(betaAdjust+ *tau+p)) = *beta * ar;
			//Rprintf("line-b-after\n");
		}
		if(*tau > 0) {
			//Rprintf("la\n");
			NumericMatrix gammaBold_r(gammaBold_s);
			//Rprintf("la-2\n");
			arma::mat gammaBold(gammaBold_r.begin(), gammaBold_r.nrow(), gammaBold_r.ncol(), false);
			//Rprintf("la-3\n");
			//arma::mat gammaBold = as<arma::mat>(gammaBold_s);
			arma::mat B = trans(gammaBold) * ar;
			//Rprintf("line-c-before\n");
			F.submat((1+betaAdjust),(betaAdjust+ *tau+1), (betaAdjust+ *tau), (betaAdjust+ *tau+p)) = B;
			//Rprintf("line-c-after\n");
		}
		//Rprintf("line-d-before\n");
		F.submat((betaAdjust+ *tau+1),(betaAdjust+ *tau+1),(betaAdjust+ *tau+1),(betaAdjust+ *tau+p)) = ar;
		//Rprintf("line-d-after\n");
	} else {
		p = 0;
	}

	if(!Rf_isNull(ma_s)) {
		NumericMatrix ma_r(ma_s);
		arma::mat ma(ma_r.begin(), ma_r.nrow(), ma_r.ncol(), false);
		q = ma.n_cols;
		//Rprintf("one-before\n");
		F.submat(0,(betaAdjust+ *tau+p+1),0,(betaAdjust+ *tau+p+q)) = *alpha * ma;
		//Rprintf("one-after\n");
		if(betaAdjust == 1) {
			//Rprintf("two-before\n");
			F.submat(1,(betaAdjust+ *tau+p+1),1,(betaAdjust+ *tau+p+q)) = *beta * ma;
			///Rprintf("two-after\n");
		}
		if(*tau > 0) {
			//arma::mat gammaBold = as<arma::mat>(gammaBold_s);

			NumericMatrix gammaBold_r(gammaBold_s);
			arma::mat gammaBold(gammaBold_r.begin(), gammaBold_r.nrow(), gammaBold_r.ncol(), false);

			arma::mat C = trans(gammaBold) * ma;
			//Rprintf("three-before\n");
			F.submat((1+betaAdjust),(betaAdjust+ *tau+p+1), (betaAdjust+ *tau), (betaAdjust+ *tau+p+q)) = C;
			//Rprintf("three-after\n");
		}
		if(!Rf_isNull(ar_s)) {
			//Rprintf("four-before\n");
			F.submat((betaAdjust+ *tau+1), (betaAdjust+ *tau+p+1), (betaAdjust+ *tau+1), (betaAdjust+ *tau+p+q)) = ma;
			//Rprintf("four-after\n");
		}
	} else {
		q = 0;
	}

	return R_NilValue;
}

// [[Rcpp::export]]
void updateWtransposeMatrix(NumericMatrix &wTranspose,
                            const Nullable<double> &smallPhi,
                            int tau,
                            const Nullable<NumericVector> &arCoefs,
                            const Nullable<NumericVector> &maCoefs,
                            int p,
                            int q) {
  int adjBeta = 0;
  if (smallPhi.isNotNull()) {
    adjBeta = 1;
    wTranspose(0, 1) = as<double>(smallPhi);
  }

  if (p > 0) {
    const NumericVector ar = as<NumericVector>(arCoefs);
    for (int i = 1; i <= p; i++) {
      wTranspose(0, adjBeta + tau + i) = ar[i - 1];
    }
    if (q > 0) {
      const NumericVector ma = as<NumericVector>(maCoefs);
      for (int i = 1; i <= q; i++) {
        wTranspose(0, adjBeta + tau + p + i) = ma[i - 1];
      }
    }
  } else if (q > 0) {
    const NumericVector ma = as<NumericVector>(maCoefs);
    for (int i = 1; i <= q; i++) {
      wTranspose(0, adjBeta + tau + i) = ma[i - 1];
    }
  }
}

// [[Rcpp::export]]
void updateGMatrix(NumericMatrix &g,
                   const Nullable<NumericMatrix> &gammaBold,
                   double alpha,
                   const Nullable<double> &beta,
                   const Nullable<NumericVector> &gammaVector,
                   const Nullable<IntegerVector> &seasonalPeriods) {
  int adjBeta = 0;
  g(0, 0) = alpha;
  if (beta.isNotNull()) {
    g(1, 0) = as<double>(beta);
    adjBeta = 1;
  }

  if (gammaVector.isNotNull() && seasonalPeriods.isNotNull()) {
    NumericMatrix gammaBoldMat = as<NumericMatrix>(gammaBold);
    const NumericVector gamma = as<NumericVector>(gammaVector);
    const IntegerVector periods = as<IntegerVector>(seasonalPeriods);

    int position = adjBeta + 1;
    int bPos = 0;

    gammaBoldMat(0, bPos) = gamma[0];
    g(position, 0) = gamma[0];

    if (gamma.size() > 1) {
      for (int s = 0; s < periods.size() - 1; s++) {
        position += periods[s];
        bPos += periods[s];
        g(position, 0) = gamma[s + 1];
      }
    }
  }
}

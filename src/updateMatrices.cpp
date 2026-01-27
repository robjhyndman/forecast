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
void updateFMatrix(arma::mat &F,
                   const Nullable<double> &smallPhi,
                   double alpha,
                   const Nullable<double> &beta,
                   const Nullable<arma::mat> &gammaBold,
                   const Nullable<arma::mat> &ar,
                   const Nullable<arma::mat> &ma,
                   int tau) {
  const bool hasBeta = beta.isNotNull();
  const bool hasGamma = gammaBold.isNotNull();
  const int betaAdjust = hasBeta ? 1 : 0;
  tau = hasGamma ? tau : 0;

  int p = 0;

  if (hasBeta) {
    const double phi = as<double>(smallPhi);
    F(0, 1) = phi;
    F(1, 1) = phi;
  }

  if (ar.isNotNull()) {
    const arma::mat arMat = as<arma::mat>(ar);
    p = arMat.n_cols;
    F.submat(0, betaAdjust + tau + 1, 0, betaAdjust + tau + p) = alpha * arMat;

    if (hasBeta) {
      const double b = as<double>(beta);
      F.submat(1, betaAdjust + tau + 1, 1, betaAdjust + tau + p) = b * arMat;
    }

    if (tau > 0 && hasGamma) {
      const arma::mat gammaMat = as<arma::mat>(gammaBold);
      const arma::mat B = gammaMat.t() * arMat;
      F.submat(1 + betaAdjust, betaAdjust + tau + 1, betaAdjust + tau, betaAdjust + tau + p) = B;
    }

    F.submat(betaAdjust + tau + 1, betaAdjust + tau + 1, betaAdjust + tau + 1, betaAdjust + tau + p) = arMat;
  }

  if (ma.isNotNull()) {
    const arma::mat maMat = as<arma::mat>(ma);
    const arma::uword q = maMat.n_cols;
    F.submat(0, betaAdjust + tau + p + 1, 0, betaAdjust + tau + p + q) = alpha * maMat;

    if (hasBeta) {
      const double b = as<double>(beta);
      F.submat(1, betaAdjust + tau + p + 1, 1, betaAdjust + tau + p + q) = b * maMat;
    }

    if (tau > 0 && hasGamma) {
      const arma::mat gammaMat = as<arma::mat>(gammaBold);
      const arma::mat C = gammaMat.t() * maMat;
      F.submat(1 + betaAdjust, betaAdjust + tau + p + 1, betaAdjust + tau, betaAdjust + tau + p + q) = C;
    }

    if (ar.isNotNull()) {
      F.submat(betaAdjust + tau + 1, betaAdjust + tau + p + 1, betaAdjust + tau + 1, betaAdjust + tau + p + q) = maMat;
    }
  }
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

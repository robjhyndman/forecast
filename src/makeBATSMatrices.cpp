#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
List makeBATSWMatrix(const Nullable<double> &smallPhi,
                     const Nullable<IntegerVector> &sPeriods,
                     const Nullable<NumericVector> &arCoefs,
                     const Nullable<NumericVector> &maCoefs) {
  int adjustPhi = 0;
  int numCols = 1;
  int lengthSeasonal = 0;
  int p = 0;
  int q = 0;

  if (smallPhi.isNotNull()) {
    adjustPhi = 1;
    numCols++;
  }

  if (sPeriods.isNotNull()) {
    const IntegerVector periods = as<IntegerVector>(sPeriods);
    lengthSeasonal = sum(periods);
    numCols += lengthSeasonal;
  }

  if (arCoefs.isNotNull()) {
    p = as<NumericVector>(arCoefs).size();
    numCols += p;
  }

  if (maCoefs.isNotNull()) {
    q = as<NumericVector>(maCoefs).size();
    numCols += q;
  }

  arma::mat wTranspose(1, numCols, arma::fill::zeros);

  if (sPeriods.isNotNull()) {
    const IntegerVector periods = as<IntegerVector>(sPeriods);
    int position = adjustPhi;
    for (int s = 0; s < periods.size(); s++) {
      position += periods[s];
      wTranspose(0, position) = 1;
    }
  }

  wTranspose(0, 0) = 1;

  if (smallPhi.isNotNull()) {
    wTranspose(0, 1) = as<double>(smallPhi);
  }

  if (arCoefs.isNotNull()) {
    const NumericVector ar = as<NumericVector>(arCoefs);
    for (int i = 0; i < p; i++) {
      wTranspose(0, adjustPhi + lengthSeasonal + i + 1) = ar[i];
    }
  }

  if (maCoefs.isNotNull()) {
    const NumericVector ma = as<NumericVector>(maCoefs);
    for (int i = 0; i < q; i++) {
      wTranspose(0, adjustPhi + lengthSeasonal + p + i + 1) = ma[i];
    }
  }

  return List::create(
    Named("w") = wTranspose.t(),
    Named("w.transpose") = wTranspose
  );
}

// [[Rcpp::export]]
List makeBATSGMatrix(double alpha,
                     const Nullable<double> &beta,
                     const Nullable<NumericVector> &gammaVector,
                     const Nullable<IntegerVector> &seasonalPeriods,
                     int p,
                     int q) {
  int numCols = 1 + p + q;
  int adjustBeta = 0;
  int gammaLength = 0;

  if (beta.isNotNull()) {
    numCols++;
    adjustBeta = 1;
  }

  // Find the length of the gamma/seasonal bit
  if (gammaVector.isNotNull() && seasonalPeriods.isNotNull()) {
    const IntegerVector periods = as<IntegerVector>(seasonalPeriods);
    gammaLength = sum(periods);
    numCols += gammaLength;
  }

  arma::mat gTranspose(1, numCols, arma::fill::zeros);

  gTranspose(0, 0) = alpha;

  if (beta.isNotNull()) {
    gTranspose(0, 1) = as<double>(beta);
  }

  // Copy the gamma/seasonal bits
  if (gammaVector.isNotNull() && seasonalPeriods.isNotNull()) {
    const NumericVector gamma = as<NumericVector>(gammaVector);
    const IntegerVector periods = as<IntegerVector>(seasonalPeriods);

    int position = adjustBeta + 1;
    gTranspose(0, position) = gamma[0];

    if (gamma.size() > 1) {
      for (int s = 0; s < periods.size() - 1; s++) {
        position += periods[s];
        gTranspose(0, position) = gamma[s + 1];
      }
    }
  }

  if (p != 0) {
    gTranspose(0, adjustBeta + gammaLength + 1) = 1;
  }

  if (q != 0) {
    gTranspose(0, adjustBeta + gammaLength + p + 1) = 1;
  }

  const arma::mat g = gTranspose.t();

  if (gammaVector.isNotNull() && seasonalPeriods.isNotNull()) {
    const arma::mat gammaBold = gTranspose.cols(1 + adjustBeta, adjustBeta + gammaLength);
    return List::create(
      Named("g") = g,
      Named("g.transpose") = gTranspose,
      Named("gamma.bold.matrix") = gammaBold
    );
  }

  return List::create(
    Named("g") = g,
    Named("g.transpose") = gTranspose,
    Named("gamma.bold.matrix") = R_NilValue
  );
}

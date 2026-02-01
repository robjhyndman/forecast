#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
List makeTBATSWMatrix(const Nullable<double> &smallPhi,
                      const Nullable<IntegerVector> &kVector,
                      const Nullable<NumericVector> &arCoefs,
                      const Nullable<NumericVector> &maCoefs,
                      const Nullable<int> &tau) {
  int adjustPhi = 0;
  int numCols = 1;
  int tauVal = 0;
  int p = 0;
  int q = 0;

  if (smallPhi.isNotNull()) {
    adjustPhi = 1;
    numCols++;
  }

  if (kVector.isNotNull()) {
    tauVal = as<int>(tau);
    numCols += tauVal;
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
  wTranspose(0, 0) = 1.0;

  if (kVector.isNotNull()) {
    const IntegerVector kVec = as<IntegerVector>(kVector);
    int position = adjustPhi;
    for (int s = 0; s < kVec.size(); s++) {
      for (int j = position + 1; j <= position + kVec[s]; j++) {
        wTranspose(0, j) = 1.0;
      }
      position += 2 * kVec[s];
    }
  }

  if (adjustPhi == 1) {
    wTranspose(0, 1) = as<double>(smallPhi);
  }

  if (arCoefs.isNotNull()) {
    const NumericVector ar = as<NumericVector>(arCoefs);
    for (int i = 1; i <= p; i++) {
      wTranspose(0, adjustPhi + tauVal + i) = ar[i - 1];
    }
  }

  if (maCoefs.isNotNull()) {
    const NumericVector ma = as<NumericVector>(maCoefs);
    for (int i = 1; i <= q; i++) {
      wTranspose(0, adjustPhi + tauVal + p + i) = ma[i - 1];
    }
  }

  return List::create(
    Named("w") = wTranspose.t(),
    Named("w.transpose") = wTranspose
  );
}

// [[Rcpp::export]]
arma::mat makeCIMatrix(int k, double m) {
  const double pi = arma::datum::pi;
  arma::mat C(k, k, arma::fill::zeros);

  for (int j = 1; j <= k; j++) {
    C(j - 1, j - 1) = std::cos((2 * pi * j) / m);
  }
  return C;
}

// [[Rcpp::export]]
arma::mat makeSIMatrix(int k, double m) {
  const double pi = arma::datum::pi;
  arma::mat S(k, k, arma::fill::zeros);

  for (int j = 1; j <= k; j++) {
    double lambda = (2 * pi * j) / m;
    S(j - 1, j - 1) = std::sin(lambda);
  }
  return S;
}

// [[Rcpp::export]]
arma::mat makeAIMatrix(const arma::mat &C, const arma::mat &S, int k) {
  arma::mat A(k * 2, k * 2);

  A.submat(0, 0, k - 1, k - 1) = C;
  A.submat(0, k, k - 1, k * 2 - 1) = S;
  A.submat(k, 0, k * 2 - 1, k - 1) = -S;
  A.submat(k, k, k * 2 - 1, k * 2 - 1) = C;

  return A;
}

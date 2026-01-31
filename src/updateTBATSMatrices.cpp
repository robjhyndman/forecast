#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
void updateTBATSGammaBold(NumericMatrix &gammaBold,
                          const IntegerVector &kVector,
                          const NumericVector &gammaOne,
                          const NumericVector &gammaTwo) {
  int endPos = 0;

  for (int i = 0; i < kVector.size(); i++) {
    for (int j = endPos; j < kVector[i] + endPos; j++) {
      gammaBold(0, j) = gammaOne[i];
    }
    for (int j = kVector[i] + endPos; j < 2 * kVector[i] + endPos; j++) {
      gammaBold(0, j) = gammaTwo[i];
    }
    endPos += 2 * kVector[i];
  }
}

// [[Rcpp::export]]
void updateTBATSGMatrix(arma::mat &g,
                        const Nullable<arma::mat> &gammaBold,
                        double alpha,
                        const Nullable<double> &beta) {
  int adjBeta = 0;
  g(0, 0) = alpha;

  if (beta.isNotNull()) {
    g(1, 0) = as<double>(beta);
    adjBeta = 1;
  }

  if (gammaBold.isNotNull()) {
    arma::mat gb = as<arma::mat>(gammaBold);
    g.submat(adjBeta + 1, 0, adjBeta + gb.n_cols, 0) = gb.t();
  }
}

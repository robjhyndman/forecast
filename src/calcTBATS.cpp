#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
void calcTBATSFaster(const arma::mat &y,
                     arma::mat &yHat,
                     const arma::mat &wTranspose,
                     const arma::mat &F,
                     arma::mat &x,
                     const arma::mat &g,
                     arma::mat &e,
                     const arma::mat &xNought) {
  yHat.col(0) = wTranspose * xNought;
  e(0, 0) = y(0, 0) - yHat(0, 0);
  x.col(0) = F * xNought + g * e(0, 0);

  for (arma::uword t = 1; t < y.n_cols; t++) {
    yHat.col(t) = wTranspose * x.col(t - 1);
    e(0, t) = y(0, t) - yHat(0, t);
    x.col(t) = F * x.col(t - 1) + g * e(0, t);
  }
}

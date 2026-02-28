#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
List calcBATS(const arma::mat& y,
              arma::mat& yHat,
              const arma::mat& wTranspose,
              const arma::mat& F,
              arma::mat& x,
              const arma::mat& g,
              arma::mat& e) {
  for (arma::uword t = 1; t < y.n_cols; t++) {
    yHat.col(t) = wTranspose * x.col(t - 1);
    e(0, t) = y(0, t) - yHat(0, t);
    x.col(t) = F * x.col(t - 1) + g * e(0, t);
  }

  return List::create(
    Named("y.hat") = yHat,
    Named("e") = e,
    Named("x") = x
  );
}

// [[Rcpp::export]]
void calcBATSFaster(const arma::mat& y,
                    arma::mat& yHat,
                    const arma::mat& wTranspose,
                    const arma::mat& F,
                    arma::mat& x,
                    const arma::mat& g,
                    arma::mat& e,
                    const arma::mat& xNought,
                    const Nullable<IntegerVector>& sPeriods,
                    const Nullable<double>& beta,
                    int tau,
                    int p,
                    int q) {
  const int adjBeta = beta.isNotNull() ? 1 : 0;
  const int lengthArma = p + q;
  int previousS = 0;
  int lengthSeasonal = 0;
  IntegerVector sPeriodsVec;
  if (sPeriods.isNotNull()) {
    sPeriodsVec = as<IntegerVector>(sPeriods);
    lengthSeasonal = sPeriodsVec.size();
  }

  if (sPeriods.isNotNull()) {
    // One
    yHat.col(0) = wTranspose.cols(0, adjBeta) * xNought.rows(0, adjBeta);
    previousS = 0;
    for (int i = 0; i < lengthSeasonal; i++) {
      yHat(0, 0) = yHat(0, 0) + xNought((previousS + sPeriodsVec[i] + adjBeta), 0);
      previousS += sPeriodsVec[i];
    }
    if (lengthArma > 0) {
      yHat.col(0) = yHat(0, 0) + wTranspose.cols(tau + adjBeta + 1, xNought.n_rows - 1) * xNought.rows(tau + adjBeta + 1, xNought.n_rows - 1);
    }
    // Two
    e(0, 0) = y(0, 0) - yHat(0, 0);
    // Three
    x.submat(0, 0, adjBeta, 0) = F.submat(0, 0, adjBeta, adjBeta) * xNought.rows(0, adjBeta);
    if (lengthArma > 0) {
      x.submat(0, 0, adjBeta, 0) += F.submat(0, (adjBeta + tau + 1), adjBeta, (F.n_cols - 1)) * xNought.rows(adjBeta + tau + 1, F.n_cols - 1);
    }
    previousS = 0;
    for (int i = 0; i < lengthSeasonal; i++) {
      x((adjBeta + previousS + 1), 0) = xNought((adjBeta + previousS + sPeriodsVec[i]), 0);
      if (lengthArma > 0) {
        x.submat((adjBeta + previousS + 1), 0, (adjBeta + previousS + 1), 0) = x.submat((adjBeta + previousS + 1), 0, (adjBeta + previousS + 1), 0) + F.submat((adjBeta + previousS + 1), (adjBeta + tau + 1), (adjBeta + previousS + 1), (F.n_cols - 1)) * xNought.rows((adjBeta + tau + 1), (F.n_cols - 1));
      }
      x.submat((adjBeta + previousS + 2), 0, (adjBeta + previousS + sPeriodsVec[i]), 0) = xNought.rows((adjBeta + previousS + 1), (adjBeta + previousS + sPeriodsVec[i] - 1));
      previousS += sPeriodsVec[i];
    }
    if (p > 0) {
      x.submat((adjBeta + tau + 1), 0, (adjBeta + tau + 1), 0) = F.submat((adjBeta + tau + 1), (adjBeta + tau + 1), (adjBeta + tau + 1), (F.n_cols - 1)) * xNought.rows((adjBeta + tau + 1), (F.n_cols - 1));
      if (p > 1) {
        x.submat((adjBeta + tau + 2), 0, (adjBeta + tau + p), 0) = xNought.rows((adjBeta + tau + 1), (adjBeta + tau + p - 1));
      }
    }
    if (q > 0) {
      x((adjBeta + tau + p + 1), 0) = 0;
      if (q > 1) {
        x.submat((adjBeta + tau + p + 2), 0, (adjBeta + tau + p + q), 0) = xNought.rows((adjBeta + tau + p + 1), (adjBeta + tau + p + q - 1));
      }
    }

    x(0, 0) += g(0, 0) * e(0, 0);
    if (adjBeta == 1) {
      x(1, 0) += g(1, 0) * e(0, 0);
    }
    previousS = 0;
    for (int i = 0; i < lengthSeasonal; i++) {
      x((adjBeta + previousS + 1), 0) += g((adjBeta + previousS + 1), 0) * e(0, 0);
      previousS += sPeriodsVec[i];
    }
    if (p > 0) {
      x((adjBeta + tau + 1), 0) += e(0, 0);
      if (q > 0) {
        x((adjBeta + tau + p + 1), 0) += e(0, 0);
      }
    } else if (q > 0) {
      x((adjBeta + tau + 1), 0) += e(0, 0);
    }

    for (arma::uword t = 1; t < y.n_cols; t++) {
      // One
      yHat.col(t) = wTranspose.cols(0, adjBeta) * x.submat(0, (t - 1), adjBeta, (t - 1));
      previousS = 0;
      for (int i = 0; i < lengthSeasonal; i++) {
        yHat(0, t) += x((previousS + sPeriodsVec[i] + adjBeta), (t - 1));
        previousS += sPeriodsVec[i];
      }
      if (lengthArma > 0) {
        yHat.col(t) += wTranspose.cols((tau + adjBeta + 1), (xNought.n_rows - 1)) * x.submat((tau + adjBeta + 1), (t - 1), (x.n_rows - 1), (t - 1));
      }
      // Two
      e(0, t) = y(0, t) - yHat(0, t);
      // Three
      x.submat(0, t, adjBeta, t) = F.submat(0, 0, adjBeta, adjBeta) * x.submat(0, (t - 1), adjBeta, (t - 1));
      if (lengthArma > 0) {
        x.submat(0, t, adjBeta, t) += F.submat(0, (adjBeta + tau + 1), adjBeta, (F.n_cols - 1)) * x.submat((adjBeta + tau + 1), (t - 1), (F.n_cols - 1), (t - 1));
      }
      previousS = 0;
      for (int i = 0; i < lengthSeasonal; i++) {
        x((adjBeta + previousS + 1), t) = x((adjBeta + previousS + sPeriodsVec[i]), (t - 1));
        if (lengthArma > 0) {
          x.submat((adjBeta + previousS + 1), t, (adjBeta + previousS + 1), t) += F.submat((adjBeta + previousS + 1), (adjBeta + tau + 1), (adjBeta + previousS + 1), (F.n_cols - 1)) * x.submat((adjBeta + tau + 1), (t - 1), (F.n_cols - 1), (t - 1));
        }
        x.submat((adjBeta + previousS + 2), t, (adjBeta + previousS + sPeriodsVec[i]), t) = x.submat((adjBeta + previousS + 1), (t - 1), (adjBeta + previousS + sPeriodsVec[i] - 1), (t - 1));
        previousS += sPeriodsVec[i];
      }
      if (p > 0) {
        x.submat((adjBeta + tau + 1), t, (adjBeta + tau + 1), t) = F.submat((adjBeta + tau + 1), (adjBeta + tau + 1), (adjBeta + tau + 1), (F.n_cols - 1)) * x.submat((adjBeta + tau + 1), (t - 1), (F.n_cols - 1), (t - 1));
        if (p > 1) {
          x.submat((adjBeta + tau + 2), t, (adjBeta + tau + p), t) = x.submat( (adjBeta + tau + 1), (t - 1), (adjBeta + tau + p - 1), (t - 1));
        }
      }
      if (q > 0) {
        x((adjBeta + tau + p + 1), t) = 0;
        if (q > 1) {
          x.submat((adjBeta + tau + p + 2), t, (adjBeta + tau + p + q), t) = x.submat((adjBeta + tau + p + 1), (t - 1), (adjBeta + tau + p + q - 1), (t - 1));
        }
      }
      x(0, t) += g(0, 0) * e(0, t);
      if (adjBeta == 1) {
        x(1, t) += g(1, 0) * e(0, t);
      }
      previousS = 0;
      for (int i = 0; i < lengthSeasonal; i++) {
        x((adjBeta + previousS + 1), t) += g((adjBeta + previousS + 1), 0) * e(0, t);
        previousS += sPeriodsVec[i];
      }
      if (p > 0) {
        x((adjBeta + tau + 1), t) += e(0, t);
        if (q > 0) {
          x((adjBeta + tau + p + 1), t) += e(0, t);
        }
      } else if (q > 0) {
        x((adjBeta + tau + 1), t) += e(0, t);
      }
    }

  } else {
    yHat.col(0) = wTranspose * xNought;
    e(0, 0) = y(0, 0) - yHat(0, 0);
    x.col(0) = F * xNought + g * e(0, 0);

    for (arma::uword t = 1; t < y.n_cols; t++) {
      yHat.col(t) = wTranspose * x.col(t - 1);
      e(0, t) = y(0, t) - yHat(0, t);
      x.col(t) = F * x.col(t - 1) + g * e(0, t);
    }
  }
}

// [[Rcpp::export]]
arma::mat calcWTilda(arma::mat& wTildaTranspose, const arma::mat& D) {
  for (arma::uword t = 1; t < wTildaTranspose.n_rows; t++) {
    wTildaTranspose.row(t) = wTildaTranspose.row(t - 1) * D;
  }
  return wTildaTranspose;
}

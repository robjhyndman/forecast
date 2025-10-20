# Diebold-Mariano test. Modified from code by Adrian Trapletti.
# Then adapted by M. Yousaf Khan for better performance on small samples

#' Diebold-Mariano test for predictive accuracy
#'
#' The Diebold-Mariano test compares the forecast accuracy of two forecast
#' methods.
#'
#' This function implements the modified test proposed by Harvey, Leybourne and
#' Newbold (1997). The null hypothesis is that the two methods have the same
#' forecast accuracy. For `alternative = "less"`, the alternative hypothesis
#' is that method 2 is less accurate than method 1. For
#' `alternative = "greater"`, the alternative hypothesis is that method 2 is
#' more accurate than method 1. For `alternative = "two.sided"`, the
#' alternative hypothesis is that method 1 and method 2 have different levels
#' of accuracy. The long-run variance estimator can either the
#' auto-correlation estimator `varestimator = "acf"`, or the estimator based
#' on Bartlett weights `varestimator = "bartlett"` which ensures a positive estimate.
#' Both long-run variance estimators are proposed in Diebold and Mariano (1995).
#'
#' @param e1 Forecast errors from method 1.
#' @param e2 Forecast errors from method 2.
#' @param alternative A character string specifying the alternative hypothesis,
#' must be one of `"two.sided"` (default), `"greater"` or
#' `"less"`. You can specify just the initial letter.
#' @param h The forecast horizon used in calculating `e1` and `e2`.
#' @param power The power used in the loss function. Usually 1 or 2.
#' @param varestimator A character string specifying the long-run variance estimator.
#' Options are `"acf"` (default) or `"bartlett"`.
#' @return A list with class `htest` containing the following
#' components:
#' \item{statistic}{the value of the DM-statistic.}
#' \item{parameter}{the forecast horizon and loss function power used in the test.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' \item{varestimator}{a character string describing the long-run variance estimator.}
#' \item{p.value}{the p-value for the test.}
#' \item{method}{a character string with the value "Diebold-Mariano Test".}
#' \item{data.name}{a character vector giving the names of the two error series.}
#' @author George Athanasopoulos and Kirill Kuroptev
#' @references Diebold, F.X. and Mariano, R.S. (1995) Comparing predictive
#' accuracy. \emph{Journal of Business and Economic Statistics}, \bold{13},
#' 253-263.
#'
#' Harvey, D., Leybourne, S., & Newbold, P. (1997). Testing the equality of
#' prediction mean squared errors. \emph{International Journal of forecasting},
#' \bold{13}(2), 281-291.
#' @keywords htest ts
#' @examples
#'
#' # Test on in-sample one-step forecasts
#' f1 <- ets(WWWusage)
#' f2 <- auto.arima(WWWusage)
#' accuracy(f1)
#' accuracy(f2)
#' dm.test(residuals(f1), residuals(f2), h = 1)
#'
#' # Test on out-of-sample one-step forecasts
#' f1 <- ets(WWWusage[1:80])
#' f2 <- auto.arima(WWWusage[1:80])
#' f1.out <- ets(WWWusage[81:100], model = f1)
#' f2.out <- Arima(WWWusage[81:100], model = f2)
#' accuracy(f1.out)
#' accuracy(f2.out)
#' dm.test(residuals(f1.out), residuals(f2.out), h = 1)
#' @export

dm.test <- function(
  e1,
  e2,
  alternative = c("two.sided", "less", "greater"),
  h = 1,
  power = 2,
  varestimator = c("acf", "bartlett")
) {
  alternative <- match.arg(alternative)
  varestimator <- match.arg(varestimator)

  h <- as.integer(h)
  if (h < 1L) {
    stop("h must be at least 1")
  }
  if (h > length(e1)) {
    stop("h cannot be longer than the number of forecast errors")
  }
  d <- c(abs(e1))^power - c(abs(e2))^power
  d.cov <- acf(
    d,
    na.action = na.omit,
    lag.max = h - 1,
    type = "covariance",
    plot = FALSE
  )$acf[,, 1]
  n <- length(d)

  if (varestimator == "acf" || h == 1L) {
    # Original estimator
    d.var <- sum(c(d.cov[1], 2 * d.cov[-1])) / n
  } else {
    # varestimator == "bartlett"
    # Using Bartlett weights to ensure a positive estimate of long-run-variance
    d.var <- sum(c(d.cov[1], 2 * (1 - seq_len(h - 1) / h) * d.cov[-1])) / n
  }
  dv <- d.var

  if (dv > 0) {
    STATISTIC <- mean(d, na.rm = TRUE) / sqrt(dv)
  } else if (h == 1) {
    stop("Variance of DM statistic is zero")
  } else {
    warning(
      "Variance is negative. Try varestimator = bartlett. Proceeding with horizon h=1."
    )
    return(dm.test(e1, e2, alternative, h = 1, power, varestimator))
  }

  k <- ((n + 1 - 2 * h + (h / n) * (h - 1)) / n)^(1 / 2)
  STATISTIC <- STATISTIC * k
  names(STATISTIC) <- "DM"
  PVAL <- switch(
    alternative,
    two.sided = 2 * pt(-abs(STATISTIC), df = n - 1),
    less = pt(STATISTIC, df = n - 1),
    greater = pt(STATISTIC, df = n - 1, lower.tail = FALSE)
  )
  PARAMETER <- c(h, power)
  names(PARAMETER) <- c("Forecast horizon", "Loss function power")
  structure(
    list(
      statistic = STATISTIC,
      parameter = PARAMETER,
      alternative = alternative,
      varestimator = varestimator,
      p.value = PVAL,
      method = "Diebold-Mariano Test",
      data.name = c(deparse1(substitute(e1)), deparse1(substitute(e2)))
    ),
    class = "htest"
  )
}

is.htest <- function(x) {
  inherits(x, "htest")
}

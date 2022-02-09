# Diebold-Mariano test. Modified from code by Adrian Trapletti.
# Then adapted by M. Yousaf Khan for better performance on small samples



#' Diebold-Mariano test for predictive accuracy
#'
#' The Diebold-Mariano test compares the forecast accuracy of two forecast
#' methods.
#'
#' This function implements the modified test proposed by Harvey, Leybourne and
#' Newbold (1997). The null hypothesis is that the two methods have the same
#' forecast accuracy. For \code{alternative="less"}, the alternative hypothesis
#' is that method 2 is less accurate than method 1. For
#' \code{alternative="greater"}, the alternative hypothesis is that method 2 is
#' more accurate than method 1. For \code{alternative="two.sided"}, the
#' alternative hypothesis is that method 1 and method 2 have different levels
#' of accuracy. The used long-run-variance-estimator can be chosen, either as
#' auto-correlation-based \code{varestimator = "acf"} or as enhanced by bartlett-weights
#' \code{varestimator="bartlett"} to ensure a positive estimation. Both long-run-variance-estimator
#' are proposed in Diebold and Marian (1995).
#'
#' @param e1 Forecast errors from method 1.
#' @param e2 Forecast errors from method 2.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}.  You can specify just the initial letter.
#' @param h The forecast horizon used in calculating \code{e1} and \code{e2}.
#' @param power The power used in the loss function. Usually 1 or 2.
#' @param varestimator a charcater string specifiying the long-run-variance estimator,
#' must be either \code{"acf"} (default) or \code{"bartlett"}. You can specify just the initial letter.
#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value of the DM-statistic.}
#' \item{parameter}{the forecast horizon and loss function power used in the
#' test.} \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{varestimator}{a character string describing the long-run-variance-estimator.}
#' \item{p.value}{the p-value for the test.} \item{method}{a
#' character string with the value "Diebold-Mariano Test".} \item{data.name}{a
#' character vector giving the names of the two error series.}
#' @author George Athanasopoulos
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
#' dm.test(residuals(f1),residuals(f2),h=1)
#'
#' # Test on out-of-sample one-step forecasts
#' f1 <- ets(WWWusage[1:80])
#' f2 <- auto.arima(WWWusage[1:80])
#' f1.out <- ets(WWWusage[81:100],model=f1)
#' f2.out <- Arima(WWWusage[81:100],model=f2)
#' accuracy(f1.out)
#' accuracy(f2.out)
#' dm.test(residuals(f1.out),residuals(f2.out),h=1)
#'
#' Test on in-sample one-step forecasts
#' f1 <- arima(WWWusage,c(1,0,0))
#' f2 <- auto.arima(WWWusage)
#' accuracy(f1)
#' accuracy(f2)
#' dm.test(residuals(f1),residuals(f2),h=1)
#' dm_test_forpush(residuals(f1),residuals(f2),h=1, varestimator = "a")
#' dm_test_forpush(residuals(f1),residuals(f2),h=1, varestimator = "b")
#' # Test on out-of-sample one-step forecasts
#' f1 <- arima(WWWusage[1:80],c(1,0,0))
#' f2 <- auto.arima(WWWusage[1:80])
#' f1.out <- Arima(WWWusage[81:100],model=f1)
#' f2.out <- Arima(WWWusage[81:100],model=f2)
#' accuracy(f1.out)
#' accuracy(f2.out)
#' dm.test(residuals(f1.out),residuals(f2.out),h=1)
#' dm_test_forpush(residuals(f1.out),residuals(f2.out),h=1, varestimator = "a")
#' dm_test_forpush(residuals(f1.out),residuals(f2.out),h=1, varestimator = "b")
#' # Test on out-of-sample three-step forecasts
#' f1 <- arima(WWWusage[1:80],c(1,0,0))
#' f2 <- auto.arima(WWWusage[1:80])
#' f1.out <- forecast(WWWusage[81:83],model=f1,h=3)
#' f2.out <- forecast(WWWusage[81:83],model=f2,h=3)
#' dm.test(residuals(f1.out),residuals(f2.out),h=3)
#' dm_test_forpush(residuals(f1.out),residuals(f2.out),h=3, varestimator = "a")
#' dm_test_forpush(residuals(f1.out),residuals(f2.out),h=3, varestimator = "b")
#' # Test on out-of-sample ten-step forecasts
#' f1 <- arima(WWWusage[1:80],c(1,0,0))
#' f2 <- auto.arima(WWWusage[1:80])
#' f1.out <- forecast(f1,h=10)
#' f2.out <- forecast(f2,h=10)
#' f1.out.resids <- WWWusage[81:90]-f1.out$mean
#' f2.out.resids <- WWWusage[81:90]-f2.out$mean
#' dm.test(f1.out.resids,f2.out.resids,h=10)
#' dm_test_forpush(f1.out.resids,f2.out.resids,h=10, varestimator = "a")
#' dm_test_forpush(f1.out.resids,f2.out.resids,h=10, varestimator = "b")
#' @export


dm.test <- function(e1, e2, alternative = c("two.sided", "less", "greater"), h = 1, power = 2, varestimator=c("acf", "bartlett")){

  alternative <- match.arg(alternative)
  varestimator <- match.arg(varestimator)

  d <- c(abs(e1)) ^ power - c(abs(e2)) ^ power
  d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", plot = FALSE)$acf[, , 1]

  if (varestimator =="acf") {
    # Original estimator in the function
    d.var <- sum(c(d.cov[1], 2 * d.cov[-1])) / length(d)
  }else if(varestimator =="bartlett"){
    # Using bartlett weights to ensure a positiviley estimated long-run-variance
    m <- h-1
    b <- seq.int(from=1,to=m)
    d.var <- sum(c(d.cov[1], 2*(1-b/(m+1))*d.cov[-1])) / length(d)
  }else{
    stop("Please specifiy a valid long-run-Variance-Estimator!")
  }

  dv <- d.var

  if (dv > 0) {
    STATISTIC <- mean(d, na.rm = TRUE) / sqrt(dv)
  } else if (h == 1) {
    stop("Variance of DM statistic is zero")
  } else {
    stop("Variance is negative, use horizon h=1 or try bartlett based long-run-variance estimator.
          Test with horizon h=1 following.")
    return(dm.test(e1, e2, alternative, h = 1, power, varestimator))
  }

  n <- length(d)
  k <- ((n + 1 - 2 * h + (h / n) * (h - 1)) / n) ^ (1 / 2)
  STATISTIC <- STATISTIC * k
  names(STATISTIC) <- "DM"
  if (alternative == "two.sided") {
    PVAL <- 2 * pt(-abs(STATISTIC), df = n - 1)
  } else if (alternative == "less") {
    PVAL <- pt(STATISTIC, df = n - 1)
  } else if (alternative == "greater") {
    PVAL <- pt(STATISTIC, df = n - 1, lower.tail = FALSE)
  }
  PARAMETER <- c(h, power)
  names(PARAMETER) <- c("Forecast horizon", "Loss function power")
  structure(
    list(
      statistic = STATISTIC, parameter = PARAMETER,
      alternative = alternative,varestimator=varestimator, p.value = PVAL, method = "Diebold-Mariano Test",
      data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))
    ),
    class = "htest"
  )
}


is.htest <- function(x) {
  inherits(x, "htest")
}

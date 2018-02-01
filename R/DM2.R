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
#' of accuracy.
#'
#' @param e1 Forecast errors from method 1.
#' @param e2 Forecast errors from method 2.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}.  You can specify just the initial letter.
#' @param h The forecast horizon used in calculating \code{e1} and \code{e2}.
#' @param power The power used in the loss function. Usually 1 or 2.
#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value of the DM-statistic.}
#' \item{parameter}{the forecast horizon and loss function power used in the
#' test.} \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{p.value}{the p-value for the test.} \item{method}{a
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
#' @export
dm.test <- function(e1, e2, alternative = c("two.sided", "less", "greater"), h = 1, power = 2) {
  alternative <- match.arg(alternative)
  d <- c(abs(e1)) ^ power - c(abs(e2)) ^ power
  d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", plot = FALSE)$acf[, , 1]
  d.var <- sum(c(d.cov[1], 2 * d.cov[-1])) / length(d)
  dv <- d.var # max(1e-8,d.var)
  if (dv > 0) {
    STATISTIC <- mean(d, na.rm = TRUE) / sqrt(dv)
  } else if (h == 1) {
    stop("Variance of DM statistic is zero")
  } else {
    warning("Variance is negative, using horizon h=1")
    return(dm.test(e1, e2, alternative, h = 1, power))
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
      alternative = alternative, p.value = PVAL, method = "Diebold-Mariano Test",
      data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))
    ),
    class = "htest"
  )
}

is.htest <- function(x) {
  inherits(x, "htest")
}

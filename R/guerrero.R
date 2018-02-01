# This R script contains code for extracting the Box-Cox
# parameter, lambda, using Guerrero's method (1993).
# Written by Leanne Chhay

# guer.cv computes the coefficient of variation
# Input:
# 	lam = lambda
# 	x = original time series as a time series object
# Output: coefficient of variation

guer.cv <- function(lam, x, nonseasonal.length=2) {
  period <- round(max(nonseasonal.length, frequency(x)))
  nobsf <- length(x)
  nyr <- floor(nobsf / period)
  nobst <- floor(nyr * period)
  x.mat <- matrix(x[(nobsf - nobst + 1):nobsf], period, nyr)
  x.mean <- apply(x.mat, 2, mean, na.rm = TRUE)
  x.sd <- apply(x.mat, 2, sd, na.rm = TRUE)
  x.rat <- x.sd / x.mean ^ (1 - lam)
  return(sd(x.rat, na.rm = TRUE) / mean(x.rat, na.rm = TRUE))
}

# guerrero extracts the required lambda
# Input: x = original time series as a time series object
# Output: lambda that minimises the coefficient of variation

guerrero <- function(x, lower=-1, upper=2, nonseasonal.length=2) {
  return(optimize(
    guer.cv, c(lower, upper), x = x,
    nonseasonal.length = nonseasonal.length
  )$minimum)
}


# Modified version of boxcox from MASS package
bcloglik <- function(x, lower=-1, upper=2) {
  n <- length(x)
  if (any(x <= 0, na.rm = TRUE)) {
    stop("x must be positive")
  }
  logx <- log(na.omit(c(x)))
  xdot <- exp(mean(logx))
  if (all(class(x) != "ts")) {
    fit <- lm(x ~ 1, data = data.frame(x = x), na.action = na.exclude)
  } else if (frequency(x) > 1) {
    fit <- tslm(x ~ trend + season, data = data.frame(x = x))
  } else {
    fit <- tslm(x ~ trend, data = data.frame(x = x))
  }
  xqr <- fit$qr
  lambda <- seq(lower, upper, by = .05)
  xl <- loglik <- as.vector(lambda)
  m <- length(xl)
  x <- na.omit(c(x))
  for (i in 1L:m)
  {
    if (abs(la <- xl[i]) > 0.02) {
      xt <- (x ^ la - 1) / la
    } else {
      xt <- logx * (1 + (la * logx) / 2 * (1 + (la * logx) / 3 * (1 + (la * logx) / 4)))
    }
    loglik[i] <- -n / 2 * log(sum(qr.resid(xqr, xt / xdot ^ (la - 1)) ^ 2))
  }
  return(xl[which.max(loglik)])
}



#' Automatic selection of Box Cox transformation parameter
#'
#' If \code{method=="guerrero"}, Guerrero's (1993) method is used, where lambda
#' minimizes the coefficient of variation for subseries of \code{x}.
#'
#' If \code{method=="loglik"}, the value of lambda is chosen to maximize the
#' profile log likelihood of a linear model fitted to \code{x}. For
#' non-seasonal data, a linear time trend is fitted while for seasonal data, a
#' linear time trend with seasonal dummy variables is used.
#'
#'
#' @param x a numeric vector or time series of class \code{ts}
#' @param method Choose method to be used in calculating lambda.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @return a number indicating the Box-Cox transformation parameter.
#' @author Leanne Chhay and Rob J Hyndman
#' @seealso \code{\link{BoxCox}}
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#'
#' Guerrero, V.M. (1993) Time-series analysis supported by power
#' transformations. \emph{Journal of Forecasting}, \bold{12}, 37--48.
#' @keywords ts
#' @examples
#'
#' lambda <- BoxCox.lambda(AirPassengers,lower=0)
#' air.fit <- Arima(AirPassengers, order=c(0,1,1),
#'                  seasonal=list(order=c(0,1,1),period=12), lambda=lambda)
#' plot(forecast(air.fit))
#'
#' @export
BoxCox.lambda <- function(x, method=c("guerrero", "loglik"), lower=-1, upper=2) {
  if (any(x <= 0, na.rm = TRUE)) {
    lower <- max(lower, 0)
  }
  if (length(x) <= 2 * frequency(x)) {
    return(1)
  } # Not enough data to do much more than this
  #   stop("All values must be positive")
  method <- match.arg(method)
  if (method == "loglik") {
    return(bcloglik(x, lower, upper))
  } else {
    return(guerrero(x, lower, upper))
  }
}

#' Croston forecast model
#'
#' Based on Croston's (1972) method for intermittent demand forecasting, also described in Shenstone and Hyndman (2005). 
#' Croston's method involves using simple exponential smoothing (SES) on the non-zero elements of the time series 
#' and a separate application of SES to the times between non-zero elements of the time series. 
#' Returns a model object that can be used to generate forecasts using Croston's method
#' for intermittent demand time series. It isn't a true statistical model in that it
#' doesn't describe a data generating process that would lead to the forecasts produced
#' using Croston's method.
#' 
#' Note that prediction intervals are not computed as Croston's method has no
#' underlying stochastic model. 
#' 
#' There are two variant methods available which apply multiplicative correction factors
#' to the forecasts that result from the original Croston's method. For the
#' Syntetos-Boylan approximation (`type = "sba"`), this factor is \eqn{1 - \alpha / 2},
#' and for the Shale-Boylan-Johnston method (`type = "sbj"`), this factor is
#' \eqn{1 - \alpha / (2 - \alpha)}, where \eqn{\alpha} is the smoothing parameter for
#' the interval SES application.
#' 
#' @inheritParams Arima
#' @param alpha Value of alpha. Default value is 0.1.
#' @param type Which variant of Croston's method to use. Defaults to `"croston"` for
#' Croston's method, but can also be set to `"sba"` for the Syntetos-Boylan
#' approximation, and `"sbj"` for the Shale-Boylan-Johnston method.
#' @references Croston, J. (1972) "Forecasting and stock control for
#' intermittent demands", \emph{Operational Research Quarterly}, \bold{23}(3),
#' 289-303.
#' 
#' Shale, E.A., Boylan, J.E., & Johnston, F.R. (2006). Forecasting for intermittent demand: 
#' the estimation of an unbiased average. \emph{Journal of the Operational Research Society}, \bold{57}(5), 588-592.
#' 
#' Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying
#' Croston's method for intermittent demand forecasting". \emph{Journal of
#' Forecasting}, \bold{24}, 389-402.
#' 
#' Syntetos A.A., Boylan J.E. (2001). On the bias of intermittent demand estimates. 
#' \emph{International Journal of Production Economics}, \bold{71}, 457–466.
#' @author Rob J Hyndman
#' @return An object of class `croston_model`
#' @examples
#' y <- rpois(20, lambda = 0.3)
#' fit <- croston_model(y)
#' forecast(fit) |> autoplot()
#' @export
croston_model <- function(y, alpha = 0.1, type = c("croston", "sba", "sbj")) {
  type <- match.arg(type)
  if(alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
  }
  if (any(y < 0)) {
    stop("Croston's model only applies to non-negative data")
  }
  non_zero <- which(y != 0)
  if (length(non_zero) < 2) {
    stop("At least two non-zero values are required to use Croston's method.")
  }
  series <- deparse1(substitute(y))
  y <- as.ts(y)
  y_demand <- y[non_zero]
  y_interval <- c(non_zero[1], diff(non_zero))
  k <- length(y_demand)
  fit_demand <- numeric(k)
  fit_interval <- numeric(k)
  fit_demand[1] <- y_demand[1]
  fit_interval[1] <- y_interval[1]
  for (i in 2:k) {
    fit_demand[i] <- fit_demand[i - 1] +
      alpha * (y_demand[i] - fit_demand[i - 1])
    fit_interval[i] <- fit_interval[i - 1] +
      alpha * (y_interval[i] - fit_interval[i - 1])
  }
  if (type == "sba") {
    coeff <- 1 - alpha / 2
  } else if (type == "sbj") {
    coeff <- 1 - alpha / (2 - alpha)
  } else {
    coeff <- 1
  }
  ratio <- coeff * fit_demand / fit_interval
  fits <- rep(c(0, ratio), diff(c(0, non_zero, length(y))))
  fits[1] <- NA_real_
  output <- list(
    alpha = alpha,
    type = type,
    y = y,
    fit_demand = fit_demand,
    fit_interval = fit_interval,
    fitted = fits,
    residuals = y - fits,
    series = series
  )
  output$call <- match.call()
  structure(output, class = c("fc_model", "croston_model"))
}

#' @export
print.croston_model <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("Call:", deparse(x$call), "\n\n")
  cat("alpha:", format(x$alpha, digits = digits), "\n")
  cat("method:", x$type, "\n")
  invisible(x)
}

#' Forecasts for intermittent demand using Croston's method
#'
#' Returns forecasts and other information for Croston's forecasts applied to
#' y.
#'
#' Based on Croston's (1972) method for intermittent demand forecasting, also
#' described in Shenstone and Hyndman (2005). Croston's method involves using
#' simple exponential smoothing (SES) on the non-zero elements of the time
#' series and a separate application of SES to the times between non-zero
#' elements of the time series. The smoothing parameters of the two
#' applications of SES are assumed to be equal and are denoted by `alpha`.
#'
#' Note that prediction intervals are not computed as Croston's method has no
#' underlying stochastic model.
#'
#' @inheritParams croston_model
#' @inheritParams forecast.ts
#' @param object An object of class `croston_model` as returned by [croston_model()].
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [ses()].
#' @references Croston, J. (1972) "Forecasting and stock control for
#' intermittent demands", \emph{Operational Research Quarterly}, \bold{23}(3),
#' 289-303.
#' 
#' Shale, E.A., Boylan, J.E., & Johnston, F.R. (2006). Forecasting for intermittent demand: 
#' the estimation of an unbiased average. \emph{Journal of the Operational Research Society}, \bold{57}(5), 588-592.
#' 
#' Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying
#' Croston's method for intermittent demand forecasting". \emph{Journal of
#' Forecasting}, \bold{24}, 389-402.
#' 
#' Syntetos A.A., Boylan J.E. (2001). On the bias of intermittent demand estimates. 
#' \emph{International Journal of Production Economics}, \bold{71}, 457–466.
#' @keywords ts
#' @examples
#' y <- rpois(20, lambda = 0.3)
#' fcast <- croston(y)
#' autoplot(fcast)
#'
#' @export
forecast.croston_model <- function(object, h = 10, ...) {
  m <- frequency(object$y)
  start <- tsp(object$y)[2] + 1 / m
  output <- list(
    mean = ts(
      rep(object$fitted[length(object$fitted)], h),
      start = start,
      frequency = m
    ),
    x = object$y,
    fitted = object$fitted,
    residuals = object$residuals,
    method = "Croston's method",
    series = object$series
  )
  output$model <- list(alpha = object$alpha)
  structure(output, class = "forecast")
}

#' @rdname forecast.croston_model
#' @param x Deprecated. Included for backwards compatibility.
#' @inheritParams croston_model
#' @export
croston <- function(y, h = 10, alpha = 0.1, type = c("croston", "sba", "sbj"), x = y) {
  fit <- croston_model(x, alpha=alpha, type = type)
  fit$series <- deparse1(substitute(y))
  forecast(fit, h = h)
}

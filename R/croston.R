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
#' @param alpha Value of alpha. Default value is 0.1. If `NULL`, the alpha and initial values are
#'   jointly optimized.
#' @param type Which variant of Croston's method to use. Defaults to `"croston"` for
#' Croston's method, but can also be set to `"sba"` for the Syntetos-Boylan
#' approximation, and `"sbj"` for the Shale-Boylan-Johnston method.
#' @param init Either a string specifying the initialization method for the SES components
#'   (`"naive"` or `"mean"`), or a numeric vector of length 2 giving the initial values for
#'   demand and interval respectively. When a string is used, demand is initialized to the first
#'   non-zero value and interval is initialized using the selected method. String values are used as
#'   starting points for optimization when `alpha = NULL`; numeric values are fixed.
#' @param opt.crit Optimization criterion when `alpha = NULL`. Either `"mse"` or `"mae"`.
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
croston_model <- function(
  y,
  alpha = 0.1,
  type = c("croston", "sba", "sbj"),
  init = c("naive", "mean"),
  opt.crit = c("mse", "mae")
) {
  type <- match.arg(type)
  opt.crit <- match.arg(opt.crit)
  if (!is.null(alpha) && (alpha < 0 || alpha > 1)) {
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
  if (is.numeric(init)) {
    if (length(init) != 2) {
      stop("init must be a numeric vector of length 2 (demand, interval)")
    }
    if (init[1] < 0) {
      stop("Initial demand must be non-negative")
    }
    if (init[2] < 1) {
      stop("Initial interval must be at least 1")
    }
    init_demand <- init[1]
    init_interval <- init[2]
    opt_init <- FALSE
  } else {
    init <- match.arg(init)
    init_demand <- y_demand[1]
    init_interval <- if (init == "mean") mean(y_interval) else y_interval[1]
    opt_init <- TRUE
  }
  if (is.null(alpha)) {
    par <- c(0.1, init_demand, init_interval)
    # Only optimize init values if not user-supplied,
    # and init_interval only if more than one feasible value
    par_est <- c(TRUE, opt_init, opt_init && max(y_interval) > 1)
    opt <- optim(
      par = par[par_est],
      fn = function(opt_par) {
        par[par_est] <- opt_par
        croston_cost(
          y,
          alpha = par[1],
          y_demand,
          y_interval,
          init_demand = par[2],
          init_interval = par[3],
          non_zero,
          type,
          opt.crit
        )
      },
      lower = c(0, 0, 1)[par_est],
      upper = c(1, max(y_demand), max(y_interval))[par_est],
      method = "L-BFGS-B",
      control = list(maxit = 2000)
    )
    par[par_est] <- opt$par
    alpha <- min(max(par[1], 0), 1)
    init_demand <- par[2]
    init_interval <- par[3]
  }
  res <- croston_fit(
    alpha,
    y_demand,
    y_interval,
    init_demand,
    init_interval,
    non_zero,
    length(y),
    type
  )
  output <- list(
    alpha = alpha,
    type = type,
    init = init,
    y = y,
    fit_demand = res$fit_demand,
    fit_interval = res$fit_interval,
    fitted = res$fitted,
    residuals = y - res$fitted,
    series = series
  )
  output$call <- match.call()
  structure(output, class = c("fc_model", "croston_model"))
}

croston_fit <- function(
  alpha,
  y_demand,
  y_interval,
  init_demand,
  init_interval,
  non_zero,
  n,
  type
) {
  k <- length(y_demand)
  fit_demand <- numeric(k)
  fit_interval <- numeric(k)
  fit_demand[1] <- init_demand
  fit_interval[1] <- init_interval
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
  fitted <- rep(c(NA_real_, ratio), diff(c(0, non_zero, n)))
  list(fit_demand = fit_demand, fit_interval = fit_interval, fitted = fitted)
}

croston_cost <- function(
  y,
  alpha,
  y_demand,
  y_interval,
  init_demand,
  init_interval,
  non_zero,
  type,
  opt.crit
) {
  res <- croston_fit(
    alpha,
    y_demand,
    y_interval,
    init_demand,
    init_interval,
    non_zero,
    length(y),
    type
  )
  resid <- y - res$fitted
  if (opt.crit == "mae") {
    mean(abs(resid), na.rm = TRUE)
  } else {
    mean(resid^2, na.rm = TRUE)
  }
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
croston <- function(
  y,
  h = 10,
  alpha = 0.1,
  type = c("croston", "sba", "sbj"),
  init = c("naive", "mean"),
  opt.crit = c("mse", "mae"),
  x = y
) {
  fit <- croston_model(
    x,
    alpha = alpha,
    type = type,
    init = init,
    opt.crit = opt.crit
  )
  fit$series <- deparse1(substitute(y))
  forecast(fit, h = h)
}

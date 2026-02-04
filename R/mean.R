# Mean model

#' Mean Forecast Model
#'
#' Fits a Gaussian iid model to a univariate time series.
#'
#' The model assumes that the data are independent and identically distributed
#'
#' \deqn{Y_t \sim N(\mu,\sigma^2)}{Y[t] ~ N(mu, sigma^2)}
#'
#' Forecasts are given by
#'
#' \deqn{Y_{n+h|n}=\mu}{Y[n+h|n]=mu}
#'
#' where \eqn{\mu}{mu} is estimated by the sample mean.
#'
#' The function [summary()] is used to obtain and print a summary of the
#' results, while the function [plot()] produces a plot of the forecasts and
#' prediction intervals.
#' The generic accessor functions [stats::fitted()] and [stats::residuals()]
#' extract useful features of the object returned by [mean_model()].
#'
#' @inheritParams ets
#' @return An object of class `mean_model`.
#' @inheritSection forecast.ts forecast class
#' @seealso [forecast.mean_model()], [meanf()]
#' @author Rob J Hyndman
#' @keywords ts
#' @examples
#' fit_nile <- mean_model(Nile)
#' fit_nile |> forecast(h = 10) |> autoplot()
#' @export
mean_model <- function(y, lambda = NULL, biasadj = FALSE) {
  seriesname <- deparse1(substitute(y))
  if (inherits(y, c("data.frame", "list", "matrix", "mts"))) {
    stop("y should be a univariate time series")
  }
  y <- as.ts(y)
  orig.y <- y
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }
  n <- length(y)
  mu <- mean(y, na.rm = TRUE)
  s <- sd(y, na.rm = TRUE)
  fits <- rep(mu, n)
  res <- y - fits
  fits <- copy_msts(y, fits)
  res <- copy_msts(y, res)
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda, biasadj, s^2)
  }

  out <- list(
    method = "Mean",
    y = orig.y,
    series = seriesname,
    mu = mu,
    sigma = s,
    mu.se = s / sqrt(n),
    lambda = lambda,
    fitted = fits,
    residuals = res
  )
  out$call <- match.call()
  structure(out, class = c("fc_model", "mean_model"))
}

#' @export
print.mean_model <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Call:", deparse(x$call), "\n\n")
  cat("Mean:", format(x$mu, digits = digits), "\n")
  cat("Standard deviation:", format(x$sigma, digits = digits), "\n")
  invisible(x)
}

#' Mean Forecast
#'
#' Returns forecasts and prediction intervals for a Gaussian iid model.
#' [meanf()] is a convenience function that combines [mean_model()] and [forecast()].
#'
#' @inherit mean_model details
#' @param object An object of class `mean_model` as returned by [mean_model()].
#' @inheritParams mean_model
#' @inheritParams forecast.ets
#' @param ... Additional arguments not used.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @examples
#' fit_nile <- mean_model(Nile)
#' fit_nile |> forecast(h = 10) |> autoplot()
#' nile.fcast <- meanf(Nile, h = 10)
#' @seealso [mean_model()]
#' @keywords ts
#' @export
forecast.mean_model <- function(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(object$lambda, "biasadj"),
  bootstrap = FALSE,
  npaths = 5000,
  ...
) {
  n <- length(object$y)
  f <- rep(object$mu, h)
  level <- getConfLevel(level, fan)
  nconf <- length(level)
  if (bootstrap) {
    res <- object$residuals
    e <- na.omit(res) - mean(res, na.rm = TRUE)
    sim <- matrix(
      sample(e, size = npaths * h, replace = TRUE),
      ncol = npaths,
      nrow = h
    )
    sim <- sweep(sim, 1, f, "+")
    lower <- t(apply(sim, 1, quantile, prob = .5 - level / 200))
    upper <- t(apply(sim, 1, quantile, prob = .5 + level / 200))
  } else {
    lower <- upper <- matrix(NA, nrow = h, ncol = nconf)
    for (i in seq_len(nconf)) {
      if (n > 1) {
        tfrac <- qt(0.5 - level[i] / 200, n - 1)
      } else {
        tfrac <- -Inf
      }
      w <- -tfrac * object$sigma * sqrt(1 + 1 / n)
      lower[, i] <- f - w
      upper[, i] <- f + w
    }
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")
  f <- future_msts(object$y, f)
  lower <- future_msts(object$y, lower)
  upper <- future_msts(object$y, upper)

  if (!is.null(lambda)) {
    f <- InvBoxCox(
      f,
      lambda,
      biasadj,
      list(level = level, upper = upper, lower = lower)
    )
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }

  out <- list(
    model = object,
    method = "Mean",
    mean = f,
    lower = lower,
    upper = upper,
    level = level,
    x = object$y,
    residuals = object$residuals,
    fitted = object$fitted,
    lambda = lambda,
    series = object$series
  )
  out$model$call <- match.call()

  structure(out, class = "forecast")
}

#' @rdname forecast.mean_model
#' @inheritParams ses
#' @export
meanf <- function(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  bootstrap = FALSE,
  npaths = 5000,
  x = y
) {
  fit <- mean_model(y = x, lambda = lambda, biasadj = biasadj)
  fit$series <- deparse1(substitute(y))
  forecast(
    fit,
    h = h,
    level = level,
    fan = fan,
    lambda = lambda,
    biasadj = biasadj,
    bootstrap = bootstrap,
    npaths = npaths
  )
}

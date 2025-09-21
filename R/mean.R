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
#' The function [base::summary()] is used to obtain and print a summary of the
#' results, while the function [base::plot()] produces a plot of the forecasts and
#' prediction intervals.
#' The generic accessor functions [stats::fitted()] and [stats::residuals()]
#' extract useful features of the object returned by [mean_model()].
#'
#' @param y A univariate time series of class `ts`.
#' @inheritParams forecast.ts
#' @return An object of class `mean_model`.
#' @seealso [forecast.mean_model()], [meanf()]
#' @author Rob J Hyndman
#' @keywords ts
#' @examples
#' fit_nile <- mean_model(Nile)
#' fit_nile |> forecast(h=10) |> autoplot()
#' @export
mean_model <- function(y, lambda = NULL, biasadj = FALSE) {
  seriesname <- deparse(substitute(y))
  if (inherits(y, c("data.frame", "list", "matrix", "mts"))) {
    stop("y should be a univariate time series")
  }
  y <- as.ts(y)
  orig.y <- y
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
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
    attr(lambda, "biasadj") <- biasadj
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
  structure(out, class = "mean_model")
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
#' @details [meanf()] is a convenience function that combines [mean_model()] and [forecast()].
#'
#' @inheritParams forecast.Arima
#' @param object An object of class `mean_model` as returned by [mean_model()].
#' @param y A univariate time series of class `ts`.
#'
#' @return An object of class `forecast` which is a list containing at least the
#' following elements:
#' \describe{
#'  \item{`model`}{A list containing information about the fitted model}
#'  \item{`method`}{The name of the forecasting method as a character string}
#'  \item{`mean`}{Point forecasts as a time series}
#'  \item{`lower`}{Lower limits for prediction intervals}
#'  \item{`upper`}{Upper limits for prediction intervals}
#'  \item{`level`}{The confidence values associated with the prediction intervals}
#'  \item{`x`}{The original time series}
#'  \item{`residuals`}{Residuals from the fitted model}
#'  \item{`fitted`}{Fitted values (one-step forecasts)}
#' }
#' @examples
#' fit_nile <- mean_model(Nile)
#' fit_nile |> forecast(h=10) |> autoplot()
#' nile.fcast <- meanf(Nile, h = 10)
#' @seealso [mean_model()]
#' @export
forecast.mean_model <- function(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = NULL,
  bootstrap = FALSE,
  npaths = 5000,
  ...
) {
  n <- length(object$y)
  f <- rep(object$mu, h)
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }
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
    for (i in 1:nconf) {
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
    lambda = lambda
  )
  out$model$call <- match.call()

  return(structure(out, class = "forecast"))
}

#' @rdname forecast.mean_model
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
  fit$series <- deparse(substitute(y))
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

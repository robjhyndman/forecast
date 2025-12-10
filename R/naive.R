#' Random walk model
#'
#' Fit a generalized random walk with Gaussian errors (and optional drift) to a univariate time series.
#'
#' The model assumes that
#'
#' \deqn{Y_t = Y_{t-p} + c + \varepsilon_{t}}{Y[t] = Y[t-p] + epsilon[t]}
#'
#' where \eqn{p} is the lag parameter,
#' \eqn{c} is the drift parameter, and
#' \eqn{\varepsilon_t\sim N(0,\sigma^2)}{Y[t] ~ N(0, sigma^2)} are iid.
#'
#' The model without drift has \eqn{c=0}.
#' In the model with drift, \eqn{c} is estimated
#' by the sample mean of the differences \eqn{Y_t - Y_{t-p}}{Y[t] - Y[t-p]}.
#'
#' If \eqn{p=1}, this is equivalent to an ARIMA(0,1,0) model with
#' an optional drift coefficient. For \eqn{p>1}, it is equivalent to an
#' ARIMA(0,0,0)(0,1,0)p model.
#'
#' The forecasts are given by
#'
#' \deqn{Y_{T+h|T}= Y_{T+h-p(k+1)} + ch}{Y[T+h|T] = Y[T+h-p(k+1)]+ch}
#'
#' where \eqn{k} is the integer part of \eqn{(h-1)/p}.
#' For a regular random walk, \eqn{p=1} and \eqn{c=0}, so all forecasts are equal to the last observation.
#' Forecast standard errors allow for uncertainty in estimating the drift parameter
#' (unlike the corresponding forecasts obtained by fitting an ARIMA model
#' directly).
#'
#' The generic accessor functions [stats::fitted()] and [stats::residuals()]
#' extract useful features of the object returned.
#'
#' @inheritParams ses
#' @param lag Lag parameter. `lag = 1` corresponds to a standard random walk (giving naive forecasts if `drift = FALSE` or drift forecasts if `drift = TRUE`),
#' while `lag = m` corresponds to a seasonal random walk where m is the seasonal period (giving seasonal naive forecasts if `drift = FALSE`).
#' @param drift Logical flag. If `TRUE`, fits a random walk with drift model.
#' @export
#' @seealso [forecast.rw_model()], [rwf()], [naive()], [snaive()]
#' @return An object of class `rw_model`.
#' @examples
#' model <- rw_model(gold)
#' forecast(model, h = 50) |> autoplot()
rw_model <- function(
  y,
  lag = 1,
  drift = FALSE,
  lambda = NULL,
  biasadj = FALSE
) {
  seriesname <- deparse1(substitute(y))
  if (!is.ts(y)) {
    y <- as.ts(y)
  }
  dimy <- dim(y)
  if (!is.null(dimy) && dimy[2] > 1) {
    stop(
      "Multivariate time series detected. This function is designed for univariate time series only."
    )
  }
  origy <- y
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
    attr(lambda, "biasadj") <- biasadj
}

  m <- frequency(y)
  # Complete missing values with lagged values
  y_na <- which(is.na(y))
  y_na <- y_na[y_na > lag]
  fits <- stats::lag(y, -lag)
  for (i in y_na) {
    if (is.na(fits)[i]) {
      fits[i] <- fits[i - lag]
    }
  }

  fitted <- ts(
    c(rep(NA, lag), head(fits, -lag)),
    start = start(y),
    frequency = m
  )
  fitted <- copy_msts(y, fitted)
  if (drift) {
    fit <- summary(lm(y - fitted ~ 1, na.action = na.exclude))
    b <- fit$coefficients[1, 1]
    b.se <- fit$coefficients[1, 2]
    sigma <- fit$sigma
    fitted <- fitted + b
    res <- y - fitted
    method <- "Lag walk with drift"
  } else {
    res <- y - fitted
    b <- b.se <- 0
    sigma <- sqrt(mean(res^2, na.rm = TRUE))
    method <- "Lag walk"
  }

  if (!is.null(lambda)) {
    fitted <- InvBoxCox(fitted, lambda, biasadj, var(res))
  }

  structure(
    list(
      x = origy,
      fitted = fitted,
      future = tail(fits, lag),
      residuals = res,
      method = method,
      series = seriesname,
      sigma2 = sigma^2,
      par = list(includedrift = drift, drift = b, drift.se = b.se, lag = lag),
      lambda = lambda,
      call = match.call()
    ),
    class = c("fc_model", "rw_model")
  )
}

#' @export
print.rw_model <- function(x, ...) {
  cat(paste("Call:", deparse(x$call), "\n\n"))
  if (x$par$includedrift) {
    cat(paste0(
      "Drift: ",
      round(x$par$drift, 4),
      "  (se ",
      round(x$par$drift.se, 4),
      ")\n"
    ))
  }
  cat(paste("Residual sd:", round(sqrt(x$sigma2), 4), "\n"))
}

#' @export
fitted.rw_model <- function(object, ...) {
  object$fitted
}

#' Naive and Random Walk Forecasts
#'
#' Returns forecasts and prediction intervals for a generalized random walk model.
#' [rwf()] is a convenience function that combines [rw_model()] and [forecast()].
#' [naive()] is a wrapper to [rwf()] with `drift=FALSE` and `lag=1`, while
#' [snaive()] is a wrapper to [rwf()] with `drift=FALSE` and `lag=frequency(y)`.
#'
#' @inherit rw_model details
#' @param object An object of class `rw_model` returned by [rw_model()].
#' @inheritParams rw_model
#' @inheritParams forecast.ets
#' @param ... Additional arguments not used.
#' @inheritSection forecast.ts forecast class
#' @return An object of class `forecast`.
#' @author Rob J Hyndman
#' @seealso [rw_model()], [Arima()]
#' @keywords ts
#' @examples
#' # Three ways to do the same thing
#' gold_model <- rw_model(gold)
#' gold_fc1 <- forecast(gold_model, h = 50)
#' gold_fc2 <- rwf(gold, h = 50)
#' gold_fc3 <- naive(gold, h = 50)
#'
#' # Plot the forecasts
#' autoplot(gold_fc1)
#'
#' # Drift forecasts
#' rwf(gold, drift = TRUE) |> autoplot()
#'
#' # Seasonal naive forecasts
#' snaive(wineind) |> autoplot()
#' @export
forecast.rw_model <- function(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  npaths = 5000,
  innov = NULL,
  lambda = object$lambda,
  biasadj = FALSE,
  ...
) {
  lag <- object$par$lag
  fullperiods <- (h - 1) / lag + 1
  steps <- rep(1:fullperiods, rep(lag, fullperiods))[1:h]

  # Point forecasts
  fc <- rep(object$future, fullperiods)[1:h] + steps * object$par$drift

  # Intervals
  # Adjust prediction intervals to allow for drift coefficient standard error
  mse <- sum(object$residuals^2, na.rm = TRUE) /
    (sum(!is.na(object$residuals)) - (object$par$drift != 0))
  se <- sqrt(mse * steps + (steps * object$par$drift.se)^2)

  level <- getConfLevel(level, fan)
  nconf <- length(level)

  if (simulate || bootstrap) {
    # Compute prediction intervals using simulations
    hilo <- simulate_forecast(
      object = object,
      h = h,
      level = level,
      npaths =npaths,
      bootstrap = bootstrap,
      innov = innov,
      lambda = lambda,
      ...
    )
    lower <- hilo$lower
    upper <- hilo$upper
  } else {
    z <- qnorm(.5 + level / 200)
    lower <- upper <- matrix(NA, nrow = h, ncol = nconf)
    for (i in 1:nconf) {
      lower[, i] <- fc - z[i] * se
      upper[, i] <- fc + z[i] * se
    }
  }

  if (!is.null(lambda)) {
    fc <- InvBoxCox(fc, lambda, biasadj, se^2)
    if (!bootstrap && !simulate) {
      # Bootstrap intervals are already backtransformed
      upper <- InvBoxCox(upper, lambda)
      lower <- InvBoxCox(lower, lambda)
    }
  }

  # Set attributes
  fc <- future_msts(object$x, fc)
  lower <- future_msts(object$x, lower)
  upper <- future_msts(object$x, upper)
  colnames(lower) <- colnames(upper) <- paste0(level, "%")

  structure(
    list(
      method = object$method,
      model = object,
      lambda = lambda,
      x = object$x,
      fitted = fitted(object),
      residuals = residuals(object),
      series = object$series,
      mean = fc,
      level = level,
      lower = lower,
      upper = upper
    ),
    class = "forecast"
  )
}

#' @rdname forecast.rw_model
#' @export
rwf <- function(
  y,
  h = 10,
  drift = FALSE,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  lag = 1,
  ...,
  x = y
) {
  fit <- rw_model(
    x,
    lag = lag,
    drift = drift,
    lambda = lambda,
    biasadj = biasadj
  )

  fc <- forecast(
    fit,
    h = h,
    level = level,
    fan = fan,
    lambda = fit$lambda,
    biasadj = biasadj,
    ...
  )

  fc$model$call <- match.call()
  fc$series <- deparse1(substitute(y))

  if (drift) {
    fc$method <- "Random walk with drift"
  } else {
    fc$method <- "Random walk"
  }
  fc
}

#' @rdname forecast.rw_model
#' @inheritParams ses
#' @export
naive <- function(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  ...,
  x = y
) {
  fc <- rwf(
    x,
    h = h,
    level = level,
    fan = fan,
    lambda = lambda,
    drift = FALSE,
    biasadj = biasadj,
    ...
  )
  fc$model$call <- match.call()
  fc$series <- deparse1(substitute(y))
  fc$method <- "Naive method"
  fc
}

#' @rdname forecast.rw_model
#' @export
snaive <- function(
  y,
  h = 2 * frequency(x),
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  ...,
  x = y
) {
  fc <- rwf(
    x,
    h = h,
    level = level,
    fan = fan,
    lambda = lambda,
    drift = FALSE,
    biasadj = biasadj,
    lag = frequency(x)
  )
  fc$model$call <- match.call()
  fc$series <- deparse1(substitute(y))
  fc$method <- "Seasonal naive method"
  fc
}

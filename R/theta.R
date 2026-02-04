# Implement standard Theta method of Assimakopoulos and Nikolopoulos (2000)
# More general methods are available in the forecTheta package

# Author: RJH

#' Theta model
#'
#' The theta method of Assimakopoulos and Nikolopoulos (2000) is equivalent to
#' simple exponential smoothing with drift (Hyndman and Billah, 2003).
#' This function fits the theta model to a time series.
#' The series is tested for seasonality using the test outlined in A&N. If
#' deemed seasonal, the series is seasonally adjusted using a classical
#' multiplicative decomposition before fitting the theta model.
#'
#' More general theta methods are available in the \CRANpkg{forecTheta}
#' package.
#'
#' @inheritParams ets
#' @return An object of class `theta_model`.
#' @author Rob J Hyndman
#' @seealso [thetaf()]
#' @references Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model:
#' a decomposition approach to forecasting. \emph{International Journal of
#' Forecasting} \bold{16}, 521-530.
#'
#' Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method.
#' \emph{International J. Forecasting}, \bold{19}, 287-290.
#' @keywords ts
#' @examples
#' nile_fit <- theta_model(Nile)
#' forecast(nile_fit) |> autoplot()
#' @export
theta_model <- function(y, lambda = NULL, biasadj = FALSE) {
  series <- deparse1(substitute(y))
  n <- length(y)
  origy <- y

  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }
  # Seasonal decomposition
  m <- frequency(y)
  if (m > 1 && !is.constant(y) && n > 2 * m) {
    r <- as.numeric(acf(y, lag.max = m, plot = FALSE)$acf)[-1]
    stat <- sqrt((1 + 2 * sum(r[-m]^2)) / n)
    seasonal <- (abs(r[m]) / stat > qnorm(0.95))
  } else {
    seasonal <- FALSE
  }
  if (seasonal) {
    decomp <- decompose(y, type = "multiplicative")
    if (any(abs(seasonal(decomp)) < 1e-4)) {
      warning("Seasonal indexes close to zero. Using non-seasonal Theta method")
      seasonal <- FALSE
    } else {
      y <- seasadj(decomp)
      seas_component <- decomp$seasonal
    }
  }

  # Find parameters
  beta <- lsfit(0:(n - 1), y)$coefficients[2]
  ses_model <- ets(y, model = "ANN", opt.crit = "mse")

  # Fitted values and residuals
  fitted <- fitted(ses_model)
  res <- y - fitted
  if (seasonal) {
    fitted <- fitted * seas_component
  }
  if (!is.null(lambda)) {
    fitted <- InvBoxCox(fitted, lambda, biasadj, var(res))
    res <- y - fitted
  }

  # Return results
  structure(
    list(
      y = origy,
      series = series,
      ses_model = ses_model,
      alpha = pmax(1e-10, ses_model$par["alpha"]),
      drift = beta / 2,
      sigma2 = ses_model$sigma2,
      fitted = fitted,
      residuals = origy - fitted,
      seas_component = if (seasonal) tail(seas_component, m) else NULL,
      lambda = lambda,
      call = match.call()
    ),
    class = c("fc_model", "theta_model")
  )
}

#' @export
print.theta_model <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("Theta model: ")
  cat(x$series, "\n")
  cat("Call:", deparse(x$call), "\n")
  if (!is.null(x$seas_component)) {
    cat("Deseasonalized\n")
  }
  cat("  alpha:", format(x$alpha, digits = digits), "\n")
  cat("  drift:", format(x$drift, digits = digits), "\n")
  cat("  sigma^2:", format(x$sigma2, digits = digits), "\n")
  invisible(x)
}

#' Theta method forecasts.
#'
#' Returns forecasts and prediction intervals for a theta method forecast.
#' `thetaf()` is a convenience function that combines `theta_model()` and
#' `forecast.theta_model()`.
#' The theta method of Assimakopoulos and Nikolopoulos (2000) is equivalent to
#' simple exponential smoothing with drift (Hyndman and Billah, 2003).
#' The series is tested for seasonality using the test outlined in A&N. If
#' deemed seasonal, the series is seasonally adjusted using a classical
#' multiplicative decomposition before applying the theta method. The resulting
#' forecasts are then reseasonalized.
#' Prediction intervals are computed using the underlying state space model.
#'
#' More general theta methods are available in the \CRANpkg{forecTheta}
#' package.
#'
#' @param object An object of class `theta_model` created by [theta_model()].
#' @inheritParams ses
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [stats::arima()], [meanf()], [rwf()], [ses()]
#' @references Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model:
#' a decomposition approach to forecasting. \emph{International Journal of
#' Forecasting} \bold{16}, 521-530.
#'
#' Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method.
#' \emph{International J. Forecasting}, \bold{19}, 287-290.
#' @keywords ts
#' @examples
#' nile_fit <- theta_model(Nile)
#' forecast(nile_fit) |> autoplot()
#' @export
forecast.theta_model <- function(
  object,
  h = if (frequency(object$y) > 1) 2 * frequency(object$y) else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = FALSE,
  ...
) {
  # Check inputs
  level <- getConfLevel(level, fan)
  seasonal <- !is.null(object$seas_component)
  m <- frequency(object$y)
  n <- length(object$y)

  fcast <- forecast(object$ses_model, h = h, level = level, fan = fan, ...)
  fcast$mean <- fcast$mean +
    object$drift * (seq(h) - 1 + (1 - (1 - object$alpha)^n) / object$alpha)

  # Reseasonalize
  if (seasonal) {
    fcast$mean <- fcast$mean *
      rep(object$seas_component, trunc(1 + h / m))[seq(h)]
  }

  # Find prediction intervals
  fcast.se <- sqrt(object$sigma2) * sqrt((0:(h - 1)) * object$alpha^2 + 1)
  nconf <- length(level)
  fcast$lower <- fcast$upper <- ts(matrix(NA, nrow = h, ncol = nconf))
  tsp(fcast$lower) <- tsp(fcast$upper) <- tsp(fcast$mean)
  for (i in seq_len(nconf)) {
    zt <- -qnorm(0.5 - level[i] / 200)
    fcast$lower[, i] <- fcast$mean - zt * fcast.se
    fcast$upper[, i] <- fcast$mean + zt * fcast.se
  }

  # Back transform
  if (!is.null(lambda)) {
    fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast.se^2)
    fcast$upper <- InvBoxCox(fcast$upper, lambda)
    fcast$lower <- InvBoxCox(fcast$lower, lambda)
  }

  # Return results
  fcast$x <- object$y
  fcast$method <- "Theta"
  fcast$model <- object
  fcast$series <- object$series
  fcast$fitted <- object$fitted
  fcast
}

#' @rdname forecast.theta_model
#' @export
thetaf <- function(
  y,
  h = if (frequency(y) > 1) 2 * frequency(y) else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
) {
  fit <- theta_model(x, lambda = lambda, biasadj = biasadj)
  forecast(fit, h = h, level = level, fan = fan, ...)
}

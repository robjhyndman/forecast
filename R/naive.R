# Random walk related forecasts
# Based on lagged walks
# lag=1 corresponds to standard random walk (i.e., naive forecast)
# lag=m corresponds to seasonal naive method

lagwalk <- function(y, lag=1, h=10, drift=FALSE,
                    level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=FALSE, bootstrap=FALSE, npaths=5000) {
  if (!is.null(lambda)) {
    origy <- y
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
  }

  # Fit equivalent ARIMA model
  # This handles missing values properly
  if (lag == 1L) {
    fit <- Arima(y, c(0, 1, 0), include.constant = drift)
  } else {
    fit <- Arima(y, seasonal = list(order = c(0, 1, 0), period = lag), include.constant = drift)
  }

  # Compute forecasts
  fc <- forecast(fit, h = h, level = level, bootstrap = bootstrap, npaths = npaths)

  # Adjust prediction intervals to allow for drift coefficient standard error
  if (drift) {
    b <- fit$coef["drift"]
    b.se <- sqrt(fit$var.coef[1, 1])
    fse <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * qnorm(.5 + level[1] / 200))
    ratio <- sqrt(fse^2 + (seq(h) * b.se)^2) / fse
    fc$upper <- fc$mean + (fc$upper - fc$mean) * ratio
    fc$lower <- fc$mean - (fc$mean - fc$lower) * ratio
  }
  else {
    b <- b.se <- 0
  }
  if (!is.null(lambda)) {
    fc$x <- origy
    fc$mean <- InvBoxCox(
      fc$mean, lambda, biasadj,
      list(level = fc$level, upper = fc$upper, lower = fc$lower)
    )
    fc$fitted <- InvBoxCox(fc$fitted, lambda)
    fc$upper <- InvBoxCox(fc$upper, lambda)
    fc$lower <- InvBoxCox(fc$lower, lambda)
  }

  # Remove initial fitted values and residuals
  fc$fitted[seq(lag)] <- NA
  fc$residuals[seq(lag)] <- NA

  fc$model <- structure(
    list(includedrift = drift, drift = b, drift.se = b.se, sd = sqrt(fit$sigma2)),
    class = "naive"
  )

  return(structure(fc, class = "forecast"))
}


# Random walk
#' @rdname naive
#'
#' @examples
#'
#' gold.fcast <- rwf(gold[1:60], h=50)
#' plot(gold.fcast)
#'
#' @export
rwf <- function(y, h=10, drift=FALSE, level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=FALSE,
                bootstrap=FALSE, npaths=5000, x=y) {
  fc <- lagwalk(
    x, lag = 1, h = h, drift = drift, level = level, fan = fan,
    lambda = lambda, biasadj = biasadj, bootstrap = bootstrap, npaths = npaths
  )
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))

  if (drift) {
    fc$method <- "Random walk with drift"
  } else {
    fc$method <- "Random walk"
  }
  return(fc)
}

#' Naive and Random Walk Forecasts
#'
#' \code{rwf()} returns forecasts and prediction intervals for a random walk
#' with drift model applied to \code{y}. This is equivalent to an ARIMA(0,1,0)
#' model with an optional drift coefficient. \code{naive()} is simply a wrapper
#' to \code{rwf()} for simplicity. \code{snaive()} returns forecasts and
#' prediction intervals from an ARIMA(0,0,0)(0,1,0)m model where m is the
#' seasonal period.
#'
#' The random walk with drift model is \deqn{Y_t=c + Y_{t-1} + Z_t}{Y[t]=c +
#' Y[t-1] + Z[t]} where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are
#' given by \deqn{Y_n(h)=ch+Y_n}{Y[n+h]=ch+Y[n]}. If there is no drift (as in
#' \code{naive}), the drift parameter c=0. Forecast standard errors allow for
#' uncertainty in estimating the drift parameter (unlike the corresponding
#' forecasts obtained by fitting an ARIMA model directly).
#'
#' The seasonal naive model is \deqn{Y_t= Y_{t-m} + Z_t}{Y[t]=Y[t-m] + Z[t]}
#' where \eqn{Z_t}{Z[t]} is a normal iid error.
#'
#' @aliases print.naive
#'
#' @param y a numeric vector or time series of class \code{ts}
#' @param h Number of periods for forecasting
#' @param drift Logical flag. If TRUE, fits a random walk with drift model.
#' @param level Confidence levels for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param bootstrap If TRUE, use a bootstrap method to compute prediction intervals.
#' Otherwise, assume a normal distribution.
#' @param npaths Number of bootstrapped sample paths to use if \code{bootstrap==TRUE}.
#' @param x Deprecated. Included for backwards compatibility.
#' @inheritParams forecast
#' 
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{naive} or
#' \code{snaive}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
#' That is x minus fitted values.} \item{fitted}{Fitted values (one-step
#' forecasts)}
#' @author Rob J Hyndman
#' @seealso \code{\link{Arima}}
#' @keywords ts
#' @examples
#'
#' plot(naive(gold,h=50),include=200)
#'
#' @export
naive <- function(y, h=10, level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=FALSE,
                  bootstrap=FALSE, npaths=5000, x=y) {
  fc <- rwf(
    x, h = h, level = level, fan = fan, lambda = lambda, drift = FALSE,
    biasadj = biasadj, bootstrap = bootstrap, npaths = npaths
  )
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))
  fc$method <- "Naive method"
  return(fc)
}

#' @rdname naive
#'
#' @examples
#'
#' plot(snaive(wineind))
#'
#' @export
snaive <- function(y, h=2 * frequency(x), level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=FALSE,
                   bootstrap=FALSE, npaths=5000, x=y) {
  fc <- lagwalk(
    x, lag = frequency(x), h = h, drift = FALSE, level = level, fan = fan,
    lambda = lambda, biasadj = biasadj, bootstrap = bootstrap, npaths = npaths
  )
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))
  fc$method <- "Seasonal naive method"
  return(fc)
}

#' @export
print.naive <- function(x, ...) {
  cat(paste("Call:", deparse(x$call), "\n\n"))
  if (x$includedrift) {
    cat(paste("Drift: ", round(x$drift, 4), "  (se ", round(x$drift.se, 4), ")\n", sep = ""))
  }
  cat(paste("Residual sd:", round(x$sd, 4), "\n"))
}

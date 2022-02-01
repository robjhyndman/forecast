# Random walk related forecasts
# Based on lagged walks
# lag=1 corresponds to standard random walk (i.e., naive forecast)
# lag=m corresponds to seasonal naive method

lagwalk <- function(y, lag=1, drift=FALSE, lambda=NULL, biasadj=FALSE) {
  if(!is.ts(y)){
    y <- as.ts(y)
  }
  origy <- y
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
  }

  m <- frequency(y)
  # Complete missing values with lagged values
  y_na <- which(is.na(y))
  y_na <- y_na[y_na>lag]
  fits <- stats::lag(y, -lag)
  for(i in y_na){
    if(is.na(fits)[i]){
      fits[i] <- fits[i-lag]
    }
  }

  fitted <- ts(c(rep(NA, lag), head(fits, -lag)), start = start(y), frequency = m)
  fitted <- copy_msts(y, fitted)
  if(drift){
    fit <- summary(lm(y-fitted ~ 1, na.action=na.exclude))
    b <- fit$coefficients[1,1]
    b.se <- fit$coefficients[1,2]
    sigma <- fit$sigma
    fitted <- fitted + b
    res <- y - fitted
    method <- "Lag walk with drift"
  }
  else{
    res <- y - fitted
    b <- b.se <- 0
    sigma <- sqrt(mean(res^2, na.rm=TRUE))
    method <- "Lag walk"
  }

  if (!is.null(lambda)) {
    fitted <- InvBoxCox(fitted, lambda, biasadj, var(res))
    attr(lambda, "biasadj") <- biasadj
  }

  model <- structure(
    list(
      x = origy,
      fitted = fitted,
      future = tail(fits, lag),
      residuals = res,
      method = method,
      series = deparse(substitute(y)),
      sigma2 = sigma^2,
      par = list(includedrift = drift, drift = b, drift.se = b.se, lag = lag),
      lambda = lambda,
      call = match.call()
    ),
    class = "lagwalk"
  )
}

#' @export
forecast.lagwalk <- function(object, h=10, level=c(80, 95), fan=FALSE, lambda=NULL,
    simulate=FALSE, bootstrap=FALSE, npaths=5000, biasadj=FALSE, ...) {
  lag <- object$par$lag
  fullperiods <- (h-1)/lag+1
  steps <- rep(1:fullperiods, rep(lag,fullperiods))[1:h]

  # Point forecasts
  fc <- rep(object$future, fullperiods)[1:h] + steps*object$par$drift

  # Intervals
  # Adjust prediction intervals to allow for drift coefficient standard error
  mse <- mean(object$residuals^2, na.rm=TRUE)
  se <- sqrt(mse*steps + (steps*object$par$drift.se)^2)

  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }

  nconf <- length(level)

  if (simulate | bootstrap) # Compute prediction intervals using simulations
  {
    sim <- matrix(NA, nrow = npaths, ncol = h)
    for (i in 1:npaths)
      sim[i, ] <- simulate(object, nsim = h, bootstrap = bootstrap, lambda = lambda)
    lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8)
    upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8)
    if (nconf > 1L) {
      lower <- t(lower)
      upper <- t(upper)
    }
    else {
      lower <- matrix(lower, ncol = 1)
      upper <- matrix(upper, ncol = 1)
    }
  }
  else {
    z <- qnorm(.5 + level/200)
    lower <- upper <- matrix(NA,nrow=h,ncol=nconf)
    for(i in 1:nconf)
    {
      lower[,i] <- fc - z[i]*se
      upper[,i] <- fc + z[i]*se
    }
  }

  if (!is.null(lambda)) {
    fc <- InvBoxCox(fc, lambda, biasadj, se^2)
    if(!bootstrap){ # Bootstrap intervals are already backtransformed
      upper <- InvBoxCox(upper, lambda)
      lower <- InvBoxCox(lower, lambda)
    }
  }

  # Set attributes
  fc <- future_msts(object$x, fc)
  lower <- future_msts(object$x, lower)
  upper <- future_msts(object$x, upper)
  colnames(lower) <- colnames(upper) <- paste(level,"%",sep="")

  return(structure(
    list(
      method = object$method, model = object, lambda = lambda, x = object$x,
      fitted = fitted(object), residuals = residuals(object), series = object$series,
      mean = fc, level = level, lower = lower, upper = upper
    ), class = "forecast")
  )
}

#' @export
print.lagwalk <- function(x, ...) {
  cat(paste("Call:", deparse(x$call), "\n\n"))
  if (x$par$includedrift) {
    cat(paste("Drift: ", round(x$par$drift, 4), "  (se ", round(x$par$drift.se, 4), ")\n", sep = ""))
  }
  cat(paste("Residual sd:", round(sqrt(x$sigma2), 4), "\n"))
}

#' @export
fitted.lagwalk <- function(object, ...){
  object$fitted
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
                ..., x=y) {
  fit <- lagwalk(
    x, lag = 1, drift = drift,
    lambda = lambda, biasadj = biasadj
  )

  fc <- forecast(fit, h = h,
                 level = level, fan = fan,
                 lambda = fit$lambda, biasadj = biasadj, ...)

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
#' @param x Deprecated. Included for backwards compatibility.
#' @inheritParams forecast.ts
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
                  ..., x=y) {
  fc <- rwf(
    x, h = h, level = level, fan = fan, lambda = lambda, drift = FALSE,
    biasadj = biasadj, ...
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
                   ..., x=y) {
  fit <- lagwalk(
    x, lag = frequency(x), drift = FALSE,
    lambda = lambda, biasadj = biasadj
  )
  fc <- forecast(fit, h = h,
                 level = level, fan = fan,
                 lambda = fit$lambda, biasadj = biasadj, ...)
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))
  fc$method <- "Seasonal naive method"
  return(fc)
}

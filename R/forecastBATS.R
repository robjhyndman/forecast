#' Forecasting using BATS and TBATS models
#'
#' Forecasts \code{h} steps ahead with a BATS model. Prediction intervals are
#' also produced.
#'
#' @param object An object of class "\code{bats}". Usually the result of a call
#' to \code{\link{bats}}.
#' @param h Number of periods for forecasting. Default value is twice the
#' largest seasonal period (for seasonal data) or ten (for non-seasonal data).
#' @param level Confidence level for prediction intervals.
#' @param fan If TRUE, level is set to \code{seq(51,99,by=3)}. This is suitable
#' for fan plots.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param ... Other arguments, currently ignored.
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{forecast.bats}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A copy of the \code{bats} object}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a time series} \item{lower}{Lower limits for
#' prediction intervals} \item{upper}{Upper limits for prediction intervals}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{x}{The original time series (either \code{object} itself or the time
#' series used to create the model stored as \code{object}).}
#' \item{residuals}{Residuals from the fitted model.} \item{fitted}{Fitted
#' values (one-step forecasts)}
#' @author Slava Razbash and Rob J Hyndman
#' @seealso \code{\link{bats}}, \code{\link{tbats}},\code{\link{forecast.ets}}.
#' @references De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011),
#' Forecasting time series with complex seasonal patterns using exponential
#' smoothing, \emph{Journal of the American Statistical Association},
#' \bold{106}(496), 1513-1527.
#' @keywords ts
#' @examples
#'
#' \dontrun{
#' fit <- bats(USAccDeaths)
#' plot(forecast(fit))
#'
#' taylor.fit <- bats(taylor)
#' plot(forecast(taylor.fit))
#' }
#'
#' @export
forecast.bats <- function(object, h, level=c(80, 95), fan=FALSE, biasadj=NULL, ...) {
  # Set up the variables
  if (any(class(object$y) == "ts")) {
    ts.frequency <- frequency(object$y)
  } else {
    ts.frequency <- ifelse(!is.null(object$seasonal.periods), max(object$seasonal.periods), 1)
  }

  if (missing(h)) {
    if (is.null(object$seasonal.periods)) {
      h <- ifelse(ts.frequency == 1, 10, 2 * ts.frequency)
    } else {
      h <- 2 * max(object$seasonal.periods)
    }
  }
  else if (h <= 0) {
    stop("Forecast horizon out of bounds")
  }

  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }

  # Set up the matrices
  x <- matrix(0, nrow = nrow(object$x), ncol = h)
  y.forecast <- numeric(h)
  # w <- makeWMatrix(small.phi=object$damping.parameter, seasonal.periods=object$seasonal.periods, ar.coefs=object$ar.coefficients, ma.coefs=object$ma.coefficients)
  w <- .Call("makeBATSWMatrix", smallPhi_s = object$damping.parameter, sPeriods_s = object$seasonal.periods, arCoefs_s = object$ar.coefficients, maCoefs_s = object$ma.coefficients, PACKAGE = "forecast")
  # g <- makeGMatrix(alpha=object$alpha, beta=object$beta, gamma.vector=object$gamma.values, seasonal.periods=object$seasonal.periods, p=length(object$ar.coefficients), q=length(object$ma.coefficients))
  g <- .Call("makeBATSGMatrix", object$alpha, object$beta, object$gamma.values, object$seasonal.periods, length(object$ar.coefficients), length(object$ma.coefficients), PACKAGE = "forecast")

  F <- makeFMatrix(alpha = object$alpha, beta = object$beta, small.phi = object$damping.parameter, seasonal.periods = object$seasonal.periods, gamma.bold.matrix = g$gamma.bold.matrix, ar.coefs = object$ar.coefficients, ma.coefs = object$ma.coefficients)

  # Do the forecast
  y.forecast[1] <- w$w.transpose %*% object$x[, ncol(object$x)]
  x[, 1] <- F %*% object$x[, ncol(object$x)] # + g$g %*% object$errors[length(object$errors)]

  if (h > 1) {
    for (t in 2:h) {
      x[, t] <- F %*% x[, (t - 1)]
      y.forecast[t] <- w$w.transpose %*% x[, (t - 1)]
    }
  }
  ## Make prediction intervals here
  lower.bounds <- upper.bounds <- matrix(NA, ncol = length(level), nrow = h)
  variance.multiplier <- numeric(h)
  variance.multiplier[1] <- 1
  if (h > 1) {
    for (j in 1:(h - 1)) {
      if (j == 1) {
        f.running <- diag(ncol(F))
      } else {
        f.running <- f.running %*% F
      }
      c.j <- w$w.transpose %*% f.running %*% g$g
      variance.multiplier[(j + 1)] <- variance.multiplier[j] + c.j ^ 2
    }
  }

  variance <- object$variance * variance.multiplier
  # print(variance)
  st.dev <- sqrt(variance)
  for (i in 1:length(level)) {
    marg.error <- st.dev * abs(qnorm((100 - level[i]) / 200))
    lower.bounds[, i] <- y.forecast - marg.error
    upper.bounds[, i] <- y.forecast + marg.error
  }
  # Inv Box Cox transform if required
  if (!is.null(object$lambda)) {
    y.forecast <- InvBoxCox(y.forecast, object$lambda, biasadj, list(level = level, upper = upper.bounds, lower = lower.bounds))
    lower.bounds <- InvBoxCox(lower.bounds, object$lambda)
    if (object$lambda < 1) {
      lower.bounds <- pmax(lower.bounds, 0)
    }
    upper.bounds <- InvBoxCox(upper.bounds, object$lambda)
  }
  colnames(upper.bounds) <- colnames(lower.bounds) <- paste0(level, "%")

  forecast.object <- list(
    model = object, mean = future_msts(object$y, y.forecast),
    level = level, x = object$y, series = object$series,
    upper = future_msts(object$y, upper.bounds),
    lower = future_msts(object$y, lower.bounds),
    fitted = copy_msts(object$y, object$fitted.values),
    method = as.character(object),
    residuals = copy_msts(object$y, object$errors)
  )
  if (is.null(object$series)) {
    forecast.object$series <- deparse(object$call$y)
  }
  class(forecast.object) <- "forecast"
  return(forecast.object)
}


#' @export
as.character.bats <- function(x, ...) {
  name <- "BATS("
  if (!is.null(x$lambda)) {
    name <- paste(name, round(x$lambda, digits = 3), sep = "")
  } else {
    name <- paste(name, "1", sep = "")
  }
  name <- paste(name, ", {", sep = "")
  if (!is.null(x$ar.coefficients)) {
    name <- paste(name, length(x$ar.coefficients), sep = "")
  } else {
    name <- paste(name, "0", sep = "")
  }
  name <- paste(name, ",", sep = "")
  if (!is.null(x$ma.coefficients)) {
    name <- paste(name, length(x$ma.coefficients), sep = "")
  } else {
    name <- paste(name, "0", sep = "")
  }
  name <- paste(name, "}, ", sep = "")
  if (!is.null(x$damping.parameter)) {
    name <- paste(name, round(x$damping.parameter, digits = 3), sep = "")
  } else {
    name <- paste(name, "-", sep = "")
  }
  name <- paste(name, ", ", sep = "")
  if (!is.null(x$seasonal.periods)) {
    name <- paste(name, "{", sep = "")
    for (i in x$seasonal.periods) {
      name <- paste(name, i, sep = "")
      if (i != x$seasonal.periods[length(x$seasonal.periods)]) {
        name <- paste(name, ",", sep = "")
      } else {
        name <- paste(name, "})", sep = "")
      }
    }
  } else {
    name <- paste(name, "-)", sep = "")
  }
  return(name)
}

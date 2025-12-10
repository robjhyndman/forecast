##

#' Forecasting using a bagged model
#'
#' The bagged model forecasting method.
#'
#' This function implements the bagged model forecasting method described in
#' Bergmeir et al. By default, the [ets()] function is applied to all
#' bootstrapped series. Base models other than [ets()] can be given by the
#' parameter `fn`. Using the default parameters, the function
#' [bld.mbb.bootstrap()] is used to calculate the bootstrapped series
#' with the Box-Cox and Loess-based decomposition (BLD) bootstrap. The function
#' [forecast.baggedModel()] can then be used to calculate forecasts.
#'
#' `baggedETS` is a wrapper for `baggedModel`, setting `fn` to "ets".
#' This function is included for backwards compatibility only, and may be
#' deprecated in the future.
#'
#' @aliases print.baggedModel
#'
#' @param y A numeric vector or univariate time series of class `ts`.
#' @param bootstrapped_series bootstrapped versions of y.
#' @param fn the forecast function to use. Default is [ets()].
#' @param ... Other arguments passed to the forecast function.
#' @return Returns an object of class `baggedModel`.
#'
#' The function `print` is used to obtain and print a summary of the
#' results.
#'
#' \item{models}{A list containing the fitted ensemble models.}
#' \item{method}{The function for producing a forecastable model.}
#' \item{y}{The original time series.}
#' \item{bootstrapped_series}{The bootstrapped series.}
#' \item{modelargs}{The arguments passed through to `fn`.}
#' \item{fitted}{Fitted values (one-step forecasts). The
#' mean of the fitted values is calculated over the ensemble.}
#' \item{residuals}{Original values minus fitted values.}
#' @author Christoph Bergmeir, Fotios Petropoulos
#' @references Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
#' Exponential Smoothing Methods using STL Decomposition and Box-Cox
#' Transformation. International Journal of Forecasting 32, 303-312.
#' @keywords ts
#' @examples
#' fit <- baggedModel(WWWusage)
#' fcast <- forecast(fit)
#' plot(fcast)
#'
#' @export
baggedModel <- function(
  y,
  bootstrapped_series = bld.mbb.bootstrap(y, 100),
  fn = ets,
  ...
) {
  # Add package info in case forecast not loaded
  if (!is.function(fn)) {
    warning(
      "Using character specification for `fn` is deprecated. Please use `fn = ",
      match.arg(fn, c("ets", "auto.arima")),
      "`."
    )
    fn <- utils::getFromNamespace(
      match.arg(fn, c("ets", "auto.arima")),
      "forecast"
    )
  }

  mod_boot <- lapply(bootstrapped_series, function(x) fn(x, ...))

  # Return results
  out <- list()
  out$y <- as.ts(y)
  out$bootstrapped_series <- bootstrapped_series
  out$models <- mod_boot

  out$modelargs <- list(...)

  fitted_boot <- lapply(out$models, fitted)
  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
  out$fitted <- ts(rowMeans(fitted_boot))
  tsp(out$fitted) <- tsp(out$y)
  out$residuals <- out$y - out$fitted

  out$series <- deparse1(substitute(y))
  out$method <- "baggedModel"

  out$call <- match.call()
  structure(out, class = c("fc_model", "baggedModel"))
}

#' @rdname baggedModel
#' @export
baggedETS <- function(y, bootstrapped_series = bld.mbb.bootstrap(y, 100), ...) {
  out <- baggedModel(y, bootstrapped_series, fn = ets, ...)
  class(out) <- c("baggedETS", class(out))
  out
}

#' Forecasting using a bagged model
#'
#' Returns forecasts and other information for bagged models.
#'
#' Intervals are calculated as min and max values over the point forecasts from
#' the models in the ensemble. I.e., the intervals are not prediction
#' intervals, but give an indication of how different the forecasts within the
#' ensemble are.
#'
#' @inheritParams forecast.ts
#' @param object An object of class `baggedModel` resulting from a call to
#' [baggedModel()].
#' @param ... Other arguments, passed on to the [forecast()] function of the original method
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Christoph Bergmeir, Fotios Petropoulos
#' @seealso [baggedModel()].
#' @references Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
#' Exponential Smoothing Methods using STL Decomposition and Box-Cox
#' Transformation. International Journal of Forecasting 32, 303-312.
#' @keywords ts
#' @examples
#' fit <- baggedModel(WWWusage)
#' fcast <- forecast(fit)
#' plot(fcast)
#'
#' \dontrun{
#' fit2 <- baggedModel(WWWusage, fn = "auto.arima")
#' fcast2 <- forecast(fit2)
#' plot(fcast2)
#' accuracy(fcast2)
#' }
#'
#' @export
forecast.baggedModel <- function(
  object,
  h = if (frequency(object$y) > 1) 2 * frequency(object$y) else 10,
  ...
) {
  out <- list(
    model = object,
    series = object$series,
    x = object$y,
    method = object$method,
    fitted = object$fitted,
    residuals = object$residuals
  )
  # out <- object
  tspx <- tsp(out$x)

  forecasts_boot <- lapply(out$model$models, function(mod) {
    if (inherits(mod, "ets")) {
      forecast(mod, PI = FALSE, h = h, ...)$mean
    } else {
      forecast(mod, h = h, ...)$mean
    }
  })

  forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
  colnames(forecasts_boot) <- NULL

  if (!is.null(tspx)) {
    start.f <- tspx[2] + 1 / frequency(out$x)
  } else {
    start.f <- length(out$x) + 1
  }

  # out <- list()
  out$forecasts_boot <- forecasts_boot

  #  browser()
  #  out$model$models

  out$mean <- ts(
    rowMeans(forecasts_boot),
    frequency = frequency(out$x),
    start = start.f
  )
  out$median <- ts(apply(forecasts_boot, 1, median))
  out$lower <- ts(apply(forecasts_boot, 1, min))
  out$upper <- ts(apply(forecasts_boot, 1, max))
  out$level <- 100

  tsp(out$median) <- tsp(out$lower) <- tsp(out$upper) <- tsp(out$mean)

  class(out) <- "forecast"
  out
}

# fitted.baggedModel <- function(object, h=1, accum_func=mean, ...){
#
#  fitted_boot <- lapply(object$models, fitted, h)
#  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
#  fitted_boot <- apply(fitted_boot, 2, accum_func)
#  fitted_boot
# }

# residuals.baggedModel <- function(object, h=1, ...){
#
#  residuals_boot <- lapply(object$models, residuals, h)
#  residuals_boot <- as.matrix(as.data.frame(residuals_boot))
#  residuals_boot
#
#  #Alternative implementation:
#  #object$x - fitted(object, h)
# }

#' @export
print.baggedModel <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  cat("Call:   ")
  print(x$call)

  # print(x$model)
  # cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits), "\n", sep = "")

  invisible(x)
}

#' @rdname is.ets
#' @export
is.baggedModel <- function(x) {
  inherits(x, "baggedModel")
}

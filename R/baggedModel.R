##



#' Forecasting using a bagged model
#'
#' The bagged model forecasting method.
#'
#' This function implements the bagged model forecasting method described in
#' Bergmeir et al. By default, the \code{\link{ets}} function is applied to all
#' bootstrapped series. Base models other than \code{\link{ets}} can be given by the
#' parameter \code{fn}. Using the default parameters, the function
#' \code{\link{bld.mbb.bootstrap}} is used to calculate the bootstrapped series
#' with the Box-Cox and Loess-based decomposition (BLD) bootstrap. The function
#' \code{\link{forecast.baggedModel}} can then be used to calculate forecasts.
#'
#' \code{baggedETS} is a wrapper for \code{baggedModel}, setting \code{fn} to "ets".
#' This function is included for backwards compatibility only, and may be
#' deprecated in the future.
#'
#' @aliases print.baggedModel
#'
#' @param y A numeric vector or time series of class \code{ts}.
#' @param bootstrapped_series bootstrapped versions of y.
#' @param fn the forecast function to use. Default is \code{\link{ets}}
#' @param \dots Other arguments passed to the forecast function.
#' @return Returns an object of class "\code{baggedModel}".
#'
#' The function \code{print} is used to obtain and print a summary of the
#' results.
#'
#' \item{models}{A list containing the fitted ensemble models.}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{y}{The original time series.}
#' \item{bootstrapped_series}{The bootstrapped series.}
#' \item{modelargs}{The arguments passed through to \code{fn}.}
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
baggedModel <- function(y, bootstrapped_series=bld.mbb.bootstrap(y, 100), fn=c("ets", "auto.arima"), ...) {
  # Add package info in case forecast not loaded
  fn <- utils::getFromNamespace(match.arg(fn), "forecast")

  mod_boot <- lapply(bootstrapped_series, function(x) {
    mod <- fn(x, ...)
  })

  # Return results
  out <- list()
  out$y <- as.ts(y)
  out$bootstrapped_series <- bootstrapped_series
  out$models <- mod_boot

  out$modelargs <- list(...)

  fitted_boot <- lapply(out$models, fitted)
  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
  out$fitted <- ts(apply(fitted_boot, 1, mean))
  tsp(out$fitted) <- tsp(out$y)
  out$residuals <- out$y - out$fitted

  out$series <- deparse(substitute(y))
  out$method <- "baggedModel"

  out$call <- match.call()
  return(structure(out, class = c("baggedModel")))
}


#' @rdname baggedModel
#' @export
baggedETS <- function(y, bootstrapped_series=bld.mbb.bootstrap(y, 100), ...) {
  out <- baggedModel(y, bootstrapped_series, fn = "ets", ...)
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
#' @param object An object of class "\code{baggedModel}" resulting from a call to
#' \code{\link{baggedModel}}.
#' @param h Number of periods for forecasting.
#' @param ... Other arguments, passed on to the \code{\link{forecast}} function of the original method
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' An object of class "\code{forecast}" is a list containing at least the
#' following elements:
#' \item{model}{A list containing information about the fitted model}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals}
#' \item{upper}{Upper limits for prediction intervals}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{x}{The original time series (either \code{object} itself or the
#' time series used to create the model stored as \code{object}).}
#' \item{xreg}{The external regressors used in fitting (if given).}
#' \item{residuals}{Residuals from the fitted model. That
#' is x minus fitted values.}
#' \item{fitted}{Fitted values (one-step forecasts)}
#' @author Christoph Bergmeir, Fotios Petropoulos
#' @seealso \code{\link{baggedModel}}.
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
#' fit2 <- baggedModel(WWWusage, fn="auto.arima")
#' fcast2 <- forecast(fit2)
#' plot(fcast2)
#' accuracy(fcast2)}
#'
#' @export
forecast.baggedModel <- function(object, h=ifelse(frequency(object$x) > 1, 2 * frequency(object$x), 10), ...) {
  out <- list(
    model = object, series = object$series, x = object$y, method = object$method, fitted = object$fitted,
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

  out$mean <- ts(apply(forecasts_boot, 1, mean), frequency = frequency(out$x), start = start.f)
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
print.baggedModel <- function(x, digits = max(3, getOption("digits") - 3), ...) {
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

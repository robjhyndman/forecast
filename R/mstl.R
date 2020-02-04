#' Multiple seasonal decomposition
#'
#' Decompose a time series into seasonal, trend and remainder components.
#' Seasonal components are estimated iteratively using STL. Multiple seasonal periods are
#' allowed. The trend component is computed for the last iteration of STL.
#' Non-seasonal time series are decomposed into trend and remainder only.
#' In this case, \code{\link[stats]{supsmu}} is used to estimate the trend.
#' Optionally, the time series may be Box-Cox transformed before decomposition.
#' Unlike \code{\link[stats]{stl}}, \code{mstl} is completely automated.
#' @param x Univariate time series of class \code{msts} or \code{ts}.
#' @param iterate Number of iterations to use to refine the seasonal component.
#' @param s.window Seasonal windows to be used in the  decompositions. If scalar,
#' the same value is used for all seasonal components. Otherwise, it should be a vector
#' of the same length as the number of seasonal components.
#' @param ... Other arguments are passed to \code{\link[stats]{stl}}.
#' @inheritParams forecast
#'
#' @seealso \code{\link[stats]{stl}}, \code{\link[stats]{supsmu}}
#' @examples
#' library(ggplot2)
#' mstl(taylor) %>% autoplot()
#' mstl(AirPassengers, lambda = "auto") %>% autoplot()
#' @export
mstl <- function(x, lambda = NULL, iterate = 2, s.window = 13, ...) {
  # What is x?
  origx <- x
  n <- length(x)
  if ("msts" %in% class(x)) {
    msts <- attributes(x)$msts
    if (any(msts >= n / 2)) {
      warning("Dropping seasonal components with fewer than two full periods.")
      msts <- msts[msts < n / 2]
      x <- forecast::msts(x, seasonal.periods = msts)
    }
    msts <- sort(msts, decreasing = FALSE)
  }
  else if ("ts" %in% class(x)) {
    msts <- frequency(x)
    iterate <- 1L
  }
  else {
    x <- as.ts(x)
    msts <- 1L
  }
  # Check dimension
  if (!is.null(dim(x))) {
    if (NCOL(x) == 1L) {
      x <- x[, 1]
    }
  }

  # Replace missing values if necessary
  if (anyNA(x)) {
    x <- na.interp(x, lambda = lambda)
  }

  # Transform if necessary
  if (!is.null(lambda)) {
    x <- forecast::BoxCox(x, lambda = lambda)
    lambda <- attr(x, "lambda")
  }
  tt <- seq_len(n)

  # Now fit stl models with only one type of seasonality at a time
  if (msts[1L] > 1) {
    seas <- as.list(rep(0, length(msts)))
    deseas <- x
    if (length(s.window) == 1L) {
      s.window <- rep(s.window, length(msts))
    }
    iterate <- pmax(1L, iterate)
    for (j in seq_len(iterate))
    {
      for (i in seq_along(msts))
      {
        deseas <- deseas + seas[[i]]
        fit <- stl(ts(deseas, frequency = msts[i]), s.window = s.window[i], ...)
        seas[[i]] <- msts(seasonal(fit), seasonal.periods = msts)
        attributes(seas[[i]]) <- attributes(x)
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- msts(trendcycle(fit), seasonal.periods = msts)
  }
  else {
    msts <- NULL
    deseas <- x
    trend <- ts(stats::supsmu(seq_len(n), x)$y)
  }
  attributes(trend) <- attributes(x)

  # Put back NAs
  deseas[is.na(origx)] <- NA

  # Estimate remainder
  remainder <- deseas - trend

  # Package into matrix
  output <- cbind(origx, trend)
  if (!is.null(msts)) {
    for (i in seq_along(msts)) {
      output <- cbind(output, seas[[i]])
    }
  }
  output <- cbind(output, remainder)
  colnames(output)[1L:2L] <- c("Data", "Trend")
  if (!is.null(msts)) {
    colnames(output)[2L + seq_along(msts)] <- paste0("Seasonal", round(msts, 2))
  }
  colnames(output)[NCOL(output)] <- "Remainder"

  if (length(msts) > 1) {
    attr(output, "seasonal.periods") <- msts
    return(structure(output,
      seasonal.periods = msts,
      class = c("mstl", "mts", "msts", "ts")
    ))
  }

  return(structure(output, class = c("mstl", "mts", "ts")))
}

#' @rdname autoplot.seas
#' @export
autoplot.mstl <- function(object, ...) {
  autoplot.mts(object, facets = TRUE, ylab = "", ...)
}

#' Forecasting using stl objects
#'
#' Forecasts of STL objects are obtained by applying a non-seasonal forecasting
#' method to the seasonally adjusted data and re-seasonalizing using the last
#' year of the seasonal component.
#'
#' \code{stlm} takes a time series \code{y}, applies an STL decomposition, and
#' models the seasonally adjusted data using the model passed as
#' \code{modelfunction} or specified using \code{method}. It returns an object
#' that includes the original STL decomposition and a time series model fitted
#' to the seasonally adjusted data. This object can be passed to the
#' \code{forecast.stlm} for forecasting.
#'
#' \code{forecast.stlm} forecasts the seasonally adjusted data, then
#' re-seasonalizes the results by adding back the last year of the estimated
#' seasonal component.
#'
#' \code{stlf} combines \code{stlm} and \code{forecast.stlm}. It takes a
#' \code{ts} argument, applies an STL decomposition, models the seasonally
#' adjusted data, reseasonalizes, and returns the forecasts. However, it allows
#' more general forecasting methods to be specified via
#' \code{forecastfunction}.
#'
#' \code{forecast.stl} is similar to \code{stlf} except that it takes the STL
#' decomposition as the first argument, instead of the time series.
#'
#' Note that the prediction intervals ignore the uncertainty associated with
#' the seasonal component. They are computed using the prediction intervals
#' from the seasonally adjusted series, which are then reseasonalized using the
#' last year of the seasonal component. The uncertainty in the seasonal
#' component is ignored.
#'
#' The time series model for the seasonally adjusted data can be specified in
#' \code{stlm} using either \code{method} or \code{modelfunction}. The
#' \code{method} argument provides a shorthand way of specifying
#' \code{modelfunction} for a few special cases. More generally,
#' \code{modelfunction} can be any function with first argument a \code{ts}
#' object, that returns an object that can be passed to \code{\link{forecast}}.
#' For example, \code{forecastfunction=ar} uses the \code{\link{ar}} function
#' for modelling the seasonally adjusted series.
#'
#' The forecasting method for the seasonally adjusted data can be specified in
#' \code{stlf} and \code{forecast.stl} using either \code{method} or
#' \code{forecastfunction}. The \code{method} argument provides a shorthand way
#' of specifying \code{forecastfunction} for a few special cases. More
#' generally, \code{forecastfunction} can be any function with first argument a
#' \code{ts} object, and other \code{h} and \code{level}, which returns an
#' object of class \code{\link{forecast}}. For example,
#' \code{forecastfunction=thetaf} uses the \code{\link{thetaf}} function for
#' forecasting the seasonally adjusted series.
#'
#' @param y A univariate numeric time series of class \code{ts}
#' @param object An object of class \code{stl} or \code{stlm}. Usually the
#' result of a call to \code{\link[stats]{stl}} or \code{stlm}.
#' @param method Method to use for forecasting the seasonally adjusted series.
#' @param modelfunction An alternative way of specifying the function for
#' modelling the seasonally adjusted series. If \code{modelfunction} is not
#' \code{NULL}, then \code{method} is ignored. Otherwise \code{method} is used
#' to specify the time series model to be used.
#' @param model Output from a previous call to \code{stlm}. If a \code{stlm}
#' model is passed, this same model is fitted to y without re-estimating any
#' parameters.
#' @param forecastfunction An alternative way of specifying the function for
#' forecasting the seasonally adjusted series. If \code{forecastfunction} is
#' not \code{NULL}, then \code{method} is ignored. Otherwise \code{method} is
#' used to specify the forecasting method to be used.
#' @param etsmodel The ets model specification passed to
#' \code{\link[forecast]{ets}}. By default it allows any non-seasonal model. If
#' \code{method!="ets"}, this argument is ignored.
#' @param xreg Historical regressors to be used in
#' \code{\link[forecast]{auto.arima}()} when \code{method=="arima"}.
#' @param newxreg Future regressors to be used in
#' \code{\link[forecast]{forecast.Arima}()}.
#' @param h Number of periods for forecasting.
#' @param level Confidence level for prediction intervals.
#' @param fan If \code{TRUE}, level is set to seq(51,99,by=3). This is suitable
#' for fan plots.
#' @param s.window Either the character string ``periodic'' or the span (in
#' lags) of the loess window for seasonal extraction.
#' @param t.window A number to control the smoothness of the trend. See
#' \code{\link[stats]{stl}} for details.
#' @param robust If \code{TRUE}, robust fitting will used in the loess
#' procedure within \code{\link[stats]{stl}}.
#' @param allow.multiplicative.trend If TRUE, then ETS models with
#' multiplicative trends are allowed. Otherwise, only additive or no trend ETS
#' models are permitted.
#' @param x Deprecated. Included for backwards compatibility.
#' @param ... Other arguments passed to \code{forecast.stl},
#' \code{modelfunction} or \code{forecastfunction}.
#' @inheritParams forecast
#'
#' @return \code{stlm} returns an object of class \code{stlm}. The other
#' functions return objects of class \code{forecast}.
#'
#' There are many methods for working with \code{\link{forecast}} objects
#' including \code{summary} to obtain and print a summary of the results, while
#' \code{plot} produces a plot of the forecasts and prediction intervals. The
#' generic accessor functions \code{fitted.values} and \code{residuals} extract
#' useful features.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{stl}}, \code{\link{forecast.ets}},
#' \code{\link{forecast.Arima}}.
#' @keywords ts
#' @examples
#'
#' tsmod <- stlm(USAccDeaths, modelfunction = ar)
#' plot(forecast(tsmod, h = 36))
#'
#' decomp <- stl(USAccDeaths, s.window = "periodic")
#' plot(forecast(decomp))
#' @export
forecast.stl <- function(object, method = c("ets", "arima", "naive", "rwdrift"), etsmodel = "ZZN",
                         forecastfunction = NULL,
                         h = frequency(object$time.series) * 2, level = c(80, 95), fan = FALSE,
                         lambda = NULL, biasadj = NULL, xreg = NULL, newxreg = NULL, allow.multiplicative.trend = FALSE, ...) {
  method <- match.arg(method)
  if (is.null(forecastfunction)) {
    if (method != "arima" && (!is.null(xreg) || !is.null(newxreg))) {
      stop("xreg and newxreg arguments can only be used with ARIMA models")
    }
    if (method == "ets") {
      # Ensure non-seasonal model
      if (substr(etsmodel, 3, 3) != "N") {
        warning("The ETS model must be non-seasonal. I'm ignoring the seasonal component specified.")
        substr(etsmodel, 3, 3) <- "N"
      }
      forecastfunction <- function(x, h, level, ...) {
        fit <- ets(na.interp(x), model = etsmodel, allow.multiplicative.trend = allow.multiplicative.trend, ...)
        return(forecast(fit, h = h, level = level))
      }
    }
    else if (method == "arima") {
      forecastfunction <- function(x, h, level, ...) {
        fit <- auto.arima(x, xreg = xreg, seasonal = FALSE, ...)
        return(forecast(fit, h = h, level = level, xreg = newxreg))
      }
    }
    else if (method == "naive") {
      forecastfunction <- function(x, h, level, ...) {
        rwf(x, drift = FALSE, h = h, level = level, ...)
      }
    }
    else if (method == "rwdrift") {
      forecastfunction <- function(x, h, level, ...) {
        rwf(x, drift = TRUE, h = h, level = level, ...)
      }
    }
  }
  if (is.null(xreg) != is.null(newxreg)) {
    stop("xreg and newxreg arguments must both be supplied")
  }
  if (!is.null(newxreg)) {
    if (NROW(as.matrix(newxreg)) != h) {
      stop("newxreg should have the same number of rows as the forecast horizon h")
    }
  }
  if (fan) {
    level <- seq(51, 99, by = 3)
  }

  if ("mstl" %in% class(object)) {
    seasonal.periods <- attr(object, "seasonal.periods")
    if (is.null(seasonal.periods)) {
      seasonal.periods <- frequency(object)
    }
    seascomp <- matrix(0, ncol = length(seasonal.periods), nrow = h)
    for (i in seq_along(seasonal.periods))
    {
      mp <- round(seasonal.periods[i], 2)
      n <- NROW(object)
      colname <- paste0("Seasonal", mp)
      seascomp[, i] <- rep(object[n - rev(seq_len(mp)) + 1, colname], trunc(1 + (h - 1) / mp))[seq_len(h)]
    }
    lastseas <- rowSums(seascomp)
    xdata <- object[, "Data"]
    seascols <- grep("Seasonal", colnames(object))
    allseas <- rowSums(object[, seascols, drop = FALSE])
    series <- NULL
  }
  else if ("stl" %in% class(object)) {
    m <- frequency(object$time.series)
    n <- NROW(object$time.series)
    lastseas <- rep(seasonal(object)[n - (m:1) + 1], trunc(1 + (h - 1) / m))[1:h]
    xdata <- ts(rowSums(object$time.series))
    tsp(xdata) <- tsp(object$time.series)
    allseas <- seasonal(object)
    series <- deparse(object$call$x)
  }
  else {
    stop("Unknown object class")
  }

  # De-seasonalize
  x.sa <- seasadj(object)
  # Forecast
  fcast <- forecastfunction(x.sa, h = h, level = level, ...)

  # Reseasonalize
  fcast$mean <- fcast$mean + lastseas
  fcast$upper <- fcast$upper + lastseas
  fcast$lower <- fcast$lower + lastseas
  fcast$x <- xdata
  fcast$method <- paste("STL + ", fcast$method)
  fcast$series <- series
  # fcast$seasonal <- ts(lastseas[1:m],frequency=m,start=tsp(object$time.series)[2]-1+1/m)
  fcast$fitted <- fitted(fcast) + allseas
  fcast$residuals <- fcast$x - fcast$fitted

  if (!is.null(lambda)) {
    fcast$x <- InvBoxCox(fcast$x, lambda)
    fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
    fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast)
    fcast$lower <- InvBoxCox(fcast$lower, lambda)
    fcast$upper <- InvBoxCox(fcast$upper, lambda)
    attr(lambda, "biasadj") <- biasadj
    fcast$lambda <- lambda
  }

  return(fcast)
}

#' @export
forecast.mstl <- function(object, method = c("ets", "arima", "naive", "rwdrift"), etsmodel = "ZZN",
                          forecastfunction = NULL,
                          h = frequency(object) * 2, level = c(80, 95), fan = FALSE,
                          lambda = NULL, biasadj = NULL, xreg = NULL, newxreg = NULL, allow.multiplicative.trend = FALSE, ...) {
  forecast.stl(
    object,
    method = method, etsmodel = etsmodel,
    forecastfunction = forecastfunction, h = h, level = level, fan = fan, lambda = lambda,
    biasadj = biasadj, xreg = xreg, newxreg = newxreg, allow.multiplicative.trend = allow.multiplicative.trend, ...
  )
}

# Function takes time series, does STL decomposition, and fits a model to seasonally adjusted series
# But it does not forecast. Instead, the result can be passed to forecast().
#' @rdname forecast.stl
#' @export
stlm <- function(y, s.window = 13, robust = FALSE, method = c("ets", "arima"), modelfunction = NULL, model = NULL,
                 etsmodel = "ZZN", lambda = NULL, biasadj = FALSE, xreg = NULL, allow.multiplicative.trend = FALSE, x = y, ...) {
  method <- match.arg(method)

  # Check univariate
  if (NCOL(x) > 1L) {
    stop("y must be a univariate time series")
  } else {
    if (!is.null(ncol(x))) {
      if (ncol(x) == 1L) { # Probably redundant check
        x <- x[, 1L]
      }
    }
  }
  # Check x is a seasonal time series
  tspx <- tsp(x)
  if (is.null(tspx)) {
    stop("y is not a seasonal ts object")
  } else if (tspx[3] <= 1L) {
    stop("y is not a seasonal ts object")
  }

  # Transform data if necessary
  origx <- x
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
  }

  # Do STL decomposition
  stld <- mstl(x, s.window = s.window, robust = robust)

  if (!is.null(model)) {
    if (inherits(model$model, "ets")) {
      modelfunction <- function(x, ...) {
        return(ets(x, model = model$model, use.initial.values = TRUE, ...))
      }
    }
    else if (inherits(model$model, "Arima")) {
      modelfunction <- function(x, ...) {
        return(Arima(x, model = model$model, xreg = xreg, ...))
      }
    }
    else if (!is.null(model$modelfunction)) {
      if ("model" %in% names(formals(model$modelfunction))) {
        modelfunction <- function(x, ...) {
          return(model$modelfunction(x, model = model$model, ...))
        }
      }
    }
    if (is.null(modelfunction)) {
      stop("Unknown model type")
    }
  }
  # Construct modelfunction if not passed as an argument
  else if (is.null(modelfunction)) {
    if (method != "arima" && !is.null(xreg)) {
      stop("xreg arguments can only be used with ARIMA models")
    }
    if (method == "ets") {
      # Ensure non-seasonal model
      if (substr(etsmodel, 3, 3) != "N") {
        warning("The ETS model must be non-seasonal. I'm ignoring the seasonal component specified.")
        substr(etsmodel, 3, 3) <- "N"
      }
      modelfunction <- function(x, ...) {
        return(ets(
          x,
          model = etsmodel,
          allow.multiplicative.trend = allow.multiplicative.trend, ...
        ))
      }
    }
    else if (method == "arima") {
      modelfunction <- function(x, ...) {
        return(auto.arima(x, xreg = xreg, seasonal = FALSE, ...))
      }
    }
  }

  # De-seasonalize
  x.sa <- seasadj(stld)

  # Model seasonally adjusted data
  fit <- modelfunction(x.sa, ...)
  fit$x <- x.sa

  # Fitted values and residuals
  seascols <- grep("Seasonal", colnames(stld))
  allseas <- rowSums(stld[, seascols, drop = FALSE])

  fits <- fitted(fit) + allseas
  res <- residuals(fit)
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda, biasadj, var(res))
    attr(lambda, "biasadj") <- biasadj
  }

  return(structure(list(
    stl = stld, model = fit, modelfunction = modelfunction, lambda = lambda,
    x = origx, series = deparse(substitute(y)), m = frequency(origx), fitted = fits, residuals = res
  ), class = "stlm"))
}

#' @rdname forecast.stl
#' @export
forecast.stlm <- function(object, h = 2 * object$m, level = c(80, 95), fan = FALSE,
                          lambda = object$lambda, biasadj = NULL, newxreg = NULL, allow.multiplicative.trend = FALSE, ...) {
  if (!is.null(newxreg)) {
    if (nrow(as.matrix(newxreg)) != h) {
      stop("newxreg should have the same number of rows as the forecast horizon h")
    }
  }
  if (fan) {
    level <- seq(51, 99, by = 3)
  }

  # Forecast seasonally adjusted series
  if (is.element("Arima", class(object$model)) && !is.null(newxreg)) {
    fcast <- forecast(object$model, h = h, level = level, xreg = newxreg, ...)
  } else if (is.element("ets", class(object$model))) {
    fcast <- forecast(
      object$model,
      h = h, level = level,
      allow.multiplicative.trend = allow.multiplicative.trend, ...
    )
  } else {
    fcast <- forecast(object$model, h = h, level = level, ...)
  }

  # In-case forecast method uses different horizon length (such as using xregs)
  h <- NROW(fcast$mean)
  # Forecast seasonal series with seasonal naive
  seasonal.periods <- attributes(object$stl)$seasonal.periods
  if (is.null(seasonal.periods)) {
    seasonal.periods <- frequency(object$stl)
  }
  seascomp <- matrix(0, ncol = length(seasonal.periods), nrow = h)
  for (i in seq_along(seasonal.periods))
  {
    mp <- seasonal.periods[i]
    n <- NROW(object$stl)
    colname <- paste0("Seasonal", round(mp, 2))
    seascomp[, i] <- rep(object$stl[n - rev(seq_len(mp)) + 1, colname], trunc(1 + (h - 1) / mp))[seq_len(h)]
  }
  lastseas <- rowSums(seascomp)
  xdata <- object$stl[, "Data"]
  seascols <- grep("Seasonal", colnames(object$stl))
  allseas <- rowSums(object$stl[, seascols, drop = FALSE])
  series <- NULL

  #  m <- frequency(object$stl$time.series)
  n <- NROW(xdata)

  # Reseasonalize
  fcast$mean <- fcast$mean + lastseas
  fcast$upper <- fcast$upper + lastseas
  fcast$lower <- fcast$lower + lastseas
  fcast$method <- paste("STL + ", fcast$method)
  fcast$series <- object$series
  # fcast$seasonal <- ts(lastseas[1:m],frequency=m,start=tsp(object$stl$time.series)[2]-1+1/m)
  # fcast$residuals <- residuals()
  fcast$fitted <- fitted(fcast) + allseas
  fcast$residuals <- residuals(fcast)

  if (!is.null(lambda)) {
    fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
    fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast)
    fcast$lower <- InvBoxCox(fcast$lower, lambda)
    fcast$upper <- InvBoxCox(fcast$upper, lambda)
    attr(lambda, "biasadj") <- biasadj
    fcast$lambda <- lambda
  }
  fcast$x <- object$x

  return(fcast)
}

#' @rdname forecast.stl
#'
#' @examples
#'
#' plot(stlf(AirPassengers, lambda = 0))
#' @export
stlf <- function(y, h = frequency(x) * 2, s.window = 13, t.window = NULL, robust = FALSE, lambda = NULL, biasadj = FALSE, x = y, ...) {
  seriesname <- deparse(substitute(y))

  # Check univariate
  if (NCOL(x) > 1L) {
    stop("y must be a univariate time series")
  } else {
    if (!is.null(ncol(x))) {
      if (ncol(x) == 1L) { # Probably redundant check
        x <- x[, 1L]
      }
    }
  }
  # Check x is a seasonal time series
  tspx <- tsp(x)
  if (is.null(tspx)) {
    stop("y is not a seasonal ts object")
  } else if (tspx[3] <= 1L) {
    stop("y is not a seasonal ts object")
  }

  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
  }

  fit <- mstl(x, s.window = s.window, t.window = t.window, robust = robust)
  fcast <- forecast(fit, h = h, lambda = lambda, biasadj = biasadj, ...)

  # if (!is.null(lambda))
  # {
  #   fcast$x <- origx
  #   fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
  #   fcast$mean <- InvBoxCox(fcast$mean, lambda)
  #   fcast$lower <- InvBoxCox(fcast$lower, lambda)
  #   fcast$upper <- InvBoxCox(fcast$upper, lambda)
  #   fcast$lambda <- lambda
  # }

  fcast$series <- seriesname

  return(fcast)
}

#' @rdname is.ets
#' @export
is.stlm <- function(x) {
  inherits(x, "stlm")
}

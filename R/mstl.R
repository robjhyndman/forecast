#' Multiple seasonal decomposition
#'
#' Decompose a time series into seasonal, trend and remainder components.
#' Seasonal components are estimated iteratively using STL. Multiple seasonal periods are
#' allowed. The trend component is computed for the last iteration of STL.
#' Non-seasonal time series are decomposed into trend and remainder only.
#' In this case, [stats::supsmu()] is used to estimate the trend.
#' Optionally, the time series may be Box-Cox transformed before decomposition.
#' Unlike [stats::stl()], `mstl` is completely automated.
#' @param x Univariate time series of class `msts` or `ts`.
#' @param iterate Number of iterations to use to refine the seasonal component.
#' @param s.window Seasonal windows to be used in the  decompositions. If scalar,
#' the same value is used for all seasonal components. Otherwise, it should be a vector
#' of the same length as the number of seasonal components (or longer).
#' @param ... Other arguments are passed to [stats::stl()].
#' @inheritParams forecast.ts
#'
#' @seealso [stats::stl()], [stats::supsmu()]
#' @examples
#' library(ggplot2)
#' mstl(taylor) |> autoplot()
#' mstl(AirPassengers, lambda = "auto") |> autoplot()
#' @export
mstl <- function(
  x,
  lambda = NULL,
  biasadj = FALSE,
  iterate = 2,
  s.window = 7 + 4 * seq(6),
  ...
) {
  # What is x?
  origx <- x
  n <- length(x)
  if (inherits(x, "msts")) {
    msts <- attributes(x)$msts
    if (any(msts >= n / 2)) {
      warning("Dropping seasonal components with fewer than two full periods.")
      msts <- msts[msts < n / 2]
      x <- msts(x, seasonal.periods = msts)
    }
    msts <- sort(msts, decreasing = FALSE)
  } else if (is.ts(x)) {
    msts <- frequency(x)
    iterate <- 1L
  } else {
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
    x <- BoxCox(x, lambda = lambda)
    attr(lambda, "biasadj") <- biasadj
  }

  # Now fit stl models with only one type of seasonality at a time
  if (msts[1L] > 1) {
    seas <- as.list(rep(0, length(msts)))
    deseas <- x
    if (length(s.window) == 1L) {
      s.window <- rep(s.window, length(msts))
    }
    iterate <- pmax(1L, iterate)
    for (j in seq_len(iterate)) {
      for (i in seq_along(msts)) {
        deseas <- deseas + seas[[i]]
        fit <- stl(ts(deseas, frequency = msts[i]), s.window = s.window[i], ...)
        seas[[i]] <- msts(seasonal(fit), seasonal.periods = msts)
        attributes(seas[[i]]) <- attributes(x)
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- msts(trendcycle(fit), seasonal.periods = msts)
  } else {
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
  output <- cbind(c(origx), c(trend))
  if (!is.null(msts)) {
    for (i in seq_along(msts)) {
      output <- cbind(output, c(seas[[i]]))
    }
  }
  output <- cbind(output, c(remainder))
  colnames(output) <- paste0("V", seq_len(NCOL(output)))
  colnames(output)[1L:2L] <- c("Data", "Trend")
  if (!is.null(msts)) {
    colnames(output)[2L + seq_along(msts)] <- paste0("Seasonal", round(msts, 2))
  }
  colnames(output)[NCOL(output)] <- "Remainder"

  output <- copy_msts(origx, output)
  class(output) <- c("mstl", class(output))
  output
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
#' `forecast.stlm` forecasts the seasonally adjusted data, then
#' re-seasonalizes the results by adding back the last year of the estimated
#' seasonal component.
#'
#' `stlf` combines [stlm()] and `forecast.stlm`. It takes a
#' `ts` argument, applies an STL decomposition, models the seasonally
#' adjusted data, reseasonalizes, and returns the forecasts. However, it allows
#' more general forecasting methods to be specified via
#' `forecastfunction`.
#'
#' `forecast.stl` is similar to `stlf` except that it takes the STL
#' decomposition as the first argument, instead of the time series.
#'
#' Note that the prediction intervals ignore the uncertainty associated with
#' the seasonal component. They are computed using the prediction intervals
#' from the seasonally adjusted series, which are then reseasonalized using the
#' last year of the seasonal component. The uncertainty in the seasonal
#' component is ignored.
#'
#' The forecasting method for the seasonally adjusted data can be specified in
#' `stlf` and `forecast.stl` using either `method` or
#' `forecastfunction`. The `method` argument provides a shorthand way
#' of specifying `forecastfunction` for a few special cases. More
#' generally, `forecastfunction` can be any function with first argument a
#' `ts` object, and other `h` and `level`, which returns an
#' object of class [forecast()]. For example,
#' `forecastfunction = thetaf` uses the [thetaf()] function for
#' forecasting the seasonally adjusted series.
#'
#' @inheritParams forecast.Arima
#' @param object An object of class `stl` or `stlm`. Usually the
#' result of a call to [stats::stl()] or `stlm`.
#' @param method Method to use for forecasting the seasonally adjusted series.
#' @param forecastfunction An alternative way of specifying the function for
#' forecasting the seasonally adjusted series. If `forecastfunction` is
#' not `NULL`, then `method` is ignored. Otherwise `method` is
#' used to specify the forecasting method to be used.
#' @param etsmodel The ets model specification passed to
#' [ets()]. By default it allows any non-seasonal model. If
#' `method != "ets"`, this argument is ignored.
#' @param xreg Historical regressors to be used in
#' [auto.arima()] when `method = "arima"`.
#' @param newxreg Future regressors to be used in [forecast.Arima()].
#' @param s.window Either the character string `"periodic"` or the span (in
#' lags) of the loess window for seasonal extraction.
#' @param t.window A number to control the smoothness of the trend. See
#' [stats::stl()] for details.
#' @param robust If `TRUE`, robust fitting will used in the loess
#' procedure within [stats::stl()].
#' @param allow.multiplicative.trend If `TRUE`, then ETS models with
#' multiplicative trends are allowed. Otherwise, only additive or no trend ETS
#' models are permitted.
#' @param ... Other arguments passed to `forecast.stl`,
#' `modelfunction` or `forecastfunction`.
#' @inheritParams Arima
#'
#' @return `stlm` returns an object of class `stlm`. The other
#' functions return objects of class `forecast`.
#'
#' There are many methods for working with [forecast()] objects
#' including `summary` to obtain and print a summary of the results, while
#' `plot` produces a plot of the forecasts and prediction intervals. The
#' generic accessor functions `fitted.values` and `residuals` extract
#' useful features.
#' @author Rob J Hyndman
#' @seealso [stats::stl()], [forecast.ets()], [forecast.Arima()].
#' @keywords ts
#' @examples
#'
#' tsmod <- stlm(USAccDeaths, modelfunction = ar)
#' plot(forecast(tsmod, h = 36))
#'
#' decomp <- stl(USAccDeaths, s.window = "periodic")
#' plot(forecast(decomp))
#' @export
forecast.stl <- function(
  object,
  method = c("ets", "arima", "naive", "rwdrift"),
  etsmodel = "ZZN",
  forecastfunction = NULL,
  h = frequency(object$time.series) * 2,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  xreg = NULL,
  newxreg = NULL,
  allow.multiplicative.trend = FALSE,
  ...
) {
  method <- match.arg(method)
  if (is.null(forecastfunction)) {
    if (method != "arima" && (!is.null(xreg) || !is.null(newxreg))) {
      stop("xreg and newxreg arguments can only be used with ARIMA models")
    }
    if (method == "ets") {
      # Ensure non-seasonal model
      if (substr(etsmodel, 3, 3) != "N") {
        warning(
          "The ETS model must be non-seasonal. I'm ignoring the seasonal component specified."
        )
        substr(etsmodel, 3, 3) <- "N"
      }
      forecastfunction <- function(x, h, level, ...) {
        fit <- ets(
          na.interp(x),
          model = etsmodel,
          allow.multiplicative.trend = allow.multiplicative.trend,
          ...
        )
        forecast(fit, h = h, level = level)
      }
    } else if (method == "arima") {
      forecastfunction <- function(x, h, level, ...) {
        fit <- auto.arima(x, xreg = xreg, seasonal = FALSE, ...)
        forecast(fit, h = h, level = level, xreg = newxreg)
      }
    } else if (method == "naive") {
      forecastfunction <- function(x, h, level, ...) {
        rwf(x, drift = FALSE, h = h, level = level, ...)
      }
    } else if (method == "rwdrift") {
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
      stop(
        "newxreg should have the same number of rows as the forecast horizon h"
      )
    }
  }
  if (fan) {
    level <- seq(51, 99, by = 3)
  }

  if (inherits(object, "mstl")) {
    seasoncolumns <- grep("Season", colnames(object), fixed = TRUE)
    nseasons <- length(seasoncolumns)
    seascomp <- matrix(0, ncol = nseasons, nrow = h)
    seasonal.periods <- as.numeric(sub(
      "Seasonal",
      "",
      colnames(object)[seasoncolumns],
      fixed = TRUE
    ))
    n <- NROW(object)
    for (i in seq(nseasons)) {
      mp <- seasonal.periods[i]
      colname <- colnames(object)[seasoncolumns[i]]
      seascomp[, i] <- rep(
        object[n - rev(seq_len(mp)) + 1, colname],
        trunc(1 + (h - 1) / mp)
      )[seq_len(h)]
    }
    lastseas <- rowSums(seascomp)
    xdata <- object[, "Data"]
    seascols <- grep("Seasonal", colnames(object), fixed = TRUE)
    allseas <- rowSumsTS(object[, seascols, drop = FALSE])
    series <- NULL
  } else if (inherits(object, "stl")) {
    m <- frequency(object$time.series)
    n <- NROW(object$time.series)
    lastseas <- rep(seasonal(object)[n - (m:1) + 1], trunc(1 + (h - 1) / m))[
      1:h
    ]
    xdata <- ts(rowSums(object$time.series))
    tsp(xdata) <- tsp(object$time.series)
    allseas <- seasonal(object)
    series <- deparse(object$call$x)
  } else {
    stop("Unknown object class")
  }

  # De-seasonalize
  x.sa <- seasadj(object)
  # Forecast
  fcast <- forecastfunction(x.sa, h = h, level = level, ...)

  # Reseasonalize
  fcast$mean <- future_msts(xdata, fcast$mean + lastseas)
  fcast$upper <- future_msts(xdata, fcast$upper + lastseas)
  fcast$lower <- future_msts(xdata, fcast$lower + lastseas)
  fcast$x <- xdata
  fcast$method <- paste("STL + ", fcast$method)
  fcast$series <- series
  fcast$fitted <- copy_msts(xdata, fitted(fcast) + allseas)
  fcast$residuals <- copy_msts(xdata, fcast$x - fcast$fitted)

  if (!is.null(lambda)) {
    fcast$x <- InvBoxCox(fcast$x, lambda)
    fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
    fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast)
    fcast$lower <- InvBoxCox(fcast$lower, lambda)
    fcast$upper <- InvBoxCox(fcast$upper, lambda)
    attr(lambda, "biasadj") <- biasadj
    fcast$lambda <- lambda
  }

  fcast
}

#' @export
forecast.mstl <- function(
  object,
  method = c("ets", "arima", "naive", "rwdrift"),
  etsmodel = "ZZN",
  forecastfunction = NULL,
  h = frequency(object) * 2,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(object$lambda, "biasadj"),
  xreg = NULL,
  newxreg = NULL,
  allow.multiplicative.trend = FALSE,
  ...
) {
  forecast.stl(
    object,
    method = method,
    etsmodel = etsmodel,
    forecastfunction = forecastfunction,
    h = h,
    level = level,
    fan = fan,
    lambda = lambda,
    biasadj = biasadj,
    xreg = xreg,
    newxreg = newxreg,
    allow.multiplicative.trend = allow.multiplicative.trend,
    ...
  )
}

# rowSums for mts objects
#
# Applies rowSums and returns ts with same tsp attributes as input. This
# allows the result to be added to other time series with different lengths
# but overlapping time indexes.
# param mts a matrix or multivariate time series
# return a vector of rowsums which is a ts if the `mts` is a ts
rowSumsTS <- function(mts) {
  the_tsp <- tsp(mts)
  ret <- rowSums(mts)
  if (is.null(the_tsp)) {
    ret
  } else {
    tsp(ret) <- the_tsp
    as.ts(ret)
  }
}

#' Forecasting model using STL with a generative time series model
#'
#' Forecasts of STL objects are obtained by applying a non-seasonal forecasting
#' model to the seasonally adjusted data and re-seasonalizing using the last
#' year of the seasonal component. `stlm` takes a time series `y`, applies an STL decomposition, and
#' models the seasonally adjusted data using the model passed as
#' `modelfunction` or specified using `method`. It returns an object
#' that includes the original STL decomposition and a time series model fitted
#' to the seasonally adjusted data. This object can be passed to the
#' `forecast.stlm` for forecasting.
#'
#' The time series model for the seasonally adjusted data can be specified in
#' `stlm` using either `method` or `modelfunction`. The
#' `method` argument provides a shorthand way of specifying
#' `modelfunction` for a few special cases. More generally,
#' `modelfunction` can be any function with first argument a `ts`
#' object, that returns an object that can be passed to [forecast()].
#' For example, `modelfunction = ar` uses the [ar()] function
#' for modelling the seasonally adjusted series.
#'
#' @inheritParams Arima
#' @param method Method to use for forecasting the seasonally adjusted series.
#' @param modelfunction An alternative way of specifying the function for
#' modelling the seasonally adjusted series. If `modelfunction` is not
#' `NULL`, then `method` is ignored. Otherwise `method` is used
#' to specify the time series model to be used.
#' @param model Output from a previous call to `stlm`. If a `stlm`
#' model is passed, this same model is fitted to y without re-estimating any
#' parameters.
#' @param etsmodel The ets model specification passed to
#' [ets()]. By default it allows any non-seasonal model. If
#' `method != "ets"`, this argument is ignored.
#' @param xreg Historical regressors to be used in
#' [auto.arima()] when `method = "arima"`.
#' @param s.window Either the character string `"periodic"` or the span (in
#' lags) of the loess window for seasonal extraction.
#' @param t.window A number to control the smoothness of the trend. See
#' [stats::stl()] for details.
#' @param robust If `TRUE`, robust fitting will used in the loess
#' procedure within [stats::stl()].
#' @param allow.multiplicative.trend If `TRUE`, then ETS models with
#' multiplicative trends are allowed. Otherwise, only additive or no trend ETS
#' models are permitted.
#' @param ... Other arguments passed to `modelfunction`.
#'
#' @return An object of class `stlm`. 
#'
#' @author Rob J Hyndman
#' @seealso [stats::stl()], [ets()], [Arima()].
#' @keywords ts
#' @examples
#'
#' tsmod <- stlm(USAccDeaths, modelfunction = ar)
#' forecast(tsmod, h = 36) |> autoplot()
#'
#' decomp <- stl(USAccDeaths, s.window = "periodic")
#' forecast(decomp) |> autoplot()
#' @export
stlm <- function(
  y,
  s.window = 7 + 4 * seq(6),
  t.window = NULL,
  robust = FALSE,
  method = c("ets", "arima"),
  modelfunction = NULL,
  model = NULL,
  etsmodel = "ZZN",
  lambda = NULL,
  biasadj = FALSE,
  xreg = NULL,
  allow.multiplicative.trend = FALSE,
  x = y,
  ...
) {
  method <- match.arg(method)

  # Check univariate
  if (NCOL(x) > 1L) {
    stop("y must be a univariate time series")
  } else {
    if (!is.null(ncol(x))) {
      if (ncol(x) == 1L) {
        # Probably redundant check
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

  if (!is.null(model) && is.null(lambda)) {
    lambda <- model$lambda
  }

  # Transform data if necessary
  origx <- x
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }

  # Do STL decomposition
  stld <- mstl(x, s.window = s.window, t.window = t.window, robust = robust)

  if (!is.null(model)) {
    if (inherits(model$model, "ets")) {
      modelfunction <- function(x, ...) {
        ets(x, model = model$model, use.initial.values = TRUE, ...)
      }
    } else if (inherits(model$model, "Arima")) {
      modelfunction <- function(x, ...) {
        Arima(x, model = model$model, xreg = xreg, ...)
      }
    } else if (!is.null(model$modelfunction)) {
      if ("model" %in% names(formals(model$modelfunction))) {
        modelfunction <- function(x, ...) {
          model$modelfunction(x, model = model$model, ...)
        }
      }
    }
    if (is.null(modelfunction)) {
      stop("Unknown model type")
    }
  } else if (is.null(modelfunction)) {
    # Construct modelfunction if not passed as an argument
    if (method != "arima" && !is.null(xreg)) {
      stop("xreg arguments can only be used with ARIMA models")
    }
    if (method == "ets") {
      # Ensure non-seasonal model
      if (substr(etsmodel, 3, 3) != "N") {
        warning(
          "The ETS model must be non-seasonal. I'm ignoring the seasonal component specified."
        )
        substr(etsmodel, 3, 3) <- "N"
      }
      modelfunction <- function(x, ...) {
        ets(
          x,
          model = etsmodel,
          allow.multiplicative.trend = allow.multiplicative.trend,
          ...
        )
      }
    } else if (method == "arima") {
      modelfunction <- function(x, ...) {
        auto.arima(x, xreg = xreg, seasonal = FALSE, ...)
      }
    }
  }

  # De-seasonalize
  x.sa <- seasadj(stld)

  # Model seasonally adjusted data
  fit <- modelfunction(x.sa, ...)
  fit$x <- x.sa

  # Fitted values and residuals
  seascols <- grep("Seasonal", colnames(stld), fixed = TRUE)
  allseas <- rowSumsTS(stld[, seascols, drop = FALSE])

  fits <- fitted(fit) + allseas
  res <- residuals(fit)
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda, biasadj, var(res))
  }

  structure(
    list(
      stl = stld,
      model = fit,
      modelfunction = modelfunction,
      lambda = lambda,
      x = origx,
      series = deparse1(substitute(y)),
      m = frequency(origx),
      fitted = fits,
      residuals = res
    ),
    class = c("fc_model", "stlm")
  )
}

#' @rdname forecast.stl
#' @export
forecast.stlm <- function(
  object,
  h = 2 * object$m,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(lambda, "biasadj"),
  newxreg = NULL,
  allow.multiplicative.trend = FALSE,
  ...
) {
  if (!is.null(newxreg)) {
    if (nrow(as.matrix(newxreg)) != h) {
      stop(
        "newxreg should have the same number of rows as the forecast horizon h"
      )
    }
  }
  if (fan) {
    level <- seq(51, 99, by = 3)
  }

  # Forecast seasonally adjusted series
  if (is.Arima(object$model) && !is.null(newxreg)) {
    fcast <- forecast(object$model, h = h, level = level, xreg = newxreg, ...)
  } else if (is.ets(object$model)) {
    fcast <- forecast(
      object$model,
      h = h,
      level = level,
      allow.multiplicative.trend = allow.multiplicative.trend,
      ...
    )
  } else {
    fcast <- forecast(object$model, h = h, level = level, ...)
  }

  # In-case forecast method uses different horizon length (such as using xregs)
  h <- NROW(fcast$mean)
  # Forecast seasonal series with seasonal naive
  seasonal.periods <- attributes(object$stl)$msts
  if (is.null(seasonal.periods)) {
    seasonal.periods <- frequency(object$stl)
  }
  seascomp <- matrix(0, ncol = length(seasonal.periods), nrow = h)
  for (i in seq_along(seasonal.periods)) {
    mp <- seasonal.periods[i]
    n <- NROW(object$stl)
    colname <- paste0("Seasonal", round(mp, 2))
    seascomp[, i] <- rep(
      object$stl[n - rev(seq_len(mp)) + 1, colname],
      trunc(1 + (h - 1) / mp)
    )[seq_len(h)]
  }
  lastseas <- rowSums(seascomp)
  xdata <- object$stl[, "Data"]
  seascols <- grep("Seasonal", colnames(object$stl), fixed = TRUE)
  allseas <- rowSumsTS(object$stl[, seascols, drop = FALSE])

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

  fcast
}

#' @rdname forecast.stl
#'
#' @examples
#'
#' plot(stlf(AirPassengers, lambda = 0))
#' @export
stlf <- function(
  y,
  h = frequency(x) * 2,
  s.window = 7 + 4 * seq(6),
  t.window = NULL,
  robust = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
) {
  seriesname <- deparse1(substitute(y))

  # Check univariate
  if (NCOL(x) > 1L) {
    stop("y must be a univariate time series")
  } else {
    if (!is.null(ncol(x))) {
      if (ncol(x) == 1L) {
        # Probably redundant check
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
    attr(lambda, "biasadj") <- biasadj
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

  fcast
}

#' @rdname is.ets
#' @export
is.stlm <- function(x) {
  inherits(x, "stlm")
}

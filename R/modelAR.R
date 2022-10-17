# Defaults:
# For non-seasonal data, p chosen using AIC from linear AR(p) model
# For seasonal data, p chosen using AIC from linear AR(p) model after
#    seasonally adjusting with STL decomposition, and P=1

#' Time Series Forecasts with a user-defined model
#'
#' Experimental function to forecast univariate time series with a
#' user-defined model
#'
#' This is an experimental function and only recommended for advanced users.
#' The selected model is fitted with lagged values of \code{y} as
#' inputs. The inputs are for
#' lags 1 to \code{p}, and lags \code{m} to \code{mP} where
#' \code{m=frequency(y)}. If \code{xreg} is provided, its columns are also
#' used as inputs. If there are missing values in \code{y} or
#' \code{xreg}, the corresponding rows (and any others which depend on them as
#' lags) are omitted from the fit. The model is trained for one-step
#' forecasting. Multi-step forecasts are computed recursively.
#'
#' @aliases print.modelAR
#'
#' @param y A numeric vector or time series of class \code{ts}.
#' @param p Embedding dimension for non-seasonal time series. Number of
#' non-seasonal lags used as inputs. For non-seasonal time series, the default
#' is the optimal number of lags (according to the AIC) for a linear AR(p)
#' model. For seasonal time series, the same method is used but applied to
#' seasonally adjusted data (from an stl decomposition).
#' @param P Number of seasonal lags used as inputs.
#' @param FUN Function used for model fitting. Must accept argument \code{x}
#' and \code{y} for the predictors and response, respectively (\code{formula}
#' object not currently supported).
#' @param predict.FUN Prediction function used to apply \code{FUN} to new data.
#' Must accept an object of class \code{FUN} as its first argument, and a
#' data frame or matrix of new data for its second argument. Additionally,
#' it should return fitted values when new data is omitted.
#' @param xreg Optionally, a vector or matrix of external regressors, which
#' must have the same number of rows as \code{y}. Must be numeric.
#' @param model Output from a previous call to \code{nnetar}. If model is
#' passed, this same model is fitted to \code{y} without re-estimating any
#' parameters.
#' @param subset Optional vector specifying a subset of observations to be used
#' in the fit. Can be an integer index vector or a logical vector the same
#' length as \code{y}. All observations are used by default.
#' @param scale.inputs If TRUE, inputs are scaled by subtracting the column
#' means and dividing by their respective standard deviations. If \code{lambda}
#' is not \code{NULL}, scaling is applied after Box-Cox transformation.
#' @param x Deprecated. Included for backwards compatibility.
#' @param \dots Other arguments passed to \code{FUN} for
#' \code{modelAR}.
#' @inheritParams forecast.ts
#'
#' @return Returns an object of class "\code{modelAR}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{nnetar}.
#'
#' \item{model}{A list containing information about the fitted model}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{x}{The original time series.}
#' \item{xreg}{The external regressors used in fitting (if given).}
#' \item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
#' \item{fitted}{Fitted values (one-step forecasts)}
#' \item{...}{Other arguments}
#'
#' @author Rob J Hyndman and Gabriel Caceres
#' @keywords ts
#'
#' @export
modelAR <- function(y, p, P=1, FUN, predict.FUN, xreg=NULL, lambda=NULL, model=NULL, subset=NULL, scale.inputs=FALSE, x=y, ...) {
  useoldmodel <- FALSE
  yname <- deparse(substitute(y))
  if (!is.null(model)) {
    # Use previously fitted model
    useoldmodel <- TRUE
    # Check for conflicts between new and old data:
    # Check model class
    if (!is.modelAR(model)) {
      stop("Model must be a modelAR object")
    }
    # Check new data
    m <- max(round(frequency(model$x)), 1L)
    minlength <- max(c(model$p, model$P * m)) + 1
    if (length(x) < minlength) {
      stop(paste("Series must be at least of length", minlength, "to use fitted model"))
    }
    if (tsp(as.ts(x))[3] != m) {
      warning(paste("Data frequency doesn't match fitted model, coercing to frequency =", m))
      x <- ts(x, frequency = m)
    }
    # Check xreg
    if (!is.null(model$xreg)) {
      if (is.null(xreg)) {
        stop("No external regressors provided")
      }
      if (NCOL(xreg) != NCOL(model$xreg)) {
        stop("Number of external regressors does not match fitted model")
      }
    }
    # Update parameters with previous model
    lambda <- model$lambda
    p <- model$p
    P <- model$P
    FUN <- model$FUN
    predict.FUN <- model$predict.FUN
    if (P > 0) {
      lags <- sort(unique(c(1:p, m * (1:P))))
    } else {
      lags <- 1:p
    }
    if (!is.null(model$scalex)) {
      scale.inputs <- TRUE
    }
  } else {                 # when not using an old model
    if (length(y) < 3) {
      stop("Not enough data to fit a model")
    }
    # Check for constant data in time series
    constant_data <- is.constant(na.interp(x))
    if (constant_data){
      warning("Constant data, setting p=1, P=0, lambda=NULL, scale.inputs=FALSE")
      scale.inputs <- FALSE
      lambda <- NULL
      p <- 1
      P <- 0
    }
    ## Check for constant data in xreg
    if (!is.null(xreg)){
      constant_xreg <- any(apply(as.matrix(xreg), 2, function(x) is.constant(na.interp(x))))
      if (constant_xreg){
        warning("Constant xreg column, setting scale.inputs=FALSE")
        scale.inputs <- FALSE
      }
    }
  }

  # Check for NAs in x
  if (any(is.na(x))) {
    warning("Missing values in x, omitting rows")
  }

  # Transform data
  if (!is.null(lambda)) {
    xx <- BoxCox(x, lambda)
    lambda <- attr(xx, "lambda")
  } else {
    xx <- x
  }
  ## Check whether to use a subset of the data
  xsub <- rep(TRUE, length(x))
  if (is.numeric(subset)) {
    xsub[-subset] <- FALSE
  }
  if (is.logical(subset)) {
    xsub <- subset
  }
  # Scale series
  scalex <- NULL
  if (scale.inputs) {
    if (useoldmodel) {
      scalex <- model$scalex
    }
    else {
      tmpx <- scale(xx[xsub], center = TRUE, scale = TRUE)
      scalex <- list(
        center = attr(tmpx, "scaled:center"),
        scale = attr(tmpx, "scaled:scale")
      )
    }
    xx <- scale(xx, center = scalex$center, scale = scalex$scale)
    xx <- xx[, 1]
  }
  # Check xreg class & dim
  xxreg <- NULL
  scalexreg <- NULL
  if (!is.null(xreg)) {
    xxreg <- xreg <- as.matrix(xreg)
    if (length(x) != NROW(xreg)) {
      stop("Number of rows in xreg does not match series length")
    }
    # Check for NAs in xreg
    if (any(is.na(xreg))) {
      warning("Missing values in xreg, omitting rows")
    }
    # Scale xreg
    if (scale.inputs) {
      if (useoldmodel) {
        scalexreg <- model$scalexreg
      }
      else {
        tmpx <- scale(xxreg[xsub, ], center = TRUE, scale = TRUE)
        scalexreg <- list(
          center = attr(tmpx, "scaled:center"),
          scale = attr(tmpx, "scaled:scale")
        )
      }
      xxreg <- scale(xxreg, center = scalexreg$center, scale = scalexreg$scale)
    }
  }
  # Set up lagged matrix
  n <- length(xx)
  xx <- as.ts(xx)
  m <- max(round(frequency(xx)), 1L)
  if (!useoldmodel) {
    if (m == 1) {
      if (missing(p)) {
        p <- max(length(ar(na.interp(xx))$ar), 1)
      }
      if (p >= n) {
        warning("Reducing number of lagged inputs due to short series")
        p <- n - 1
      }
      lags <- 1:p
      if (P > 1) {
        warning("Non-seasonal data, ignoring seasonal lags")
      }
      P <- 0
    } else {
      if (missing(p)) {
        if (n >= 2 * m) {
          x.sa <- seasadj(mstl(na.interp(xx)))
        } else {
          x.sa <- na.interp(xx)
        }
        p <- max(length(ar(x.sa)$ar), 1)
      }
      if (p >= n) {
        warning("Reducing number of lagged inputs due to short series")
        p <- n - 1
      }
      if (P > 0 && n >= m * P + 2) {
        lags <- sort(unique(c(1:p, m * (1:P))))
      } else {
        lags <- 1:p
        if (P > 0) {
          warning("Series too short for seasonal lags")
          P <- 0
        }
      }
    }
  }
  maxlag <- max(lags)
  nlag <- length(lags)
  y <- xx[-(1:maxlag)]
  lags.X <- matrix(NA_real_, ncol = nlag, nrow = n - maxlag)
  for (i in 1:nlag){
    lags.X[, i] <- xx[(maxlag - lags[i] + 1):(n - lags[i])]
  }
  # Add xreg into lagged matrix
  lags.X <- cbind(lags.X, xxreg[-(1:maxlag), ])
  # Remove missing values if present
  j <- complete.cases(lags.X, y)
  ## Remove values not in subset
  j <- j & xsub[-(1:maxlag)]
  ## Stop if there's no data to fit (e.g. due to NAs or NaNs)
  if (NROW(lags.X[j,, drop=FALSE]) == 0) {
    stop("No data to fit (possibly due to NA or NaN)")
  }
  ## Fit selected model
  if (useoldmodel) {
    fit <- model$model
  } else {
    fit <- FUN(x = lags.X[j,, drop=FALSE], y = y[j], ...)
  }
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$FUN <- FUN
  out$predict.FUN <- predict.FUN
  out$scalex <- scalex
  out$scalexreg <- scalexreg
  out$xreg <- xreg
  out$lambda <- lambda
  out$subset <- (1:length(x))[xsub]
  out$model <- fit
  out$modelargs <- list(...)
  if (useoldmodel) {
    out$modelargs <- model$modelargs
    fits <- c(rep(NA_real_, maxlag), predict.FUN(fit, lags.X[j,, drop=FALSE]))
  } else {
    fits <- c(rep(NA_real_, maxlag), predict.FUN(fit))
  }
  if (scale.inputs) {
    fits <- fits * scalex$scale + scalex$center
  }
  fits <- ts(fits)
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
  }
  out$fitted <- ts(rep(NA_real_, length(out$x)))
  out$fitted[c(rep(TRUE, maxlag), j)] <- fits
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- yname
  out$method <- deparse(substitute(FUN))
  out$method <- paste0(out$method, "-AR(", p)
  if (P > 0) out$method <- paste(out$method, ",", P, sep = "")
  out$method <- paste0(out$method, ")")
  if (P > 0) out$method <- paste(out$method, "[", m, "]", sep = "")
  out$call <- match.call()
  return(structure(out, class = c("modelAR")))
}

#' Forecasting using user-defined model
#'
#' Returns forecasts and other information for user-defined
#' models.
#'
#' Prediction intervals are calculated through simulations and can be slow.
#' Note that if the model is too complex and overfits the data, the residuals
#' can be arbitrarily small; if used for prediction interval calculations, they
#' could lead to misleadingly small values.
#'
#' @param object An object of class "\code{modelAR}" resulting from a call to
#' \code{\link{modelAR}}.
#' @param h Number of periods for forecasting. If \code{xreg} is used, \code{h}
#' is ignored and the number of forecast periods is set to the number of rows
#' of \code{xreg}.
#' @param PI If TRUE, prediction intervals are produced, otherwise only point
#' forecasts are calculated. If \code{PI} is FALSE, then \code{level},
#' \code{fan}, \code{bootstrap} and \code{npaths} are all ignored.
#' @param level Confidence level for prediction intervals.
#' @param fan If \code{TRUE}, level is set to \code{seq(51,99,by=3)}. This is
#' suitable for fan plots.
#' @param xreg Future values of external regressor variables.
#' @param bootstrap If \code{TRUE}, then prediction intervals computed using
#' simulations with resampled residuals rather than normally distributed
#' errors. Ignored if \code{innov} is not \code{NULL}.
#' @param npaths Number of sample paths used in computing simulated prediction
#' intervals.
#' @param innov Values to use as innovations for prediction intervals. Must be
#' a matrix with \code{h} rows and \code{npaths} columns (vectors are coerced
#' into a matrix). If present, \code{bootstrap} is ignored.
#' @param ... Additional arguments passed to \code{\link{simulate.nnetar}}
#' @inheritParams forecast.ts
#'
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{forecast.nnetar}.
#'
#' An object of class "\code{forecast}" is a list containing at least the
#' following elements:
#'   \item{model}{A list containing information about the fitted model}
#'   \item{method}{The name of the forecasting method as a character string}
#'   \item{mean}{Point forecasts as a time series}
#'   \item{lower}{Lower limits for prediction intervals}
#'   \item{upper}{Upper limits for prediction intervals}
#'   \item{level}{The confidence values associated with the prediction intervals}
#'   \item{x}{The original time series (either \code{object} itself or the time series
#'            used to create the model stored as \code{object}).}
#'   \item{xreg}{The external regressors used in fitting (if given).}
#'   \item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
#'   \item{fitted}{Fitted values (one-step forecasts)}
#'   \item{...}{Other arguments}
#'
#' @author Rob J Hyndman and Gabriel Caceres
#' @seealso \code{\link{nnetar}}.
#' @keywords ts
#'
#' @export
forecast.modelAR <- function(object, h=ifelse(object$m > 1, 2 * object$m, 10), PI=FALSE, level=c(80, 95), fan=FALSE, xreg=NULL, lambda=object$lambda, bootstrap=FALSE, npaths=1000, innov=NULL, ...) {
  out <- object
  tspx <- tsp(out$x)
  #
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }
  # Check if xreg was used in fitted model
  if (is.null(object$xreg)) {
    if (!is.null(xreg)) {
      warning("External regressors were not used in fitted model, xreg will be ignored")
    }
    xreg <- NULL
  }
  else {
    if (is.null(xreg)) {
      stop("No external regressors provided")
    }
    xreg <- as.matrix(xreg)
    if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("Number of external regressors does not match fitted model")
    }
    h <- NROW(xreg)
  }
  fcast <- numeric(h)
  xx <- object$x
  xxreg <- xreg
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda)
    lambda <- attr(xx, "lambda")
  }
  # Check and apply scaling of fitted model
  if (!is.null(object$scalex)) {
    xx <- scale(xx, center = object$scalex$center, scale = object$scalex$scale)
    if (!is.null(xreg)) {
      xxreg <- scale(xreg, center = object$scalexreg$center, scale = object$scalexreg$scale)
    }
  }

  # Get lags used in fitted model
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(xx, n = maxlag))
  # Iterative 1-step forecast
  for (i in 1:h)
  {
    newdata <- c(flag[lags], xxreg[i, ])
    if (any(is.na(newdata))) {
      stop("I can't forecast when there are missing values near the end of the series.")
    }
    fcast[i] <- object$predict.FUN(object$model, newdata)
    flag <- c(fcast[i], flag[-maxlag])
  }
  # Re-scale point forecasts
  if (!is.null(object$scalex)) {
    fcast <- fcast * object$scalex$scale + object$scalex$center
  }
  # Add ts properties
  fcast <- ts(fcast, start = tspx[2] + 1 / tspx[3], frequency = tspx[3])
  # Back-transform point forecasts
  if (!is.null(lambda)) {
    fcast <- InvBoxCox(fcast, lambda)
  }
  # Compute prediction intervals using simulations
  if (isTRUE(PI)) {
    nint <- length(level)
    sim <- matrix(NA, nrow = npaths, ncol = h)
    if (!is.null(innov)) {
      if (length(innov) != h * npaths) {
        stop("Incorrect number of innovations, need h*npaths values")
      }
      innov <- matrix(innov, nrow = h, ncol = npaths)
      bootstrap <- FALSE
    }
    for (i in 1:npaths)
      sim[i, ] <- simulate(object, nsim = h, bootstrap = bootstrap, xreg = xreg, lambda = lambda, innov = innov[, i], ...)
    lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8)
    upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8)
    if (nint > 1L) {
      lower <- ts(t(lower))
      upper <- ts(t(upper))
    }
    else {
      lower <- ts(matrix(lower, ncol = 1L))
      upper <- ts(matrix(upper, ncol = 1L))
    }
    tsp(lower) <- tsp(upper) <- tsp(fcast)
  }
  else {
    level <- NULL
    lower <- NULL
    upper <- NULL
  }
  out$mean <- fcast
  out$level <- level
  out$lower <- lower
  out$upper <- upper
  return(structure(out, class = "forecast"))
}

#' @rdname fitted.Arima
#' @export
fitted.modelAR <- function(object, h=1, ...) {
  if (h == 1) {
    return(object$fitted)
  }
  else {
    return(hfitted(object = object, h = h, FUN = "modelAR", ...))
  }
}

#' @export
print.modelAR <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  cat("Call:   ")
  print(x$call)
  print(x$model)
  cat(
    "\nsigma^2 estimated as ", format(mean(residuals(x) ^ 2, na.rm = TRUE), digits = digits),
    "\n", sep = ""
  )
  invisible(x)
}

#' @rdname is.ets
#' @export
is.modelAR <- function(x) {
  inherits(x, "modelAR")
}

# Defaults:
# For non-seasonal data, p chosen using AIC from linear AR(p) model
# For seasonal data, p chosen using AIC from linear AR(p) model after
#    seasonally adjusting with STL decomposition, and P=1
# size set to average of number of inputs and number of outputs: (p+P+1)/2
# if xreg is included then size = (p+P+ncol(xreg)+1)/2

#' Neural Network Time Series Forecasts
#'
#' Feed-forward neural networks with a single hidden layer and lagged inputs
#' for forecasting univariate time series.
#'
#' A feed-forward neural network is fitted with lagged values of `y` as inputs
#' and a single hidden layer with `size` nodes. The inputs are for lags 1 to
#' `p`, and lags `m` to `mP` where `m = frequency(y)`. If `xreg` is provided,
#' its columns are also used as inputs. If there are missing values in `y` or
#' `xreg`, the corresponding rows (and any others which depend on them as lags)
#' are omitted from the fit. A total of `repeats` networks are fitted, each
#' with random starting weights. These are then averaged when computing
#' forecasts. The network is trained for one-step forecasting. Multi-step
#' forecasts are computed recursively.
#'
#' For non-seasonal data, the fitted model is denoted as an NNAR(p,k) model,
#' where k is the number of hidden nodes. This is analogous to an AR(p) model
#' but with nonlinear functions. For seasonal data, the fitted model is called
#' an NNAR(p,P,k)\[m\] model, which is analogous to an ARIMA(p,0,0)(P,0,0)\[m\]
#' model but with nonlinear functions.
#'
#' @aliases print.nnetar print.nnetarmodels
#'
#' @inheritParams Arima
#' @param p Embedding dimension for non-seasonal time series. Number of
#' non-seasonal lags used as inputs. For non-seasonal time series, the default
#' is the optimal number of lags (according to the AIC) for a linear AR(p)
#' model. For seasonal time series, the same method is used but applied to
#' seasonally adjusted data (from an stl decomposition). If set to zero to
#' indicate that no non-seasonal lags should be included, then P must be at
#' least 1 and a model with only seasonal lags will be fit.
#' @param P Number of seasonal lags used as inputs.
#' @param size Number of nodes in the hidden layer. Default is half of the
#' number of input nodes (including external regressors, if given) plus 1.
#' @param repeats Number of networks to fit with different random starting
#' weights. These are then averaged when producing forecasts.
#' @param model Output from a previous call to `nnetar`. If model is
#' passed, this same model is fitted to `y` without re-estimating any
#' parameters.
#' @param subset Optional vector specifying a subset of observations to be used
#' in the fit. Can be an integer index vector or a logical vector the same
#' length as `y`. All observations are used by default.
#' @param scale.inputs If `TRUE`, inputs are scaled by subtracting the column
#' means and dividing by their respective standard deviations. If `lambda`
#' is not `NULL`, scaling is applied after Box-Cox transformation.
#' @param parallel If `TRUE`, then the specification search is done in parallel
#' via [parallel::parLapply()]. This can give a significant speedup on
#' multicore machines.
#' @param num.cores Allows the user to specify the amount of parallel processes
#' to be used if `parallel = TRUE`. If `NULL`, then the number of logical cores
#' is automatically detected and all available cores are used.
#' @param ... Other arguments passed to [nnet::nnet()] for `nnetar`.
#'
#' @return Returns an object of class `nnetar`.
#'
#' The function `summary` is used to obtain and print a summary of the
#' results.
#'
#' The generic accessor functions `fitted.values` and `residuals`
#' extract useful features of the value returned by `nnetar`.
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
#' @examples
#' fit <- nnetar(lynx)
#' fcast <- forecast(fit)
#' plot(fcast)
#'
#' ## Arguments can be passed to nnet()
#' fit <- nnetar(lynx, decay = 0.5, maxit = 150)
#' plot(forecast(fit))
#' lines(lynx)
#'
#' ## Fit model to first 100 years of lynx data
#' fit <- nnetar(window(lynx, end = 1920), decay = 0.5, maxit = 150)
#' plot(forecast(fit, h = 14))
#' lines(lynx)
#'
#' ## Apply fitted model to later data, including all optional arguments
#' fit2 <- nnetar(window(lynx, start = 1921), model = fit)
#'
#' @export
nnetar <- function(
  y,
  p,
  P = 1,
  size = NULL,
  repeats = 20,
  xreg = NULL,
  lambda = NULL,
  model = NULL,
  subset = NULL,
  scale.inputs = TRUE,
  parallel = FALSE,
  num.cores = 2,
  x = y,
  ...
) {
  useoldmodel <- FALSE
  yname <- deparse1(substitute(y))
  if (!is.null(model)) {
    # Use previously fitted model
    useoldmodel <- TRUE
    # Check for conflicts between new and old data:
    # Check model class
    if (!is.nnetar(model)) {
      stop("Model must be a nnetar object")
    }
    # Check new data
    m <- max(round(frequency(model$x)), 1L)
    minlength <- max(c(model$p, model$P * m)) + 1
    if (length(x) < minlength) {
      stop(paste(
        "Series must be at least of length",
        minlength,
        "to use fitted model"
      ))
    }
    if (tsp(as.ts(x))[3] != m) {
      warning(paste(
        "Data frequency doesn't match fitted model, coercing to frequency =",
        m
      ))
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
    size <- model$size
    p <- model$p
    P <- model$P
    if (p == 0 && P == 0) {
      stop("Both p = 0 and P = 0 in supplied 'model' object")
    }
    if (P > 0) {
      lags <- sort(unique(c(seq_len(p), m * (seq_len(P)))))
    } else {
      lags <- seq_len(p)
    }
    if (is.null(model$scalex)) {
      scale.inputs <- FALSE
    }
  } else {
    # when not using an old model
    if (length(y) < 3) {
      stop("Not enough data to fit a model")
    }
    # Check for constant data in time series
    constant_data <- is.constant(na.interp(x))
    if (constant_data) {
      warning(
        "Constant data, setting p=1, P=0, lambda=NULL, scale.inputs=FALSE"
      )
      scale.inputs <- FALSE
      lambda <- NULL
      p <- 1
      P <- 0
    }
    ## Check for constant data in xreg
    if (!is.null(xreg)) {
      constant_xreg <- any(apply(as.matrix(xreg), 2, function(x) {
        is.constant(na.interp(x))
      }))
      if (constant_xreg) {
        warning("Constant xreg column, setting scale.inputs=FALSE")
        scale.inputs <- FALSE
      }
    }
  }

  # Check for NAs in x
  if (anyNA(x)) {
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
    } else {
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
    if (anyNA(xreg)) {
      warning("Missing values in xreg, omitting rows")
    }
    # Scale xreg
    if (scale.inputs) {
      if (useoldmodel) {
        scalexreg <- model$scalexreg
      } else {
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
      # For non-seasonal data also use default calculation for p if that
      # argument is 0, but issue a warning
      if (p == 0) {
        warning(
          "Cannot set p = 0 for non-seasonal data; using default calculation for p"
        )
        p <- max(length(ar(na.interp(xx))$ar), 1)
      }
      if (p >= n) {
        warning("Reducing number of lagged inputs due to short series")
        p <- n - 1
      }
      lags <- seq_len(p)
      if (P > 1) {
        warning("Non-seasonal data, ignoring seasonal lags")
      }
      P <- 0
    } else {
      if (missing(p)) {
        if (n > 2 * m) {
          x.sa <- seasadj(mstl(na.interp(xx)))
        } else {
          x.sa <- na.interp(xx)
        }
        p <- max(length(ar(x.sa)$ar), 1)
      }
      if (p == 0 && P == 0) {
        stop("'p' and 'P' cannot both be zero")
      }
      if (p >= n) {
        warning("Reducing number of lagged inputs due to short series")
        p <- n - 1
      }
      if (P > 0 && n >= m * P + 2) {
        lags <- sort(unique(c(seq_len(p), m * (seq_len(P)))))
      } else {
        lags <- seq_len(p)
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
  for (i in 1:nlag) {
    lags.X[, i] <- xx[(maxlag - lags[i] + 1):(n - lags[i])]
  }
  # Add xreg into lagged matrix
  lags.X <- cbind(lags.X, xxreg[-(1:maxlag), , drop = FALSE])
  if (is.null(size)) {
    size <- round((NCOL(lags.X) + 1) / 2)
  }
  # Remove missing values if present
  j <- complete.cases(lags.X, y)
  ## Remove values not in subset
  j <- j & xsub[-(1:maxlag)]
  ## Stop if there's no data to fit (e.g. due to NAs or NaNs)
  if (NROW(lags.X[j, , drop = FALSE]) == 0) {
    stop("No data to fit (possibly due to NA or NaN)")
  }
  ## Fit average ANN.
  if (useoldmodel) {
    fit <- oldmodel_avnnet(
      lags.X[j, , drop = FALSE],
      y[j],
      size = size,
      model = model,
      parallel = parallel,
      num.cores = num.cores
    )
  } else {
    fit <- avnnet(
      lags.X[j, , drop = FALSE],
      y[j],
      size = size,
      repeats = repeats,
      parallel = parallel,
      num.cores = num.cores,
      ...
    )
  }
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$scalex <- scalex
  out$scalexreg <- scalexreg
  out$size <- size
  out$xreg <- xreg
  out$lambda <- lambda
  out$subset <- seq_along(x)[xsub]
  out$model <- fit
  out$nnetargs <- list(...)
  if (useoldmodel) {
    out$nnetargs <- model$nnetargs
  }
  if (NROW(lags.X[j, , drop = FALSE]) == 1) {
    fits <- c(rep(NA_real_, maxlag), mean(sapply(fit, predict)))
  } else {
    fits <- c(rep(NA_real_, maxlag), rowMeans(sapply(fit, predict)))
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
  out$fitted <- copy_msts(out$x, out$fitted)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- yname
  out$method <- paste0("NNAR(", p)
  if (P > 0) {
    out$method <- paste0(out$method, ",", P)
  }
  out$method <- paste0(out$method, ",", size, ")")
  if (P > 0) {
    out$method <- paste0(out$method, "[", m, "]")
  }
  out$call <- match.call()
  structure(out, class = c("fc_model", "nnetar"))
}

# Aggregate several neural network models
avnnet <- function(
  x,
  y,
  repeats,
  parallel,
  num.cores,
  linout = TRUE,
  trace = FALSE,
  ...
) {
  if (parallel) {
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    cl <- makeCluster(num.cores)
    on.exit(stopCluster(cl), add = TRUE)
    mods <- parLapply(cl, seq_len(repeats), function(i) {
      nnet::nnet(x = x, y = y, linout = linout, trace = trace, ...)
    })
    return(structure(mods, class = "nnetarmodels"))
  }

  mods <- vector("list", repeats)
  for (i in seq_len(repeats)) {
    mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, ...)
  }
  structure(mods, class = "nnetarmodels")
}

# Fit old model to new data
oldmodel_avnnet <- function(x, y, size, model, parallel, num.cores) {
  repeats <- length(model$model)
  args <- list(x = x, y = y, size = size, linout = 1, trace = FALSE)
  # include additional nnet arguments
  args <- c(args, model$nnetargs)
  # set iterations to zero (i.e. weights stay fixed)
  args$maxit <- 0

  if (parallel) {
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    cl <- makeCluster(num.cores)
    on.exit(stopCluster(cl), add = TRUE)
    mods <- parLapply(cl, seq_len(repeats), function(i) {
      args$Wts <- model$model[[i]]$wts
      do.call(nnet::nnet, args)
    })
    return(structure(mods, class = "nnetarmodels"))
  }

  mods <- vector("list", repeats)
  for (i in seq_len(repeats)) {
    args$Wts <- model$model[[i]]$wts
    mods[[i]] <- do.call(nnet::nnet, args)
  }
  structure(mods, class = "nnetarmodels")
}

#' @export
print.nnetarmodels <- function(x, ...) {
  cat(paste("\nAverage of", length(x), "networks, each of which is\n"))
  print(x[[1]])
}


#' Forecasting using neural network models
#'
#' Returns forecasts and other information for univariate neural network
#' models.
#'
#' Prediction intervals are calculated through simulations and can be slow.
#' Note that if the network is too complex and overfits the data, the residuals
#' can be arbitrarily small; if used for prediction interval calculations, they
#' could lead to misleadingly small values. It is possible to use out-of-sample
#' residuals to ameliorate this, see examples.
#'
#' @inheritParams forecast.Arima
#' @param object An object of class `nnetar` resulting from a call to
#' [nnetar()].
#' @param PI If `TRUE`, prediction intervals are produced, otherwise only point
#' forecasts are calculated. If `PI` is `FALSE`, then `level`,
#' `fan`, `bootstrap` and `npaths` are all ignored.
#' @param innov Values to use as innovations for prediction intervals. Must be
#' a matrix with `h` rows and `npaths` columns (vectors are coerced
#' into a matrix). If present, `bootstrap` is ignored.
#' @param ... Additional arguments passed to [simulate.nnetar()].
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman and Gabriel Caceres
#' @seealso [nnetar()].
#' @keywords ts
#' @examples
#' ## Fit & forecast model
#' fit <- nnetar(USAccDeaths, size = 2)
#' fcast <- forecast(fit, h = 20)
#' plot(fcast)
#'
#' \dontrun{
#' ## Include prediction intervals in forecast
#' fcast2 <- forecast(fit, h = 20, PI = TRUE, npaths = 100)
#' plot(fcast2)
#'
#' ## Set up out-of-sample innovations using cross-validation
#' fit_cv <- CVar(USAccDeaths, size = 2)
#' res_sd <- sd(fit_cv$residuals, na.rm = TRUE)
#' myinnovs <- rnorm(20 * 100, mean = 0, sd = res_sd)
#' ## Forecast using new innovations
#' fcast3 <- forecast(fit, h = 20, PI = TRUE, npaths = 100, innov = myinnovs)
#' plot(fcast3)
#' }
#'
#' @export
forecast.nnetar <- function(
  object,
  h = if (object$m > 1) 2 * object$m else 10,
  PI = FALSE,
  level = c(80, 95),
  fan = FALSE,
  xreg = NULL,
  lambda = object$lambda,
  bootstrap = FALSE,
  npaths = 1000,
  innov = NULL,
  ...
) {
  out <- object
  tspx <- tsp(out$x)
  level <- getConfLevel(level, fan)
  # Check if xreg was used in fitted model
  if (is.null(object$xreg)) {
    if (!is.null(xreg)) {
      warning(
        "External regressors were not used in fitted model, xreg will be ignored"
      )
    }
    xreg <- NULL
  } else {
    if (is.null(xreg)) {
      stop("No external regressors provided")
    }
    xreg <- as.matrix(xreg)
    if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("Number of external regressors does not match fitted model")
    }
    if (!identical(colnames(xreg), colnames(object$xreg))) {
      warning(
        "xreg contains different column names from the xreg used in training. Please check that the regressors are in the same order."
      )
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
      xxreg <- scale(
        xreg,
        center = object$scalexreg$center,
        scale = object$scalexreg$scale
      )
    }
  }

  # Get lags used in fitted model
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(xx, n = maxlag))
  # Iterative 1-step forecast
  for (i in 1:h) {
    newdata <- c(flag[lags], xxreg[i, ])
    if (anyNA(newdata)) {
      fcast[i] <- NA_real_
    } else {
      fcast[i] <- mean(sapply(object$model, predict, newdata = newdata))
    }
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
    hilo <- simulate_forecast(
      object = object,
      h = h,
      level = level,
      npaths = npaths,
      bootstrap = bootstrap,
      innov = innov,
      xreg = xreg,
      lambda = lambda,
      ...
    )
    lower <- ts(hilo$lower)
    upper <- ts(hilo$upper)
    out$lower <- future_msts(out$x, lower)
    out$upper <- future_msts(out$x, upper)
  } else {
    level <- NULL
    lower <- NULL
    upper <- NULL
  }
  out$mean <- future_msts(out$x, fcast)
  out$level <- level

  structure(out, class = "forecast")
}

#' @rdname fitted.Arima
#' @export
fitted.nnetar <- function(object, h = 1, ...) {
  if (h == 1) {
    object$fitted
  } else {
    hfitted(object = object, h = h, FUN = "nnetar", ...)
  }
}

#' @export
print.nnetar <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  # cat("  one hidden layer with",x$size,"nodes\n")
  cat("Call:   ")
  print(x$call)
  print(x$model)
  cat(
    "\nsigma^2 estimated as ",
    format(mean(residuals(x)^2, na.rm = TRUE), digits = digits),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @rdname is.ets
#' @export
is.nnetar <- function(x) {
  inherits(x, "nnetar")
}

#' @rdname is.ets
#' @export
is.nnetarmodels <- function(x) {
  inherits(x, "nnetarmodels")
}

# Scale a univariate time series
#' @export
scale.ts <- function(x, center = TRUE, scale = TRUE) {
  tspx <- tsp(x)
  x <- as.ts(scale.default(x, center = center, scale = scale))
  tsp(x) <- tspx
  x
}

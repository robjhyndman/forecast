# Author: srazbash
###############################################################################

#' BATS model (Exponential smoothing state space model with Box-Cox
#' transformation, ARMA errors, Trend and Seasonal components)
#'
#' Fits a BATS model applied to `y`, as described in De Livera, Hyndman &
#' Snyder (2011). Parallel processing is used by default to speed up the
#' computations.
#'
#' @aliases as.character.bats print.bats
#'
#' @inheritParams forecast.ts
#' @param y The time series to be forecast. Can be `numeric`, `msts`
#' or `ts`. Only univariate time series are supported.
#' @param use.box.cox `TRUE`/`FALSE` indicates whether to use the Box-Cox
#' transformation or not. If `NULL` then both are tried and the best fit
#' is selected by AIC.
#' @param use.trend `TRUE`/`FALSE` indicates whether to include a trend or
#' not. If `NULL` then both are tried and the best fit is selected by AIC.
#' @param use.damped.trend `TRUE`/`FALSE` indicates whether to include a
#' damping parameter in the trend or not. If `NULL` then both are tried
#' and the best fit is selected by AIC.
#' @param seasonal.periods If `y` is `numeric`, then seasonal periods can
#' be specified with this parameter.
#' @param use.arma.errors `TRUE`/`FALSE` indicates whether to include ARMA
#' errors or not. If `TRUE` the best fit is selected by AIC. If
#' `FALSE` then the selection algorithm does not consider ARMA errors.
#' @param use.parallel `TRUE`/`FALSE` indicates whether or not to use
#' parallel processing.
#' @param num.cores The number of parallel processes to be used if using
#' parallel processing. If `NULL` then the number of logical cores is
#' detected and all available cores are used.
#' @param bc.lower The lower limit (inclusive) for the Box-Cox transformation.
#' @param bc.upper The upper limit (inclusive) for the Box-Cox transformation.
#' @param model Output from a previous call to `bats`. If model is passed,
#' this same model is fitted to `y` without re-estimating any parameters.
#' @param ... Additional arguments to be passed to `auto.arima` when
#' choose an ARMA(p, q) model for the errors. (Note that xreg will be ignored,
#' as will any arguments concerning seasonality and differencing, but arguments
#' controlling the values of p and q will be used.)
#' @return An object of class `bats`. The generic accessor functions
#' `fitted.values` and `residuals` extract useful features of the
#' value returned by `bats` and associated functions. The fitted model is
#' designated BATS(omega, p,q, phi, m1,...mJ) where omega is the Box-Cox
#' parameter and phi is the damping parameter; the error is modelled as an
#' ARMA(p,q) process and m1,...,mJ list the seasonal periods used in the model.
#' @author Slava Razbash and Rob J Hyndman
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
bats <- function(
  y,
  use.box.cox = NULL,
  use.trend = NULL,
  use.damped.trend = NULL,
  seasonal.periods = NULL,
  use.arma.errors = TRUE,
  use.parallel = length(y) > 1000,
  num.cores = 2,
  bc.lower = 0,
  bc.upper = 1,
  biasadj = FALSE,
  model = NULL,
  ...
) {
  if (!is.numeric(y) || NCOL(y) > 1) {
    stop("y should be a univariate time series")
  }

  seriesname <- deparse1(substitute(y))

  origy <- y
  attr_y <- attributes(origy)

  # Get seasonal periods
  if (is.null(seasonal.periods)) {
    if (inherits(y, "msts")) {
      seasonal.periods <- attr(y, "msts")
    } else if (is.ts(y)) {
      seasonal.periods <- frequency(y)
    } else {
      y <- as.ts(y)
      seasonal.periods <- 1
    }
    seasonal.periods <- seasonal.periods[seasonal.periods < length(y)]
    if (length(seasonal.periods) == 0L) {
      seasonal.periods <- 1
    }
  } else {
    # Add ts attributes
    if (!is.ts(y)) {
      y <- msts(y, seasonal.periods)
    }
  }
  seasonal.periods <- unique(pmax(seasonal.periods, 1))
  if (all(seasonal.periods == 1)) {
    seasonal.periods <- NULL
  }

  ny <- length(y)
  y <- na.contiguous(y)
  if (ny != length(y)) {
    warning(
      "Missing values encountered. Using longest contiguous portion of time series"
    )
    if (!is.null(attr_y$tsp)) {
      attr_y$tsp[1:2] <- range(time(y))
    }
  }

  # Refit model if available
  if (!is.null(model)) {
    refitModel <- try(fitPreviousBATSModel(y, model = model), silent = TRUE)
    return(refitModel)
  }

  # Check for constancy
  if (is.constant(y)) {
    fit <- list(
      y = y,
      x = matrix(y, nrow = 1, ncol = ny),
      errors = y * 0,
      fitted.values = y,
      seed.states = matrix(y[1]),
      AIC = -Inf,
      likelihood = -Inf,
      variance = 0,
      alpha = 0.9999,
      method = "BATS",
      call = match.call()
    )
    return(structure(fit, class = c("fc_model", "bats")))
  }

  # Check for non-positive data
  if (any((y <= 0))) {
    use.box.cox <- FALSE
  }

  if (!is.null(use.box.cox) && !is.null(use.trend) && use.parallel) {
    if (use.trend && (!is.null(use.damped.trend))) {
      # In the this case, there is only one alternative.
      use.parallel <- FALSE
    } else if (!use.trend) {
      # As above, in the this case, there is only one alternative.
      use.parallel <- FALSE
    }
  }

  if (!is.null(seasonal.periods)) {
    seasonal.mask <- (seasonal.periods == 1)
    seasonal.periods <- seasonal.periods[!seasonal.mask]
  }
  # Check if there is anything to parallelise
  if (
    is.null(seasonal.periods) && !is.null(use.box.cox) && !is.null(use.trend)
  ) {
    use.parallel <- FALSE
  }

  if (is.null(use.box.cox)) {
    use.box.cox <- c(FALSE, TRUE)
  }
  if (any(use.box.cox)) {
    init.box.cox <- BoxCox.lambda(y, lower = bc.lower, upper = bc.upper)
  } else {
    init.box.cox <- NULL
  }
  if (is.null(use.trend)) {
    use.trend <- c(FALSE, TRUE)
  } else if (!use.trend) {
    use.damped.trend <- FALSE
  }
  if (is.null(use.damped.trend)) {
    use.damped.trend <- c(FALSE, TRUE)
  }

  y <- as.numeric(y)

  if (use.parallel) {
    # Set up the control array
    control.array <- NULL
    for (box.cox in use.box.cox) {
      for (trend in use.trend) {
        for (damping in use.damped.trend) {
          if (!trend && damping) {
            next
          }
          control.line <- c(box.cox, trend, damping)
          if (!is.null(control.array)) {
            control.array <- rbind(control.array, control.line)
          } else {
            control.array <- control.line
          }
        }
      }
    }
    ## Fit the models
    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }
    clus <- makeCluster(num.cores)
    models.list <- clusterApplyLB(
      clus,
      seq_len(nrow(control.array)),
      parFilterSpecifics,
      y = y,
      control.array = control.array,
      seasonal.periods = seasonal.periods,
      use.arma.errors = use.arma.errors,
      init.box.cox = init.box.cox,
      bc.lower = bc.lower,
      bc.upper = bc.upper,
      biasadj = biasadj,
      ...
    )
    stopCluster(clus)
    ## Choose the best model
    #### Get the AICs
    aics <- numeric(nrow(control.array))
    for (i in seq_len(nrow(control.array))) {
      aics[i] <- models.list[[i]]$AIC
    }
    best.number <- which.min(aics)
    best.model <- models.list[[best.number]]
  } else {
    best.aic <- Inf
    best.model <- NULL
    for (box.cox in use.box.cox) {
      for (trend in use.trend) {
        for (damping in use.damped.trend) {
          current.model <- try(
            filterSpecifics(
              y,
              box.cox = box.cox,
              trend = trend,
              damping = damping,
              seasonal.periods = seasonal.periods,
              use.arma.errors = use.arma.errors,
              init.box.cox = init.box.cox,
              bc.lower = bc.lower,
              bc.upper = bc.upper,
              biasadj = biasadj,
              ...
            ),
            silent = TRUE
          )
          if (!inherits(current.model, "try-error")) {
            if (current.model$AIC < best.aic) {
              best.aic <- current.model$AIC
              best.model <- current.model
            }
          }
        }
      }
    }
  }
  if (is.null(best.model)) {
    stop("Unable to fit a model")
  }
  best.model$call <- match.call()
  if (best.model$optim.return.code != 0) {
    warning("optim() did not converge.")
  }

  attributes(best.model$fitted.values) <- attributes(
    best.model$errors
  ) <- attr_y
  best.model$y <- origy
  best.model$series <- seriesname
  best.model$method <- "BATS"
  best.model
}

filterSpecifics <- function(
  y,
  box.cox,
  trend,
  damping,
  seasonal.periods,
  use.arma.errors,
  force.seasonality = FALSE,
  init.box.cox = NULL,
  bc.lower = 0,
  bc.upper = 1,
  biasadj = FALSE,
  ...
) {
  if (!trend && damping) {
    return(list(AIC = Inf))
  }

  first.model <- fitSpecificBATS(
    y,
    use.box.cox = box.cox,
    use.beta = trend,
    use.damping = damping,
    seasonal.periods = seasonal.periods,
    init.box.cox = init.box.cox,
    bc.lower = bc.lower,
    bc.upper = bc.upper,
    biasadj = biasadj
  )
  if (!is.null(seasonal.periods) && !force.seasonality) {
    non.seasonal.model <- fitSpecificBATS(
      y,
      use.box.cox = box.cox,
      use.beta = trend,
      use.damping = damping,
      seasonal.periods = NULL,
      init.box.cox = init.box.cox,
      bc.lower = bc.lower,
      bc.upper = bc.upper,
      biasadj = biasadj
    )
    if (first.model$AIC > non.seasonal.model$AIC) {
      seasonal.periods <- NULL
      first.model <- non.seasonal.model
    }
  }
  if (use.arma.errors) {
    suppressWarnings(
      arma <- auto.arima(as.numeric(first.model$errors), d = 0, ...)
    )
    p <- arma$arma[1]
    q <- arma$arma[2]
    if (p != 0 || q != 0) {
      # Did auto.arima() find any AR() or MA() coefficients?
      if (p != 0) {
        ar.coefs <- numeric(p)
      } else {
        ar.coefs <- NULL
      }
      if (q != 0) {
        ma.coefs <- numeric(q)
      } else {
        ma.coefs <- NULL
      }
      # printCASE(box.cox, trend, damping, seasonal.periods, ar.coefs, ma.coefs, p, q)
      second.model <- fitSpecificBATS(
        y,
        use.box.cox = box.cox,
        use.beta = trend,
        use.damping = damping,
        seasonal.periods = seasonal.periods,
        ar.coefs = ar.coefs,
        ma.coefs = ma.coefs,
        init.box.cox = init.box.cox,
        bc.lower = bc.lower,
        bc.upper = bc.upper,
        biasadj = biasadj
      )
      if (second.model$AIC < first.model$AIC) {
        return(second.model)
      } else {
        return(first.model)
      }
    } else {
      # Else auto.arima() did not find any AR() or MA()coefficients
      return(first.model)
    }
  } else {
    return(first.model)
  }
}

parFilterSpecifics <- function(
  control.number,
  control.array,
  y,
  seasonal.periods,
  use.arma.errors,
  force.seasonality = FALSE,
  init.box.cox = NULL,
  bc.lower = 0,
  bc.upper = 1,
  biasadj = FALSE,
  ...
) {
  box.cox <- control.array[control.number, 1]
  trend <- control.array[control.number, 2]
  damping <- control.array[control.number, 3]

  if (!trend && damping) {
    return(list(AIC = Inf))
  }

  first.model <- fitSpecificBATS(
    y,
    use.box.cox = box.cox,
    use.beta = trend,
    use.damping = damping,
    seasonal.periods = seasonal.periods,
    init.box.cox = init.box.cox,
    bc.lower = bc.lower,
    bc.upper = bc.upper,
    biasadj = biasadj
  )
  if (!is.null(seasonal.periods) && !force.seasonality) {
    non.seasonal.model <- fitSpecificBATS(
      y,
      use.box.cox = box.cox,
      use.beta = trend,
      use.damping = damping,
      seasonal.periods = NULL,
      init.box.cox = init.box.cox,
      bc.lower = bc.lower,
      bc.upper = bc.upper,
      biasadj = biasadj
    )
    if (first.model$AIC > non.seasonal.model$AIC) {
      seasonal.periods <- NULL
      first.model <- non.seasonal.model
    }
  }
  if (use.arma.errors) {
    suppressWarnings(
      arma <- auto.arima(as.numeric(first.model$errors), d = 0, ...)
    )
    p <- arma$arma[1]
    q <- arma$arma[2]
    if (p != 0 || q != 0) {
      # Did auto.arima() find any AR() or MA() coefficients?
      if (p != 0) {
        ar.coefs <- numeric(p)
      } else {
        ar.coefs <- NULL
      }
      if (q != 0) {
        ma.coefs <- numeric(q)
      } else {
        ma.coefs <- NULL
      }
      # printCASE(box.cox, trend, damping, seasonal.periods, ar.coefs, ma.coefs, p, q)
      second.model <- fitSpecificBATS(
        y,
        use.box.cox = box.cox,
        use.beta = trend,
        use.damping = damping,
        seasonal.periods = seasonal.periods,
        ar.coefs = ar.coefs,
        ma.coefs = ma.coefs,
        init.box.cox = init.box.cox,
        bc.lower = bc.lower,
        bc.upper = bc.upper,
        biasadj = biasadj
      )
      if (second.model$AIC < first.model$AIC) {
        return(second.model)
      } else {
        return(first.model)
      }
    } else {
      # Else auto.arima() did not find any AR() or MA()coefficients
      return(first.model)
    }
  } else {
    return(first.model)
  }
}

#' @rdname fitted.Arima
#' @export
fitted.bats <- function(object, h = 1, ...) {
  if (h == 1) {
    object$fitted.values
  } else {
    hfitted(object = object, h = h, FUN = "bats", ...)
  }
}

#' @export
print.bats <- function(x, ...) {
  cat(as.character(x))
  cat("\n")
  cat("\nCall: ")
  print(x$call)
  cat("\nParameters")
  if (!is.null(x$lambda)) {
    cat("\n  Lambda: ")
    cat(round(x$lambda, 6))
  }
  cat("\n  Alpha: ")
  cat(x$alpha)
  if (!is.null(x$beta)) {
    cat("\n  Beta: ")
    cat(x$beta)
    cat("\n  Damping Parameter: ")
    cat(round(x$damping.parameter, 6))
  }
  if (!is.null(x$gamma.values)) {
    cat("\n  Gamma Values: ")
    cat(x$gamma.values)
  }
  if (!is.null(x$ar.coefficients)) {
    cat("\n  AR coefficients: ")
    cat(round(x$ar.coefficients, 6))
  }
  if (!is.null(x$ma.coefficients)) {
    cat("\n  MA coefficients: ")
    cat(round(x$ma.coefficients, 6))
  }
  cat("\n")
  cat("\nSeed States:\n")
  print(x$seed.states)

  cat("\nSigma: ")
  cat(sqrt(x$variance))

  cat("\nAIC: ")
  cat(x$AIC)
  cat("\n")
}


#' Plot components from BATS model
#'
#' Produces a plot of the level, slope and seasonal components from a BATS or
#' TBATS model. The plotted components are Box-Cox transformed using the estimated transformation parameter.
#'
#' @param x Object of class \dQuote{bats/tbats}.
#' @param object Object of class \dQuote{bats/tbats}.
#' @param main Main title for plot.
#' @param range.bars Logical indicating if each plot should have a bar at its
#' right side representing relative size. If `NULL`, automatic selection
#' takes place.
#' @param ... Other plotting parameters passed to [graphics::par()].
#' @return None. Function produces a plot
#' @author Rob J Hyndman
#' @seealso [bats()]], [tbats()]
#' @keywords hplot
#'
#' @export
plot.bats <- function(x, main = "Decomposition by BATS model", ...) {
  # Get original data, transform if necessary
  if (!is.null(x$lambda)) {
    y <- BoxCox(x$y, x$lambda)
  } else {
    y <- x$y
  }

  # Extract states
  out <- cbind(observed = c(y), level = x$x[1, ])
  if (!is.null(x$beta)) {
    out <- cbind(out, slope = x$x[2, ])
  }
  nonseas <- 2 + !is.null(x$beta) # No. non-seasonal columns in out
  nseas <- length(x$gamma.values) # No. seasonal periods
  if (!is.null(x$gamma.values)) {
    seas.states <- x$x[-(1:(1 + !is.null(x$beta))), ]
    j <- cumsum(c(1, x$seasonal.periods))
    for (i in seq_len(nseas)) {
      out <- cbind(out, season = seas.states[j[i], ])
    }
    if (nseas > 1) {
      colnames(out)[nonseas + 1:nseas] <- paste0("season", 1:nseas)
    }
  }

  # Add time series characteristics
  out <- ts(out)
  tsp(out) <- tsp(y)

  # Do the plot
  plot.ts(out, main = main, nc = 1, ...)
}

#' @rdname is.ets
#' @export
is.bats <- function(x) {
  inherits(x, "bats")
}

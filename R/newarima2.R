#' Fit best ARIMA model to univariate time series
#'
#' Returns best ARIMA model according to either AIC, AICc or BIC value. The
#' function conducts a search over possible model within the order constraints
#' provided.
#'
#' The default arguments are designed for rapid estimation of models for many time series.
#' If you are analysing just one time series, and can afford to take some more time, it
#' is recommended that you set \code{stepwise=FALSE} and \code{approximation=FALSE}.
#'
#' Non-stepwise selection can be slow, especially for seasonal data. The stepwise
#' algorithm outlined in Hyndman & Khandakar (2008) is used except that the default
#' method for selecting seasonal differences is now based on an estimate of seasonal
#' strength (Wang, Smith & Hyndman, 2006) rather than the Canova-Hansen test.
#' There are also some other minor variations to the algorithm described in
#' Hyndman and Khandakar (2008).
#'
#' @inheritParams stats::arima
#' @param y a univariate time series
#' @param d Order of first-differencing. If missing, will choose a value based
#' on \code{test}.
#' @param D Order of seasonal-differencing. If missing, will choose a value
#' based on \code{season.test}.
#' @param max.p Maximum value of p
#' @param max.q Maximum value of q
#' @param max.P Maximum value of P
#' @param max.Q Maximum value of Q
#' @param max.order Maximum value of p+q+P+Q if model selection is not
#' stepwise.
#' @param max.d Maximum number of non-seasonal differences
#' @param max.D Maximum number of seasonal differences
#' @param start.p Starting value of p in stepwise procedure.
#' @param start.q Starting value of q in stepwise procedure.
#' @param start.P Starting value of P in stepwise procedure.
#' @param start.Q Starting value of Q in stepwise procedure.
#' @param stationary If \code{TRUE}, restricts search to stationary models.
#' @param seasonal If \code{FALSE}, restricts search to non-seasonal models.
#' @param ic Information criterion to be used in model selection.
#' @param stepwise If \code{TRUE}, will do stepwise selection (faster).
#' Otherwise, it searches over all models. Non-stepwise selection can be very
#' slow, especially for seasonal models.
#' @param nmodels Maximum number of models considered in the stepwise search.
#' @param trace If \code{TRUE}, the list of ARIMA models considered will be
#' reported.
#' @param approximation If \code{TRUE}, estimation is via conditional sums of
#' squares and the information criteria used for model selection are
#' approximated. The final model is still computed using maximum likelihood
#' estimation. Approximation should be used for long time series or a high
#' seasonal period to avoid excessive computation times.
#' @param truncate An integer value indicating how many observations to use in
#' model selection. The last \code{truncate} values of the series are used to
#' select a model when \code{truncate} is not \code{NULL} and
#' \code{approximation=TRUE}. All observations are used if either
#' \code{truncate=NULL} or \code{approximation=FALSE}.
#' @param xreg Optionally, a numerical vector or matrix of external regressors, which
#' must have the same number of rows as \code{y}. (It should not be a data frame.)
#' @param test Type of unit root test to use. See \code{\link{ndiffs}} for
#' details.
#' @param test.args Additional arguments to be passed to the unit root test.
#' @param seasonal.test This determines which method is used to select the number of seasonal differences.
#' The default method is to use a measure of seasonal strength computed from an STL decomposition.
#' Other possibilities involve seasonal unit root tests.
#' @param seasonal.test.args Additional arguments to be passed to the seasonal
#' unit root test.
#' See \code{\link{nsdiffs}} for details.
#' @param allowdrift If \code{TRUE}, models with drift terms are considered.
#' @param allowmean If \code{TRUE}, models with a non-zero mean are considered.
#' @param parallel If \code{TRUE} and \code{stepwise = FALSE}, then the
#' specification search is done in parallel. This can give a significant
#' speedup on multicore machines.
#' @param num.cores Allows the user to specify the amount of parallel processes
#' to be used if \code{parallel = TRUE} and \code{stepwise = FALSE}. If
#' \code{NULL}, then the number of logical cores is automatically detected and
#' all available cores are used.
#' @param x Deprecated. Included for backwards compatibility.
#' @param ... Additional arguments to be passed to \code{\link[stats]{arima}}.
#' @inheritParams forecast.ts
#'
#' @return Same as for \code{\link{Arima}}
#' @author Rob J Hyndman
#' @seealso \code{\link{Arima}}
#' @references Hyndman, RJ and Khandakar, Y (2008) "Automatic time series
#' forecasting: The forecast package for R", \emph{Journal of Statistical
#' Software}, \bold{26}(3).
#'
#' Wang, X, Smith, KA, Hyndman, RJ (2006) "Characteristic-based clustering
#' for time series data", \emph{Data Mining and Knowledge Discovery},
#' \bold{13}(3), 335-364.
#' @keywords ts
#' @examples
#' fit <- auto.arima(WWWusage)
#' plot(forecast(fit,h=20))
#'
#' @export
auto.arima <- function(y, d=NA, D=NA, max.p=5, max.q=5,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=2, start.q=2, start.P=1, start.Q=1,
                       stationary=FALSE, seasonal=TRUE, ic=c("aicc", "aic", "bic"),
                       stepwise=TRUE, nmodels = 94, trace=FALSE,
                       approximation=(length(x) > 150 | frequency(x) > 12),
                       method = NULL, truncate=NULL, xreg=NULL,
                       test=c("kpss", "adf", "pp"), test.args = list(),
                       seasonal.test=c("seas", "ocsb", "hegy", "ch"), seasonal.test.args = list(),
                       allowdrift=TRUE, allowmean=TRUE, lambda=NULL, biasadj=FALSE,
                       parallel=FALSE, num.cores=2, x=y, ...) {
  # Only non-stepwise parallel implemented so far.
  if (stepwise && parallel) {
    warning("Parallel computer is only implemented when stepwise=FALSE, the model will be fit in serial.")
    parallel <- FALSE
  }

  if (trace && parallel) {
    message("Tracing model searching in parallel is not supported.")
    trace <- FALSE
  }

  series <- deparse(substitute(y))
  x <- as.ts(x)
  if (NCOL(x) > 1) {
    stop("auto.arima can only handle univariate time series")
  }

  # Trim leading NAs and find length of non-missing data
  orig.x <- x
  missing <- is.na(x)
  firstnonmiss <- head(which(!missing),1)
  lastnonmiss <- tail(which(!missing),1)
  serieslength <- sum(!missing[firstnonmiss:lastnonmiss])

  # Trim initial missing values
  x <- subset(x, start=firstnonmiss)
  if(!is.null(xreg)) {
    if(!is.numeric(xreg))
      stop("xreg should be a numeric matrix or a numeric vector")
    xreg <- as.matrix(xreg)
    xreg <- xreg[firstnonmiss:NROW(xreg),,drop=FALSE]
  }

  # Check for constant data
  if (is.constant(x)) {
    if(all(is.na(x)))
      stop("All data are missing")
    if (allowmean) {
      fit <- Arima(x, order = c(0, 0, 0), fixed = mean(x, na.rm = TRUE), ...)
    } else {
      fit <- Arima(x, order = c(0, 0, 0), include.mean = FALSE, ...)
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    fit$constant <- TRUE
    return(fit)
  }
  ic <- match.arg(ic)
  test <- match.arg(test)
  seasonal.test <- match.arg(seasonal.test)

  # Only consider non-seasonal models
  if (seasonal) {
    m <- frequency(x)
  } else {
    m <- 1
  }
  if (m < 1) {
    # warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    m <- 1
  }
  else {
    m <- round(m)
  } # Avoid non-integer seasonal periods

  max.p <- min(max.p, floor(serieslength / 3))
  max.q <- min(max.q, floor(serieslength / 3))
  max.P <- min(max.P, floor(serieslength / 3 / m))
  max.Q <- min(max.Q, floor(serieslength / 3 / m))

  # Use AIC if npar <= 3
  # AICc won't work for tiny samples.
  if (serieslength <= 3L) {
    ic <- "aic"
  }

  # Transform data if requested
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }

  # Check xreg and do regression if necessary
  if (!is.null(xreg)) {
    if (is.null(colnames(xreg))) {
      colnames(xreg) <- if (ncol(xreg) == 1) "xreg" else paste("xreg", 1:ncol(xreg), sep = "")
    }
    xregg <- xreg

    xx <- x
    # Check that xreg is not rank deficient
    # First check if any columns are constant
    constant_columns <- apply(xregg, 2, is.constant)
    if (all(constant_columns)) {
      xregg <- NULL
    }
    else{
      if (any(constant_columns)) {
        xregg <- xregg[, -which(constant_columns), drop = FALSE]
      }

      # Now check if it is rank deficient
      sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
      if (min(sv) / sum(sv) < .Machine$double.eps) {
        stop("xreg is rank deficient")
      }

      # Finally find residuals from regression in order
      # to estimate appropriate level of differencing
      j <- !is.na(x) & !is.na(rowSums(xregg))
      xx[j] <- residuals(lm(x ~ xregg))
    }
  }
  else {
    xx <- x
    xregg <- NULL
  }

  # Choose order of differencing
  if (stationary) {
    d <- D <- 0
  }
  if (m == 1) {
    D <- max.P <- max.Q <- 0
  } else if(is.na(D) & length(xx) <= 2*m) {
    D <- 0
  } else if(is.na(D)) {
    D <- do.call("nsdiffs", c(list(xx, test=seasonal.test, max.D=max.D), seasonal.test.args))
    # Make sure xreg is not null after differencing
    if (D > 0 && !is.null(xregg)) {
      diffxreg <- diff(xregg, differences = D, lag = m)
      if (any(apply(diffxreg, 2, is.constant))) {
        D <- D - 1
      }
    }
    # Make sure xx is not all missing after differencing
    if (D > 0) {
      dx <- diff(xx, differences = D, lag = m)
      if (all(is.na(dx)))
        D <- D - 1
    }
  }
  if (D > 0) {
    dx <- diff(xx, differences = D, lag = m)
  } else {
    dx <- xx
  }
  if (!is.null(xregg)) {
    if (D > 0) {
      diffxreg <- diff(xregg, differences = D, lag = m)
    } else {
      diffxreg <- xregg
    }
  }
  if (is.na(d)) {
    d <- do.call("ndiffs", c(list(dx, test = test, max.d = max.d), test.args))
    # Make sure xreg is not null after differencing
    if (d > 0 && !is.null(xregg)) {
      diffxreg <- diff(diffxreg, differences = d, lag = 1)
      if (any(apply(diffxreg, 2, is.constant))) {
        d <- d - 1
      }
    }
    # Make sure dx is not all missing after differencing
    if (d > 0) {
      diffdx <- diff(dx, differences=d, lag=1)
      if(all(is.na(diffdx)))
        d <- d - 1
    }
  }

  # Check number of differences selected
  if (D >= 2) {
    warning("Having more than one seasonal differences is not recommended. Please consider using only one seasonal difference.")
  } else if (D + d > 2) {
    warning("Having 3 or more differencing operations is not recommended. Please consider reducing the total number of differences.")
  }

  if (d > 0) {
    dx <- diff(dx, differences = d, lag = 1)
  }

  if(length(dx) == 0L)
    stop("Not enough data to proceed")
  else if (is.constant(dx)) {
    if (is.null(xreg)) {
      if (D > 0 && d == 0) {
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), include.constant = TRUE, fixed = mean(dx/m, na.rm = TRUE), method = method, ...)
      } else if (D > 0 && d > 0) {
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), method = method, ...)
      } else if (d == 2) {
        fit <- Arima(x, order = c(0, d, 0), method = method, ...)
      } else if (d < 2) {
        fit <- Arima(x, order = c(0, d, 0), include.constant = TRUE, fixed = mean(dx, na.rm = TRUE), method = method, ...)
      } else {
        stop("Data follow a simple polynomial and are not suitable for ARIMA modelling.")
      }
    }
    else # Perfect regression
    {
      if (D > 0) {
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), xreg = xreg, method = method, ...)
      } else {
        fit <- Arima(x, order = c(0, d, 0), xreg = xreg, method = method, ...)
      }
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    return(fit)
  }

  if (m > 1) {
    if (max.P > 0) {
      max.p <- min(max.p, m - 1)
    }
    if (max.Q > 0) {
      max.q <- min(max.q, m - 1)
    }
  }

  # Find constant offset for AIC calculation using white noise model
  if (approximation) {
    if (!is.null(truncate)) {
      tspx <- tsp(x)
      if (length(x) > truncate) {
        x <- ts(tail(x, truncate), end = tspx[2], frequency = tspx[3])
      }
    }
    if (D == 0) {
      fit <- try(stats::arima(x, order = c(0, d, 0), xreg = xreg, ...), silent = TRUE)
    } else {
      fit <- try(stats::arima(
        x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m),
        xreg = xreg, ...
      ), silent = TRUE)
    }
    if (!is.element("try-error", class(fit))) {
      offset <- -2 * fit$loglik - serieslength * log(fit$sigma2)
    } else # Not sure this should ever happen
    {
      # warning("Unable to calculate AIC offset")
      offset <- 0
    }
  }
  else {
    offset <- 0
  }

  allowdrift <- allowdrift & (d + D) == 1
  allowmean <- allowmean & (d + D) == 0

  constant <- allowdrift | allowmean

  if (approximation && trace) {
    cat("\n Fitting models using approximations to speed things up...\n")
  }

  if (!stepwise) {
    bestfit <- search.arima(
      x, d, D, max.p, max.q, max.P, max.Q, max.order, stationary,
      ic, trace, approximation, method = method, xreg = xreg, offset = offset,
      allowdrift = allowdrift, allowmean = allowmean,
      parallel = parallel, num.cores = num.cores, ...
    )
    bestfit$call <- match.call()
    bestfit$call$x <- data.frame(x = x)
    bestfit$lambda <- lambda
    bestfit$x <- orig.x
    bestfit$series <- series
    bestfit$fitted <- fitted.Arima(bestfit)
    if (trace) {
      cat("\n\n Best model:", arima.string(bestfit, padding = TRUE), "\n\n")
    }
    return(bestfit)
  }

  # Starting model
  if (length(x) < 10L) {
    start.p <- min(start.p, 1L)
    start.q <- min(start.q, 1L)
    start.P <- 0L
    start.Q <- 0L
  }
  p <- start.p <- min(start.p, max.p)
  q <- start.q <- min(start.q, max.q)
  P <- start.P <- min(start.P, max.P)
  Q <- start.Q <- min(start.Q, max.Q)

  results <- matrix(NA, nrow = nmodels, ncol = 8)

  bestfit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
  results[1, ] <- c(p, d, q, P, D, Q, constant, bestfit$ic)
  # Null model with possible constant
  fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
  results[2, ] <- c(0, d, 0, 0, D, 0, constant, fit$ic)
  if (fit$ic < bestfit$ic) {
    bestfit <- fit
    p <- q <- P <- Q <- 0
  }
  k <- 2
  # Basic AR model
  if (max.p > 0 || max.P > 0) {
    fit <- myarima(x, order = c(max.p > 0, d, 0), seasonal = c((m > 1) & (max.P > 0), D, 0), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[k+1, ] <- c(max.p > 0, d, 0, (m > 1) & (max.P > 0), D, 0, constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- (max.p > 0)
      P <- (m > 1) & (max.P > 0)
      q <- Q <- 0
    }
    k <- k + 1
  }
  # Basic MA model
  if (max.q > 0 || max.Q > 0) {
    fit <- myarima(x, order = c(0, d, max.q > 0), seasonal = c(0, D, (m > 1) & (max.Q > 0)), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[k+1, ] <- c(0, d, max.q > 0, 0, D, (m > 1) & (max.Q > 0), constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- P <- 0
      Q <- (m > 1) & (max.Q > 0)
      q <- (max.q > 0)
    }
    k <- k + 1
  }
  # Null model with no constant
  if (constant) {
    fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = FALSE, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[k+1, ] <- c(0, d, 0, 0, D, 0, 0, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- q <- P <- Q <- 0
    }
    k <- k + 1
  }

  startk <- 0
  while (startk < k && k < nmodels) {
    startk <- k
    if (P > 0 && newmodel(p, d, q, P - 1, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P - 1)
        next
      }
    }
    if (Q > 0 && newmodel(p, d, q, P, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        next
      }
    }
    if (P < max.P && newmodel(p, d, q, P + 1, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P + 1)
        next
      }
    }
    if (Q < max.Q && newmodel(p, d, q, P, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        next
      }
    }
    if (Q > 0 && P > 0 && newmodel(p, d, q, P - 1, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        P <- (P - 1)
        next
      }
    }
    if (Q < max.Q && P > 0 && newmodel(p, d, q, P - 1, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        P <- (P - 1)
        next
      }
    }
    if (Q > 0 && P < max.P && newmodel(p, d, q, P + 1, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        P <- (P + 1)
        next
      }
    }
    if (Q < max.Q && P < max.P && newmodel(p, d, q, P + 1, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        P <- (P + 1)
        next
      }
    }

    if (p > 0 && newmodel(p - 1, d, q, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p - 1)
        next
      }
    }
    if (q > 0 && newmodel(p, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        next
      }
    }
    if (p < max.p && newmodel(p + 1, d, q, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p + 1)
        next
      }
    }
    if (q < max.q && newmodel(p, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        next
      }
    }
    if (q > 0 && p > 0 && newmodel(p - 1, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        p <- (p - 1)
        next
      }
    }
    if (q < max.q && p > 0 && newmodel(p - 1, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        p <- (p - 1)
        next
      }
    }
    if (q > 0 && p < max.p && newmodel(p + 1, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        p <- (p + 1)
        next
      }
    }
    if (q < max.q && p < max.p && newmodel(p + 1, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        p <- (p + 1)
        next
      }
    }
    if (allowdrift || allowmean) {
      if (newmodel(p, d, q, P, D, Q, !constant, results[1:k, ])) {
        k <- k + 1; if(k>nmodels) next
        fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = !constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
        results[k, ] <- c(p, d, q, P, D, Q, !constant, fit$ic)
        if (fit$ic < bestfit$ic) {
          bestfit <- fit
          constant <- !constant
        }
      }
    }
  }

  if(k > nmodels){
    warning(sprintf("Stepwise search was stopped early due to reaching the model number limit: `nmodels = %i`", nmodels))
  }

  # Refit using ML if approximation used for IC
  if (approximation && !is.null(bestfit$arma)) {
    if (trace) {
      cat("\n\n Now re-fitting the best model(s) without approximations...\n")
    }
    icorder <- order(results[, 8])
    nmodels <- sum(!is.na(results[, 8]))
    for (i in seq(nmodels))
    {
      k <- icorder[i]
      fit <- myarima(
        x, order = c(results[k, 1], d, results[k, 3]),
        seasonal = c(results[k, 4], D, results[k, 6]),
        constant = results[k, 7] == 1,
        ic, trace, approximation = FALSE, method = method, xreg = xreg, ...
      )
      if (fit$ic < Inf) {
        bestfit <- fit
        break
      }
    }
  }

  # Nothing fitted
  if (bestfit$ic == Inf && !isTRUE(method=="CSS")) {
    if (trace) {
      cat("\n")
    }
    stop("No suitable ARIMA model found")
  }

  # Return best fit

  bestfit$x <- orig.x
  bestfit$series <- series
  bestfit$ic <- NULL
  bestfit$call <- match.call()
  bestfit$call$x <- data.frame(x = x)
  bestfit$lambda <- lambda
  bestfit$fitted <- fitted.Arima(bestfit)

  if (trace) {
    cat("\n\n Best model:", arima.string(bestfit, padding = TRUE), "\n\n")
  }

  return(bestfit)
}

# Calls arima from stats package and adds data to the returned object
# Also allows refitting to new data
# and drift terms to be included.
myarima <- function(x, order = c(0, 0, 0), seasonal = c(0, 0, 0), constant=TRUE, ic="aic", trace=FALSE, approximation=FALSE, offset=0, xreg=NULL, method = NULL, ...) {
  # Length of non-missing interior
  missing <- is.na(x)
  firstnonmiss <- head(which(!missing),1)
  lastnonmiss <- tail(which(!missing),1)
  n <- sum(!missing[firstnonmiss:lastnonmiss])
  m <- frequency(x)
  use.season <- (sum(seasonal) > 0) & m > 0
  diffs <- order[2] + seasonal[2]
  if(is.null(method)){
    if (approximation) {
      method <- "CSS"
    } else {
      method <- "CSS-ML"
    }
  }
  if (diffs == 1 && constant) {
    xreg <- `colnames<-`(cbind(drift = 1:length(x), xreg),
      make.unique(c("drift", if(is.null(colnames(xreg)) && !is.null(xreg)) rep("", NCOL(xreg)) else colnames(xreg))))
    if (use.season) {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, seasonal = list(order = seasonal, period = m), xreg = xreg, method = method, ...), silent = TRUE))
    } else {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, xreg = xreg, method = method, ...), silent = TRUE))
    }
  }
  else {
    if (use.season) {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, seasonal = list(order = seasonal, period = m), include.mean = constant, method = method, xreg = xreg, ...), silent = TRUE))
    } else {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, include.mean = constant, method = method, xreg = xreg, ...), silent = TRUE))
    }
  }
  if (is.null(xreg)) {
    nxreg <- 0
  } else {
    nxreg <- ncol(as.matrix(xreg))
  }
  if (!is.element("try-error", class(fit))) {
    nstar <- n - order[2] - seasonal[2] * m
    if (diffs == 1 && constant) {
      # fitnames <- names(fit$coef)
      # fitnames[length(fitnames)-nxreg] <- "drift"
      # names(fit$coef) <- fitnames
      fit$xreg <- xreg
    }
    npar <- length(fit$coef[fit$mask]) + 1
    if (method == "CSS") {
      fit$aic <- offset + nstar * log(fit$sigma2) + 2 * npar
    }
    if (!is.na(fit$aic)) {
      fit$bic <- fit$aic + npar * (log(nstar) - 2)
      fit$aicc <- fit$aic + 2 * npar * (npar + 1) / (nstar - npar - 1)
      fit$ic <- switch(ic, bic = fit$bic, aic = fit$aic, aicc = fit$aicc)
    }
    else {
      fit$aic <- fit$bic <- fit$aicc <- fit$ic <- Inf
    }
    # Adjust residual variance to be unbiased
    fit$sigma2 <- sum(fit$residuals ^ 2, na.rm = TRUE) / (nstar - npar + 1)

    # Check for unit roots
    minroot <- 2
    if (order[1] + seasonal[1] > 0) {
      testvec <- fit$model$phi
      k <- abs(testvec) > 1e-8
      if (sum(k) > 0) {
        last.nonzero <- max(which(k))
      } else {
        last.nonzero <- 0
      }
      if (last.nonzero > 0) {
        testvec <- testvec[1:last.nonzero]
        proots <- try(polyroot(c(1,-testvec)))
        if (!is.element("try-error", class(proots))) {
          minroot <- min(minroot, abs(proots))
        }
        else fit$ic <- Inf
      }
    }
    if (order[3] + seasonal[3] > 0 & fit$ic < Inf) {
      testvec <- fit$model$theta
      k <- abs(testvec) > 1e-8
      if (sum(k) > 0) {
        last.nonzero <- max(which(k))
      } else {
        last.nonzero <- 0
      }
      if (last.nonzero > 0) {
        testvec <- testvec[1:last.nonzero]
        proots <- try(polyroot(c(1,testvec)))
        if (!is.element("try-error", class(proots))) {
          minroot <- min(minroot, abs(proots))
        }
        else fit$ic <- Inf
      }
    }
    # Avoid bad models
    if (minroot < 1 + 1e-2 | checkarima(fit)) {
      fit$ic <- Inf
    }

    fit$xreg <- xreg

    if (trace) {
      cat("\n", arima.string(fit, padding = TRUE), ":", fit$ic)
    }

    return(structure(fit, class = c("forecast_ARIMA", "ARIMA", "Arima")))
  }
  else {
    # Catch errors due to unused arguments
    if (length(grep("unused argument", fit)) > 0L) {
      stop(fit[1])
    }

    if (trace) {
      cat("\n ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")
      if (use.season) {
        cat("(", seasonal[1], ",", seasonal[2], ",", seasonal[3], ")[", m, "]", sep = "")
      }
      if (constant && (order[2] + seasonal[2] == 0)) {
        cat(" with non-zero mean")
      } else if (constant && (order[2] + seasonal[2] == 1)) {
        cat(" with drift        ")
      } else if (!constant && (order[2] + seasonal[2] == 0)) {
        cat(" with zero mean    ")
      } else {
        cat("                   ")
      }
      cat(" :", Inf)
    }
    return(list(ic = Inf))
  }
}

newmodel <- function(p, d, q, P, D, Q, constant, results) {
  n <- nrow(results)
  for (i in 1:n) {
    if(!all(is.na(results[i, seq(7)]))) {
      if (all(c(p, d, q, P, D, Q, constant) == results[i, 1:7])) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

arima.string <- function(object, padding=FALSE) {
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  m <- order[7]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")
  if (m > 1 && sum(order[4:6]) > 0) {
    result <- paste(result, "(", order[4], ",", order[5], ",", order[6], ")[", m, "]", sep = "")
  }
  if (padding && m > 1 && sum(order[4:6]) == 0) {
    result <- paste(result, "         ", sep = "")
    if (m <= 9) {
      result <- paste(result, " ", sep = "")
    } else if (m <= 99) {
      result <- paste(result, "  ", sep = "")
    } else {
      result <- paste(result, "   ", sep = "")
    }
  }
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
      result <- paste(result, "with drift        ")
    } else {
      result <- paste("Regression with", result, "errors")
    }
  }
  else {
    if (is.element("constant", names(object$coef)) || is.element("intercept", names(object$coef))) {
      result <- paste(result, "with non-zero mean")
    } else if (order[2] == 0 && order[5] == 0) {
      result <- paste(result, "with zero mean    ")
    } else {
      result <- paste(result, "                  ")
    }
  }
  if (!padding) {
    # Strip trailing spaces
    result <- gsub("[ ]*$", "", result)
  }
  return(result)
}

#' @export
summary.Arima <- function(object, ...) {
  class(object) <- c("summary.Arima", class(object))
  object
}

#' @export
print.summary.Arima <- function(x, ...) {
  NextMethod()
  cat("\nTraining set error measures:\n")
  print(accuracy(x))
}

# Check that Arima object has positive coefficient variances without returning warnings
checkarima <- function(object) {
  suppressWarnings(test <- any(is.nan(sqrt(diag(object$var.coef)))))
  return(test)
}

#' Is an object constant?
#'
#' Returns true if the object's numerical values do not vary.
#'
#'
#' @param x object to be tested
#' @export
is.constant <- function(x) {
  x <- as.numeric(x)
  y <- rep(x[1], length(x))
  return(isTRUE(all.equal(x, y)))
}

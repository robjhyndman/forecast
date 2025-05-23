search.arima <- function(x, d=NA, D=NA, max.p=5, max.q=5,
                         max.P=2, max.Q=2, max.order=5, stationary=FALSE, ic=c("aic", "aicc", "bic"),
                         trace=FALSE, approximation=FALSE, xreg=NULL, offset=offset, allowdrift=TRUE,
                         allowmean=TRUE, parallel=FALSE, num.cores=2, ...) {
  # dataname <- substitute(x)
  ic <- match.arg(ic)
  m <- frequency(x)

  allowdrift <- allowdrift & (d + D) == 1
  allowmean <- allowmean & (d + D) == 0

  maxK <- (allowdrift | allowmean)

  # Choose model orders
  # Serial - technically could be combined with the code below
  if (!parallel) {
    best.ic <- Inf
    for (i in 0:max.p) {
      for (j in 0:max.q) {
        for (I in 0:max.P) {
          for (J in 0:max.Q) {
            if (i + j + I + J <= max.order) {
              for (K in 0:maxK) {
                fit <- myarima(
                  x, order = c(i, d, j), seasonal = c(I, D, J),
                  constant = (K == 1), trace = trace, ic = ic, approximation = approximation,
                  offset = offset, xreg = xreg, ...
                )
                if (fit$ic < best.ic) {
                  best.ic <- fit$ic
                  bestfit <- fit
                  constant <- (K == 1)
                }
              }
            }
          }
        }
      }
    }
  } else if (parallel) {
    to.check <- WhichModels(max.p, max.q, max.P, max.Q, maxK)

    par.all.arima <- function(l, max.order) {
      .tmp <- UndoWhichModels(l)
      i <- .tmp[1]
      j <- .tmp[2]
      I <- .tmp[3]
      J <- .tmp[4]
      K <- .tmp[5] == 1
      if (i + j + I + J <= max.order) {
        fit <- myarima(
          x, order = c(i, d, j), seasonal = c(I, D, J), constant = (K == 1),
          trace = trace, ic = ic, approximation = approximation, offset = offset, xreg = xreg,
          ...
        )
      }
      if (exists("fit")) {
        return(cbind(fit, K))
      } else {
        return(NULL)
      }
    }

    if (is.null(num.cores)) {
      num.cores <- detectCores()
    }

    all.models <- mclapply(X = to.check, FUN = par.all.arima, max.order=max.order, mc.cores = num.cores)

    # Removing null elements
    all.models <- all.models[!sapply(all.models, is.null)]

    # Choosing best model
    best.ic <- Inf
    for (i in 1:length(all.models)) {
      if (!is.null(all.models[[i]][, 1]$ic) && all.models[[i]][, 1]$ic < best.ic) {
        bestfit <- all.models[[i]][, 1]
        best.ic <- bestfit$ic
        constant <- unlist(all.models[[i]][1, 2])
      }
    }
    class(bestfit) <- c("forecast_ARIMA", "ARIMA", "Arima")
  }

  if (exists("bestfit")) {
    # Refit using ML if approximation used for IC
    if (approximation) {
      if (trace) {
        cat("\n\n Now re-fitting the best model(s) without approximations...\n")
      }
      # constant <- length(bestfit$coef) - ncol(xreg) > sum(bestfit$arma[1:4])
      newbestfit <- myarima(
        x, order = bestfit$arma[c(1, 6, 2)],
        seasonal = bestfit$arma[c(3, 7, 4)], constant = constant, ic,
        trace = FALSE, approximation = FALSE, xreg = xreg, ...
      )
      if (newbestfit$ic == Inf) {
        # Final model is lousy. Better try again without approximation
        # warning("Unable to fit final model using maximum likelihood. AIC value approximated")
        bestfit <- search.arima(
          x, d = d, D = D, max.p = max.p, max.q = max.q,
          max.P = max.P, max.Q = max.Q, max.order = max.order, stationary = stationary,
          ic = ic, trace = trace, approximation = FALSE, xreg = xreg, offset = offset,
          allowdrift = allowdrift, allowmean = allowmean,
          parallel = parallel, num.cores = num.cores, ...
        )
        bestfit$ic <- switch(ic, bic = bestfit$bic, aic = bestfit$aic, aicc = bestfit$aicc)
      }
      else {
        bestfit <- newbestfit
      }
    }
  }
  else {
    stop("No ARIMA model able to be estimated")
  }

  bestfit$x <- x
  bestfit$series <- deparse(substitute(x))
  bestfit$ic <- NULL
  bestfit$call <- match.call()

  if (trace) {
    cat("\n\n")
  }

  return(bestfit)
}

# Set up seasonal dummies using Fourier series
SeasDummy <- function(x) {
  n <- length(x)
  m <- frequency(x)
  if (m == 1) {
    stop("Non-seasonal data")
  }
  tt <- 1:n
  fmat <- matrix(NA, nrow = n, ncol = 2 * m)
  for (i in 1:m) {
    fmat[, 2 * i] <- sin(2 * pi * i * tt / m)
    fmat[, 2 * (i - 1) + 1] <- cos(2 * pi * i * tt / m)
  }
  return(fmat[, 1:(m - 1)])
}

# CANOVA-HANSEN TEST
# Largely based on uroot package code for CH.test()
SD.test <- function(wts, s=frequency(wts)) {
  if (any(is.na(wts))) {
    stop("Series contains missing values. Please choose order of seasonal differencing manually.")
  }
  if (s == 1) {
    stop("Not seasonal data")
  }
  t0 <- start(wts)
  N <- length(wts)
  if (N <= s) {
    stop("Insufficient data")
  }
  frec <- rep(1, as.integer((s + 1) / 2))
  ltrunc <- round(s * (N / 100) ^ 0.25)
  R1 <- as.matrix(SeasDummy(wts))
  lmch <- lm(wts ~ R1, na.action = na.exclude) # run the regression : y(i)=mu+f(i)'gamma(i)+e(i)
  Fhat <- Fhataux <- matrix(nrow = N, ncol = s - 1)
  for (i in 1:(s - 1))
    Fhataux[, i] <- R1[, i] * residuals(lmch)
  for (i in 1:N) {
    for (n in 1:(s - 1))
      Fhat[i, n] <- sum(Fhataux[1:i, n])
  }
  wnw <- 1 - seq(1, ltrunc, 1) / (ltrunc + 1)
  Ne <- nrow(Fhataux)
  Omnw <- 0
  for (k in 1:ltrunc)
    Omnw <- Omnw + (t(Fhataux)[, (k + 1):Ne] %*% Fhataux[1:(Ne - k), ]) * wnw[k]
  Omfhat <- (crossprod(Fhataux) + Omnw + t(Omnw)) / Ne
  sq <- seq(1, s - 1, 2)
  frecob <- rep(0, s - 1)
  for (i in 1:length(frec)) {
    if (frec[i] == 1 && i == as.integer(s / 2)) {
      frecob[sq[i]] <- 1
    }
    if (frec[i] == 1 && i < as.integer(s / 2)) {
      frecob[sq[i]] <- frecob[sq[i] + 1] <- 1
    }
  }
  a <- length(which(frecob == 1))
  A <- matrix(0, nrow = s - 1, ncol = a)
  j <- 1
  for (i in 1:(s - 1)) {
    if (frecob[i] == 1) {
      A[i, j] <- 1
      ifelse(frecob[i] == 1, j <- j + 1, j <- j)
    }
  }
  tmp <- t(A) %*% Omfhat %*% A
  problems <- (min(svd(tmp)$d) < .Machine$double.eps)
  if (problems) {
    stL <- 0
  } else {
    stL <- (1 / N ^ 2) * sum(diag(solve(tmp, tol = 1e-25) %*% t(A) %*% t(Fhat) %*% Fhat %*% A))
  }
  return(stL)
}


#' Forecasting using ARIMA or ARFIMA models
#'
#' Returns forecasts and other information for univariate ARIMA models.
#'
#' For \code{Arima} or \code{ar} objects, the function calls
#' \code{\link[stats]{predict.Arima}} or \code{\link[stats]{predict.ar}} and
#' constructs an object of class "\code{forecast}" from the results. For
#' \code{fracdiff} objects, the calculations are all done within
#' \code{\link{forecast.fracdiff}} using the equations given by Peiris and
#' Perera (1988).
#'
#' @param object An object of class "\code{Arima}", "\code{ar}" or
#' "\code{fracdiff}". Usually the result of a call to
#' \code{\link[stats]{arima}}, \code{\link{auto.arima}},
#' \code{\link[stats]{ar}}, \code{\link{arfima}} or
#' \code{\link[fracdiff]{fracdiff}}.
#' @param h Number of periods for forecasting. If \code{xreg} is used, \code{h}
#' is ignored and the number of forecast periods is set to the number of rows
#' of \code{xreg}.
#' @param level Confidence level for prediction intervals.
#' @param fan If \code{TRUE}, level is set to \code{seq(51,99,by=3)}. This is
#' suitable for fan plots.
#' @param xreg Future values of an regression variables (for class \code{Arima}
#' objects only). A numerical vector or matrix of external regressors; it should not be a data frame.
#' @param bootstrap If \code{TRUE}, then prediction intervals computed using
#' simulation with resampled errors.
#' @param npaths Number of sample paths used in computing simulated prediction
#' intervals when \code{bootstrap=TRUE}.
#' @param ... Other arguments.
#' @inheritParams forecast.ts
#'
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{forecast.Arima}.
#'
#' An object of class "\code{forecast}" is a list containing at least the
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
#' @seealso \code{\link[stats]{predict.Arima}},
#' \code{\link[stats]{predict.ar}}, \code{\link{auto.arima}},
#' \code{\link{Arima}}, \code{\link[stats]{arima}}, \code{\link[stats]{ar}},
#' \code{\link{arfima}}.
#' @references Peiris, M. & Perera, B. (1988), On prediction with fractionally
#' differenced ARIMA models, \emph{Journal of Time Series Analysis},
#' \bold{9}(3), 215-220.
#' @keywords ts
#' @aliases forecast.forecast_ARIMA
#' @examples
#' fit <- Arima(WWWusage,c(3,1,0))
#' plot(forecast(fit))
#'
#' library(fracdiff)
#' x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
#' fit <- arfima(x)
#' plot(forecast(fit,h=30))
#'
#' @export
forecast.Arima <- function(object, h=ifelse(object$arma[5] > 1, 2 * object$arma[5], 10),
                           level=c(80, 95), fan=FALSE, xreg=NULL, lambda=object$lambda, bootstrap=FALSE, npaths=5000, biasadj=NULL, ...) {
  # Check whether there are non-existent arguments
  all.args <- names(formals())
  user.args <- names(match.call())[-1L] # including arguments passed to 3 dots
  check <- user.args %in% all.args
  if (!all(check)) {
    error.args <- user.args[!check]
    warning(sprintf("The non-existent %s arguments will be ignored.", error.args))
  }

  use.drift <- is.element("drift", names(object$coef))
  x <- object$x <- getResponse(object)
  usexreg <- (use.drift | is.element("xreg", names(object))) # | use.constant)

  if (!is.null(xreg) && usexreg) {
    if(!is.numeric(xreg))
      stop("xreg should be a numeric matrix or a numeric vector")
    xreg <- as.matrix(xreg)
    if (is.null(colnames(xreg))) {
      colnames(xreg) <- if (ncol(xreg) == 1) "xreg" else paste("xreg", 1:ncol(xreg), sep = "")
    }

    origxreg <- xreg <- as.matrix(xreg)
    h <- nrow(xreg)
  }
  else {
    if(!is.null(xreg)){
      warning("xreg not required by this model, ignoring the provided regressors")
      xreg <- NULL
    }
    origxreg <- NULL
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
  level <- sort(level)

  if (use.drift) {
    n <- length(x)
    #missing <- is.na(x)
    #firstnonmiss <- head(which(!missing),1)
    #n <- length(x) - firstnonmiss + 1
    if (!is.null(xreg)) {
      xreg <- `colnames<-`(cbind(drift = (1:h) + n, xreg),
        make.unique(c("drift", if(is.null(colnames(xreg)) && !is.null(xreg)) rep("", NCOL(xreg)) else colnames(xreg))))
    } else {
      xreg <- `colnames<-`(as.matrix((1:h) + n), "drift")
    }
  }

  # Check if data is constant
  if (!is.null(object$constant)) {
    if (object$constant) {
      pred <- list(pred = rep(x[1], h), se = rep(0, h))
    } else {
      stop("Strange value of object$constant")
    }
  }
  else if (usexreg) {
    if (is.null(xreg)) {
      stop("No regressors provided")
    }
    object$call$xreg <- getxreg(object)
    if (NCOL(xreg) != NCOL(object$call$xreg)) {
      stop("Number of regressors does not match fitted model")
    }
    if(!identical(colnames(xreg), colnames(object$call$xreg))){
      warning("xreg contains different column names from the xreg used in training. Please check that the regressors are in the same order.")
    }
    pred <- predict(object, n.ahead = h, newxreg = xreg)
  }
  else {
    pred <- predict(object, n.ahead = h)
  }

  # Fix time series characteristics if there are missing values at end of series, or if tsp is missing from pred
  if (!is.null(x)) {
    tspx <- tsp(x)
    nx <- max(which(!is.na(x)))
    if (nx != length(x) || is.null(tsp(pred$pred)) || is.null(tsp(pred$se))) {
      tspx[2] <- time(x)[nx]
      start.f <- tspx[2] + 1 / tspx[3]
      pred$pred <- ts(pred$pred, frequency = tspx[3], start = start.f)
      pred$se <- ts(pred$se, frequency = tspx[3], start = start.f)
    }
  }

  # Compute prediction intervals
  nint <- length(level)
  if (bootstrap) # Compute prediction intervals using simulations
  {
    sim <- matrix(NA, nrow = npaths, ncol = h)
    for (i in 1:npaths)
      sim[i, ] <- simulate(object, nsim = h, bootstrap = TRUE, xreg = origxreg, lambda = lambda)
    lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8)
    upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8)
    if (nint > 1L) {
      lower <- t(lower)
      upper <- t(upper)
    }
    else {
      lower <- matrix(lower, ncol = 1)
      upper <- matrix(upper, ncol = 1)
    }
  }
  else { # Compute prediction intervals via the normal distribution
    lower <- matrix(NA, ncol = nint, nrow = length(pred$pred))
    upper <- lower
    for (i in 1:nint) {
      qq <- qnorm(0.5 * (1 + level[i] / 100))
      lower[, i] <- pred$pred - qq * pred$se
      upper[, i] <- pred$pred + qq * pred$se
    }
    if (!is.finite(max(upper))) {
      warning("Upper prediction intervals are not finite.")
    }
  }
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")
  lower <- ts(lower)
  upper <- ts(upper)
  tsp(lower) <- tsp(upper) <- tsp(pred$pred)
  method <- arima.string(object, padding = FALSE)
  seriesname <- if (!is.null(object$series)) {
    object$series
  }
  else if (!is.null(object$call$x)) {
    object$call$x
  }
  else {
    object$call$y
  }
  fits <- fitted.Arima(object)
  if (!is.null(lambda) && is.null(object$constant)) { # Back-transform point forecasts and prediction intervals
    pred$pred <- InvBoxCox(pred$pred, lambda, biasadj, pred$se^2)
    if (!bootstrap) { # Bootstrapped intervals already back-transformed
      lower <- InvBoxCox(lower, lambda)
      upper <- InvBoxCox(upper, lambda)
    }
  }
  return(structure(
    list(
      method = method, model = object, level = level,
      mean = future_msts(x, pred$pred), lower = future_msts(x, lower),
      upper = future_msts(x, upper), x = x, series = seriesname,
      fitted = copy_msts(x, fits), residuals = copy_msts(x, residuals.Arima(object))
    ),
    class = "forecast"
  ))
}

#' @export
forecast.forecast_ARIMA <- forecast.Arima

#' @rdname forecast.Arima
#' @export
forecast.ar <- function(object, h=10, level=c(80, 95), fan=FALSE, lambda=NULL,
                        bootstrap=FALSE, npaths=5000, biasadj=FALSE, ...) {
  x <- getResponse(object)
  pred <- predict(object, newdata = x, n.ahead = h)
  if (bootstrap) # Recompute se using simulations
  {
    sim <- matrix(NA, nrow = npaths, ncol = h)
    for (i in 1:npaths)
      sim[i, ] <- simulate(object, nsim = h, bootstrap = TRUE)
    pred$se <- apply(sim, 2, sd)
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
  nint <- length(level)
  lower <- matrix(NA, ncol = nint, nrow = length(pred$pred))
  upper <- lower
  for (i in 1:nint) {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- pred$pred - qq * pred$se
    upper[, i] <- pred$pred + qq * pred$se
  }
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")
  method <- paste("AR(", object$order, ")", sep = "")
  f <- frequency(x)
  res <- residuals.ar(object)
  fits <- fitted.ar(object)

  if (!is.null(lambda)) {
    pred$pred <- InvBoxCox(pred$pred, lambda, biasadj, list(level = level, upper = upper, lower = lower))

    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
    fits <- InvBoxCox(fits, lambda)
    x <- InvBoxCox(x, lambda)
  }

  return(structure(
    list(
      method = method, model = object, level = level,
      mean = future_msts(x, pred$pred),
      lower = future_msts(x, lower),
      upper = future_msts(x, upper), x = x, series = deparse(object$call$x),
      fitted = copy_msts(x, fits), residuals = copy_msts(x, res)
    )
    , class = "forecast"
  ))
}

# Find xreg matrix in an Arima object

getxreg <- function(z) {
  # Look in the obvious place first
  if (is.element("xreg", names(z))) {
    return(z$xreg)
  }
  # Next most obvious place
  else if (is.element("xreg", names(z$coef))) {
    return(eval.parent(z$coef$xreg))
  }
  # Now check under call
  else if (is.element("xreg", names(z$call))) {
    return(eval.parent(z$call$xreg))
  }
  # Otherwise check if it exists
  else {
    armapar <- sum(z$arma[1:4]) + is.element("intercept", names(z$coef))
    npar <- length(z$coef)
    if (npar > armapar) {
      stop("It looks like you have an xreg component but I don't know what it is.\n  Please use Arima() or auto.arima() rather than arima().")
    } else { # No xreg used
      return(NULL)
    }
  }
}

#' Errors from a regression model with ARIMA errors
#'
#' Returns time series of the regression residuals from a fitted ARIMA model.
#'
#' This is a deprecated function
#' which is identical to \code{\link{residuals.Arima}(object, type="regression")}
#' Regression residuals are equal to the original data
#' minus the effect of any regression variables. If there are no regression
#' variables, the errors will be identical to the original series (possibly
#' adjusted to have zero mean).
#'
#' @param object An object containing a time series model of class \code{Arima}.
#' @return A \code{ts} object
#' @author Rob J Hyndman
#' @seealso \code{\link{residuals.Arima}}.
#' @keywords ts
#'
#' @export
arima.errors <- function(object) {
  message("Deprecated, use residuals.Arima(object, type='regression') instead")
  residuals.Arima(object, type = "regression")
}

# Return one-step fits

#' h-step in-sample forecasts for time series models.
#'
#' Returns h-step forecasts for the data used in fitting the model.
#'
#' @param object An object of class "\code{Arima}", "\code{bats}",
#' "\code{tbats}", "\code{ets}" or "\code{nnetar}".
#' @param h The number of steps to forecast ahead.
#' @param ... Other arguments.
#' @return A time series of the h-step forecasts.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso \code{\link{forecast.Arima}}, \code{\link{forecast.bats}},
#' \code{\link{forecast.tbats}}, \code{\link{forecast.ets}},
#' \code{\link{forecast.nnetar}}, \code{\link{residuals.Arima}},
#' \code{\link{residuals.bats}}, \code{\link{residuals.tbats}},
#' \code{\link{residuals.ets}}, \code{\link{residuals.nnetar}}.
#' @keywords ts
#' @aliases fitted.forecast_ARIMA
#' @examples
#' fit <- ets(WWWusage)
#' plot(WWWusage)
#' lines(fitted(fit), col='red')
#' lines(fitted(fit, h=2), col='green')
#' lines(fitted(fit, h=3), col='blue')
#' legend("topleft", legend=paste("h =",1:3), col=2:4, lty=1)
#'
#' @export
fitted.Arima <- function(object, h = 1, ...) {
  if (h == 1) {
    x <- getResponse(object)
    if (!is.null(object$fitted)) {
      return(object$fitted)
    }
    else if (is.null(x)) {
      # warning("Fitted values are unavailable due to missing historical data")
      return(NULL)
    }
    else if (is.null(object$lambda)) {
      return(x - object$residuals)
    }
    else {
      fits <- InvBoxCox(BoxCox(x, object$lambda) - object$residuals, object$lambda, NULL, object$sigma2)
      return(fits)
    }
  }
  else {
    return(hfitted(object = object, h = h, FUN = "Arima", ...))
  }
}

#' @export
fitted.forecast_ARIMA <- fitted.Arima

# Calls arima from stats package and adds data to the returned object
# Also allows refitting to new data
# and drift terms to be included.

#' Fit ARIMA model to univariate time series
#'
#' Largely a wrapper for the \code{\link[stats]{arima}} function in the stats
#' package. The main difference is that this function allows a drift term. It
#' is also possible to take an ARIMA model from a previous call to \code{Arima}
#' and re-apply it to the data \code{y}.
#'
#' The fitted model is a regression with ARIMA(p,d,q) errors
#'
#' \deqn{y_t = c + \beta' x_t + z_t}
#'
#' where \eqn{x_t} is a vector of regressors at time \eqn{t} and \eqn{z_t} is an
#' ARMA(p,d,q) error process. If there are no regressors, and \eqn{d=0}, then c
#' is an estimate of the mean of \eqn{y_t}. For more information, see Hyndman &
#' Athanasopoulos (2018). For details of the estimation algorithm, see the
#' \code{\link[stats]{arima}} function in the stats package.
#'
#' @aliases print.ARIMA summary.Arima as.character.Arima
#'
#' @param y a univariate time series of class \code{ts}.
#' @param order A specification of the non-seasonal part of the ARIMA model:
#' the three components (p, d, q) are the AR order, the degree of differencing,
#' and the MA order.
#' @param seasonal A specification of the seasonal part of the ARIMA model,
#' plus the period (which defaults to frequency(y)). This should be a list with
#' components order and period, but a specification of just a numeric vector of
#' length 3 will be turned into a suitable list with the specification as the
#' order.
#' @param xreg Optionally, a numerical vector or matrix of external regressors,
#' which must have the same number of rows as y. It should not be a data frame.
#' @param include.mean Should the ARIMA model include a mean term? The default
#' is \code{TRUE} for undifferenced series, \code{FALSE} for differenced ones
#' (where a mean would not affect the fit nor predictions).
#' @param include.drift Should the ARIMA model include a linear drift term?
#' (i.e., a linear regression with ARIMA errors is fitted.)  The default is
#' \code{FALSE}.
#' @param include.constant If \code{TRUE}, then \code{include.mean} is set to
#' be \code{TRUE} for undifferenced series and \code{include.drift} is set to
#' be \code{TRUE} for differenced series. Note that if there is more than one
#' difference taken, no constant is included regardless of the value of this
#' argument. This is deliberate as otherwise quadratic and higher order
#' polynomial trends would be induced.
#' @param method Fitting method: maximum likelihood or minimize conditional
#' sum-of-squares. The default (unless there are missing values) is to use
#' conditional-sum-of-squares to find starting values, then maximum likelihood.
#' @param model Output from a previous call to \code{Arima}. If model is
#' passed, this same model is fitted to \code{y} without re-estimating any
#' parameters.
#' @param x Deprecated. Included for backwards compatibility.
#' @param ... Additional arguments to be passed to \code{\link[stats]{arima}}.
#' @inheritParams forecast.ts
#' @return See the \code{\link[stats]{arima}} function in the stats package.
#' The additional objects returned are \item{x}{The time series data}
#' \item{xreg}{The regressors used in fitting (when relevant).}
#' \item{sigma2}{The bias adjusted MLE of the innovations variance.}
#'
#' @export
#' @references Hyndman, R.J. and Athanasopoulos, G. (2018)
#' "Forecasting: principles and practice", 2nd ed., OTexts, Melbourne, Australia.
#' \url{https://OTexts.com/fpp2/}.
#' @author Rob J Hyndman
#' @seealso \code{\link{auto.arima}}, \code{\link{forecast.Arima}}.
#' @keywords ts
#' @examples
#' library(ggplot2)
#' WWWusage %>%
#'   Arima(order=c(3,1,0)) %>%
#'   forecast(h=20) %>%
#'   autoplot
#'
#' # Fit model to first few years of AirPassengers data
#' air.model <- Arima(window(AirPassengers,end=1956+11/12),order=c(0,1,1),
#'                    seasonal=list(order=c(0,1,1),period=12),lambda=0)
#' plot(forecast(air.model,h=48))
#' lines(AirPassengers)
#'
#' # Apply fitted model to later data
#' air.model2 <- Arima(window(AirPassengers,start=1957),model=air.model)
#'
#' # Forecast accuracy measures on the log scale.
#' # in-sample one-step forecasts.
#' accuracy(air.model)
#' # out-of-sample one-step forecasts.
#' accuracy(air.model2)
#' # out-of-sample multi-step forecasts
#' accuracy(forecast(air.model,h=48,lambda=NULL),
#'          log(window(AirPassengers,start=1957)))
#'
Arima <- function(y, order=c(0, 0, 0), seasonal=c(0, 0, 0), xreg=NULL, include.mean=TRUE,
                  include.drift=FALSE, include.constant, lambda=model$lambda, biasadj=FALSE,
                  method=c("CSS-ML", "ML", "CSS"), model=NULL, x=y, ...) {
  # Remove outliers near ends
  # j <- time(x)
  # x <- na.contiguous(x)
  # if(length(j) != length(x))
  #    warning("Missing values encountered. Using longest contiguous portion of time series")

  series <- deparse(substitute(y))

  origx <- y
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")

    if (is.null(attr(lambda, "biasadj"))) {
      attr(lambda, "biasadj") <- biasadj
    }
  }

  if (!is.null(xreg)) {
    if(!is.numeric(xreg))
      stop("xreg should be a numeric matrix or a numeric vector")
    xreg <- as.matrix(xreg)
    if (is.null(colnames(xreg))) {
      colnames(xreg) <- if (ncol(xreg) == 1) "xreg" else paste("xreg", 1:ncol(xreg), sep = "")
    }
  }

  if (!is.list(seasonal)) {
    if (frequency(x) <= 1) {
      seasonal <- list(order = c(0, 0, 0), period = NA)
      if(length(x) <= order[2L])
        stop("Not enough data to fit the model")
    } else {
      seasonal <- list(order = seasonal, period = frequency(x))
      if(length(x) <= order[2L] + seasonal$order[2L] * seasonal$period)
        stop("Not enough data to fit the model")
    }
  }

  if (!missing(include.constant)) {
    if (include.constant) {
      include.mean <- TRUE
      if ((order[2] + seasonal$order[2]) == 1) {
        include.drift <- TRUE
      }
    }
    else {
      include.mean <- include.drift <- FALSE
    }
  }
  if ((order[2] + seasonal$order[2]) > 1 && include.drift) {
    warning("No drift term fitted as the order of difference is 2 or more.")
    include.drift <- FALSE
  }

  if (!is.null(model)) {
    tmp <- arima2(x, model, xreg = xreg, method = method)
    xreg <- tmp$xreg
    tmp$fitted <- NULL
    tmp$lambda <- model$lambda
  }
  else {
    if (include.drift) {
      xreg <- `colnames<-`(cbind(drift = 1:length(x), xreg),
                           make.unique(c("drift", if(is.null(colnames(xreg)) && !is.null(xreg)) rep("", NCOL(xreg)) else colnames(xreg))))
    }
    if (is.null(xreg)) {
      suppressWarnings(tmp <- stats::arima(x = x, order = order, seasonal = seasonal, include.mean = include.mean, method = method, ...))
    } else {
      suppressWarnings(tmp <- stats::arima(x = x, order = order, seasonal = seasonal, xreg = xreg, include.mean = include.mean, method = method, ...))
    }
  }

  # Calculate aicc & bic based on tmp$aic
  npar <- length(tmp$coef[tmp$mask]) + 1
  missing <- is.na(tmp$residuals)
  firstnonmiss <- head(which(!missing),1)
  lastnonmiss <- tail(which(!missing),1)
  n <- sum(!missing[firstnonmiss:lastnonmiss])
  nstar <- n - tmp$arma[6] - tmp$arma[7] * tmp$arma[5]
  tmp$aicc <- tmp$aic + 2 * npar * (nstar / (nstar - npar - 1) - 1)
  tmp$bic <- tmp$aic + npar * (log(nstar) - 2)
  tmp$series <- series
  tmp$xreg <- xreg
  tmp$call <- match.call()
  tmp$lambda <- lambda
  tmp$x <- origx

  # Adjust residual variance to be unbiased
  if (is.null(model)) {
    tmp$sigma2 <- sum(tmp$residuals ^ 2, na.rm = TRUE) / (nstar - npar + 1)
  }
  out <- structure(tmp, class = c("forecast_ARIMA", "ARIMA", "Arima"))
  out$fitted <- fitted.Arima(out)
  out$series <- series
  return(out)
}

# Refits the model to new data x
arima2 <- function(x, model, xreg, method) {
  use.drift <- is.element("drift", names(model$coef))
  use.intercept <- is.element("intercept", names(model$coef))
  use.xreg <- is.element("xreg", names(model$call))
  sigma2 <- model$sigma2
  if (use.drift) {
    driftmod <- lm(model$xreg[, "drift"] ~ I(time(as.ts(model$x))))
    newxreg <- driftmod$coefficients[1] + driftmod$coefficients[2] * time(as.ts(x))
    if (!is.null(xreg)) {
      origColNames <- colnames(xreg)
      xreg <- cbind(newxreg, xreg)
      colnames(xreg) <- c("drift", origColNames)
    } else {
      xreg <- as.matrix(data.frame(drift = newxreg))
    }
    use.xreg <- TRUE
  }

  if (!is.null(model$xreg)) {
    if (is.null(xreg)) {
      stop("No regressors provided")
    }
    if (ncol(xreg) != ncol(model$xreg)) {
      stop("Number of regressors does not match fitted model")
    }
  }

  if (model$arma[5] > 1 && sum(abs(model$arma[c(3, 4, 7)])) > 0) # Seasonal model
  {
    if (use.xreg) {
      refit <- Arima(
        x, order = model$arma[c(1, 6, 2)], seasonal = list(order = model$arma[c(3, 7, 4)], period = model$arma[5]),
        include.mean = use.intercept, xreg = xreg, method = method, fixed = model$coef
      )
    } else {
      refit <- Arima(
        x, order = model$arma[c(1, 6, 2)], seasonal = list(order = model$arma[c(3, 7, 4)], period = model$arma[5]),
        include.mean = use.intercept, method = method, fixed = model$coef
      )
    }
  }
  else if (length(model$coef) > 0) # Nonseasonal model with some parameters
  {
    if (use.xreg) {
      refit <- Arima(x, order = model$arma[c(1, 6, 2)], xreg = xreg, include.mean = use.intercept, method = method, fixed = model$coef)
    } else {
      refit <- Arima(x, order = model$arma[c(1, 6, 2)], include.mean = use.intercept, method = method, fixed = model$coef)
    }
  }
  else { # No parameters
    refit <- Arima(x, order = model$arma[c(1, 6, 2)], include.mean = FALSE, method = method)
  }

  refit$var.coef <- matrix(0, length(refit$coef), length(refit$coef))
  if (use.xreg) { # Why is this needed?
    refit$xreg <- xreg
  }
  refit$sigma2 <- sigma2

  return(refit)
}

# Modified version of function print.Arima from stats package
#' @export
print.forecast_ARIMA <- function(x, digits=max(3, getOption("digits") - 3), se=TRUE, ...) {
  cat("Series:", x$series, "\n")
  cat(arima.string(x, padding = FALSE), "\n")
  if (!is.null(x$lambda)) {
    cat("Box Cox transformation: lambda=", x$lambda, "\n")
  }

  # cat("\nCall:", deparse(x$call, width.cutoff=75), "\n", sep=" ")
  #    if(!is.null(x$xreg))
  #    {
  #        cat("\nRegression variables fitted:\n")
  #        xreg <- as.matrix(x$xreg)
  #        for(i in 1:3)
  #            cat("  ",xreg[i,],"\n")
  #        cat("   . . .\n")
  #        for(i in 1:3)
  #            cat("  ",xreg[nrow(xreg)-3+i,],"\n")
  #    }
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && NROW(x$var.coef)) {
      ses <- rep.int(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
    }
    # Change intercept to mean if no regression variables
    j <- match("intercept", colnames(coef))
    if (is.null(x$xreg) && !is.na(j)) {
      colnames(coef)[j] <- "mean"
    }
    print.default(coef, print.gap = 2)
  }
  cm <- x$call$method
  cat("\nsigma^2 = ", format(x$sigma2, digits = digits), sep="")
  if(!is.na(x$loglik))
    cat(":  log likelihood = ", format(round(x$loglik, 2L)), sep = "")
  cat("\n")
  if (is.null(cm) || cm != "CSS") {
    if(!is.na(x$aic)) {
      npar <- length(x$coef[x$mask]) + 1
      missing <- is.na(x$residuals)
      firstnonmiss <- head(which(!missing),1)
      lastnonmiss <- tail(which(!missing),1)
      n <- lastnonmiss - firstnonmiss + 1
      nstar <- n - x$arma[6] - x$arma[7] * x$arma[5]
      bic <- x$aic + npar * (log(nstar) - 2)
      aicc <- x$aic + 2 * npar * (nstar / (nstar - npar - 1) - 1)
      cat("AIC=", format(round(x$aic, 2L)), sep = "")
      cat("   AICc=", format(round(aicc, 2L)), sep = "")
      cat("   BIC=", format(round(bic, 2L)), "\n", sep = "")
    }
  }
  invisible(x)
}



#' Return the order of an ARIMA or ARFIMA model
#'
#' Returns the order of a univariate ARIMA or ARFIMA model.
#'
#'
#' @param object An object of class \dQuote{\code{Arima}}, dQuote\code{ar} or
#' \dQuote{\code{fracdiff}}. Usually the result of a call to
#' \code{\link[stats]{arima}}, \code{\link{Arima}}, \code{\link{auto.arima}},
#' \code{\link[stats]{ar}}, \code{\link{arfima}} or
#' \code{\link[fracdiff]{fracdiff}}.
#' @return A numerical vector giving the values \eqn{p}, \eqn{d} and \eqn{q} of
#' the ARIMA or ARFIMA model. For a seasonal ARIMA model, the returned vector
#' contains the values \eqn{p}, \eqn{d}, \eqn{q}, \eqn{P}, \eqn{D}, \eqn{Q} and
#' \eqn{m}, where \eqn{m} is the period of seasonality.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{ar}}, \code{\link{auto.arima}},
#' \code{\link{Arima}}, \code{\link[stats]{arima}}, \code{\link{arfima}}.
#' @keywords ts
#' @examples
#' WWWusage %>% auto.arima %>% arimaorder
#'
#' @export
arimaorder <- function(object) {
  if (is.Arima(object)) {
    order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
    names(order) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
    seasonal <- (order[7] > 1 & sum(order[4:6]) > 0)
    if (seasonal) {
      return(order)
    } else {
      return(order[1:3])
    }
  }
  else if (inherits(object, "ar")) {
    return(c("p" = object$order, "d" = 0, "q" = 0))
  }
  else if (inherits(object, "fracdiff")) {
    return(c("p" = length(object$ar), "d" = object$d, "q" = length(object$ma)))
  }
  else {
    stop("object not of class Arima, ar or fracdiff")
  }
}

#' @export
as.character.Arima <- function(x, ...) {
  arima.string(x, padding = FALSE)
}

#' @rdname is.ets
#' @export
is.Arima <- function(x) {
  inherits(x, "Arima")
}

#' @rdname fitted.Arima
#' @export
fitted.ar <- function(object, ...) {
  getResponse(object) - residuals(object)
}

#' @export
hfitted.Arima <- function(object, h, ...) {
  # As implemented in Fable
  if(h == 1){
    return(object$fitted)
  }
  y <- object$fitted+residuals(object, "innovation")
  yx <- residuals(object, "regression")
  # Get fitted model
  mod <- object$model
  # Reset model to initial state
  mod <-  stats::makeARIMA(mod$phi, mod$theta, mod$Delta)
  # Calculate regression component
  xm <- y - yx
  fits <- rep_len(NA_real_, length(y))

  start <- length(mod$Delta) + 1
  end <- length(yx) - h
  idx <- if(start > end) integer(0L) else start:end
  for(i in idx) {
    fc_mod <- attr(stats::KalmanRun(yx[seq_len(i)], mod, update = TRUE), "mod")
    fits[i + h] <- stats::KalmanForecast(h, fc_mod)$pred[h] + xm[i+h]
  }
  fits <- ts(fits)
  tsp(fits) <- tsp(object$x)
  fits
}

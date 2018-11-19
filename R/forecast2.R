# Mean forecast


#' Mean Forecast
#'
#' Returns forecasts and prediction intervals for an iid model applied to y.
#'
#' The iid model is \deqn{Y_t=\mu + Z_t}{Y[t]=mu + Z[t]} where \eqn{Z_t}{Z[t]}
#' is a normal iid error. Forecasts are given by \deqn{Y_n(h)=\mu}{Y[n+h]=mu}
#' where \eqn{\mu}{mu} is estimated by the sample mean.
#'
#' @param y a numeric vector or time series of class \code{ts}
#' @param h Number of periods for forecasting
#' @param level Confidence levels for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param bootstrap If TRUE, use a bootstrap method to compute prediction intervals.
#' Otherwise, assume a normal distribution.
#' @param npaths Number of bootstrapped sample paths to use if \code{bootstrap==TRUE}.
#' @param x Deprecated. Included for backwards compatibility.
#' @inheritParams forecast
#' 
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{meanf}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
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
#' @seealso \code{\link{rwf}}
#' @keywords ts
#' @examples
#' nile.fcast <- meanf(Nile, h=10)
#' plot(nile.fcast)
#'
#' @export
meanf <- function(y, h=10, level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=FALSE,
                  bootstrap=FALSE, npaths=5000, x=y) {
  n <- length(x)
  if (!is.null(lambda)) {
    origx <- x
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
  }
  meanx <- mean(x, na.rm = TRUE)
  fits <- rep(meanx, length(x))
  res <- x - fits
  f <- rep(meanx, h)
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }
  nconf <- length(level)
  s <- sd(x, na.rm = TRUE)
  if (bootstrap) {
    e <- na.omit(res) - mean(res, na.rm = TRUE)
    sim <- matrix(sample(e, size = npaths * h, replace = TRUE), ncol = npaths, nrow = h)
    sim <- sweep(sim, 1, f, "+")
    lower <- t(apply(sim, 1, quantile, prob = .5 - level / 200))
    upper <- t(apply(sim, 1, quantile, prob = .5 + level / 200))
  }
  else {
    lower <- upper <- matrix(NA, nrow = h, ncol = nconf)
    for (i in 1:nconf)
    {
      if (n > 1) {
        tfrac <- qt(0.5 - level[i] / 200, n - 1)
      } else {
        tfrac <- -Inf
      }
      w <- -tfrac * s * sqrt(1 + 1 / n)
      lower[, i] <- f - w
      upper[, i] <- f + w
    }
  }
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")
  if (is.ts(x)) {
    fits <- ts(fits)
    res <- ts(res)
    tsp(fits) <- tsp(res) <- tsp(x)
    freq <- frequency(x)
    f <- ts(f, start = tsp(x)[2] + 1 / freq, frequency = freq)
    lower <- ts(lower, start = tsp(x)[2] + 1 / freq, frequency = freq)
    upper <- ts(upper, start = tsp(x)[2] + 1 / freq, frequency = freq)
  }

  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
    x <- origx
    f <- InvBoxCox(f, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }

  out <- list(
    method = "Mean", level = level, x = x, series = deparse(substitute(y)), mean = f, lower = lower, upper = upper,
    model = structure(list(mu = f[1], mu.se = s / sqrt(length(x)), sd = s, bootstrap = bootstrap), class = "meanf"), lambda = lambda, fitted = fits, residuals = res
  )
  out$model$call <- match.call()

  return(structure(out, class = "forecast"))
}




#' Box Cox Transformation
#'
#' BoxCox() returns a transformation of the input variable using a Box-Cox
#' transformation. InvBoxCox() reverses the transformation.
#'
#' The Box-Cox transformation is given by \deqn{f_\lambda(x) =\frac{x^\lambda -
#' 1}{\lambda}}{f(x;lambda)=(x^lambda - 1)/lambda} if \eqn{\lambda\ne0}{lambda
#' is not equal to 0}. For \eqn{\lambda=0}{lambda=0},
#' \deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
#'
#' @param x a numeric vector or time series of class \code{ts}.
#' @param lambda transformation parameter. If \code{lambda = "auto"}, then 
#' the transformation parameter lambda is chosen using BoxCox.lambda.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If transformed data is used to produce forecasts and fitted values,
#' a regular back transformation will result in median forecasts. If biasadj is TRUE,
#' an adjustment will be made to produce mean forecasts and fitted values.
#' @param fvar Optional parameter required if biasadj=TRUE. Can either be the
#' forecast variance, or a list containing the interval \code{level}, and the
#' corresponding \code{upper} and \code{lower} intervals.
#' @return a numeric vector of the same length as x.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso \code{\link{BoxCox.lambda}}
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#' @keywords ts
#' @examples
#'
#' lambda <- BoxCox.lambda(lynx)
#' lynx.fit <- ar(BoxCox(lynx,lambda))
#' plot(forecast(lynx.fit,h=20,lambda=lambda))
#'
#' @export
BoxCox <- function(x, lambda) {
  if (lambda == "auto") {
    lambda <- BoxCox.lambda(x)
  }
  if (lambda < 0) {
    x[x < 0] <- NA
  }
  if (lambda == 0) {
    out <- log(x)
  } else {
    out <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  }
  if (!is.null(colnames(x))) {
    colnames(out) <- colnames(x)
  }
  attr(out, "lambda") <- lambda
  return(out)
}

#' @rdname BoxCox
#' @export
InvBoxCox <- function(x, lambda, biasadj=FALSE, fvar=NULL) {
  if (lambda < 0) {
    x[x > -1 / lambda] <- NA
  }
  if (lambda == 0) {
    out <- exp(x)
  } else {
    xx <- x * lambda + 1
    out <- sign(xx) * abs(xx) ^ (1 / lambda)
  }
  if (!is.null(colnames(x))) {
    colnames(out) <- colnames(x)
  }

  if (is.null(biasadj)) {
    biasadj <- attr(lambda, "biasadj")
  }
  if (!is.logical(biasadj)) {
    warning("biasadj information not found, defaulting to FALSE.")
    biasadj <- FALSE
  }
  if (biasadj) {
    if (is.null(fvar)) {
      stop("fvar must be provided when biasadj=TRUE")
    }
    if (is.list(fvar)) { # Create fvar from forecast interval
      level <- max(fvar$level)
      if (NCOL(fvar$upper) > 1 && NCOL(fvar$lower)) {
        i <- match(level, fvar$level)
        fvar$upper <- fvar$upper[, i]
        fvar$lower <- fvar$lower[, i]
      }
      if (level > 1) {
        level <- level / 100
      }
      level <- mean(c(level, 1))
      # Note: Use BoxCox transformed upper and lower values
      fvar <- as.numeric((fvar$upper - fvar$lower) / stats::qnorm(level) / 2) ^ 2
    }
    if (NCOL(fvar) > 1) {
      fvar <- diag(fvar)
    }
    out <- out * (1 + 0.5 * fvar * (1 - lambda) / (out) ^ (2 * lambda))
  }
  return(out)
}

# Deprecated
InvBoxCoxf <- function(x=NULL, fvar=NULL, lambda=NULL) {
  message("Deprecated, use InvBoxCox instead")
  if (is.null(lambda)) {
    stop("Must specify lambda using lambda=numeric(1)")
  }
  if (is.null(fvar)) {
    level <- max(x$level)
    if (NCOL(x$upper) > 1 && NCOL(x$lower)) {
      i <- match(level, x$level)
      x$upper <- x$upper[, i]
      x$lower <- x$lower[, i]
    }
    if (level > 1) {
      level <- level / 100
    }
    level <- mean(c(level, 1))
    # Note: Use BoxCox transformed upper and lower values
    fvar <- ((x$upper - x$lower) / stats::qnorm(level) / 2) ^ 2
  }
  else {
    x <- list(mean = x)
  }
  if ("matrix" %in% class(fvar)) {
    fvar <- diag(fvar)
  }

  return(x$mean * (1 + 0.5 * fvar * (1 - lambda) / (x$mean) ^ (2 * lambda)))
}



#' Forecasting using Structural Time Series models
#'
#' Returns forecasts and other information for univariate structural time
#' series models.
#'
#' This function calls \code{predict.StructTS} and constructs an object of
#' class "\code{forecast}" from the results.
#'
#' @param object An object of class "\code{StructTS}". Usually the result of a
#' call to \code{\link[stats]{StructTS}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param ... Other arguments.
#' @inheritParams forecast
#' 
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{forecast.StructTS}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
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
#' @seealso \code{\link[stats]{StructTS}}.
#' @keywords ts
#' @examples
#' fit <- StructTS(WWWusage,"level")
#' plot(forecast(fit))
#'
#' @export
forecast.StructTS <- function(object, h=ifelse(object$coef["epsilon"] > 1e-10, 2 * object$xtsp[3], 10), level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=NULL, ...) {
  x <- object$data
  pred <- predict(object, n.ahead = h)
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
  upper <- lower <- matrix(NA, ncol = nint, nrow = length(pred$pred))
  for (i in 1:nint)
  {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- pred$pred - qq * pred$se
    upper[, i] <- pred$pred + qq * pred$se
  }
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")
  if (is.element("seas", names(object$coef))) {
    method <- "Basic structural model"
  } else if (is.element("slope", names(object$coef))) {
    method <- "Local linear structural model"
  } else {
    method <- "Local level structural model"
  }

  fits <- ts(c(x - residuals(object)))
  tsp(fits) <- tsp(x)
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
    x <- InvBoxCox(x, lambda)
    pred$pred <- InvBoxCox(pred$pred, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }


  return(structure(
    list(
      method = method, model = object, level = level, mean = pred$pred,
      lower = lower, upper = upper, x = x, series = object$series, fitted = fits, residuals = x-fits
    ),
    class = "forecast"
  ))
}

#' Forecasting using Holt-Winters objects
#'
#' Returns forecasts and other information for univariate Holt-Winters time
#' series models.
#'
#' This function calls \code{\link[stats]{predict.HoltWinters}} and constructs
#' an object of class "\code{forecast}" from the results.
#'
#' It is included for completeness, but the \code{\link{ets}} is recommended
#' for use instead of \code{\link[stats]{HoltWinters}}.
#'
#' @param object An object of class "\code{HoltWinters}". Usually the result of
#' a call to \code{\link[stats]{HoltWinters}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param ... Other arguments.
#' @inheritParams forecast
#' 
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by
#' \code{forecast.HoltWinters}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted
#' model.} \item{fitted}{Fitted values (one-step forecasts)}
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{predict.HoltWinters}},
#' \code{\link[stats]{HoltWinters}}.
#' @keywords ts
#' @examples
#' fit <- HoltWinters(WWWusage,gamma=FALSE)
#' plot(forecast(fit))
#'
#' @export
forecast.HoltWinters <- function(object, h=ifelse(frequency(object$x) > 1, 2 * frequency(object$x), 10),
                                 level=c(80, 95), fan=FALSE, lambda=NULL, biasadj=NULL, ...) {
  x <- object$x
  if (!is.null(object$exponential)) {
    if (object$exponential) {
      stop("Forecasting for exponential trend not yet implemented.")
    }
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

  pred <- predict(object, n.ahead = h, prediction.interval = TRUE, level = level[1] / 100)
  pmean <- pred[, 1]
  upper <- lower <- matrix(NA, ncol = nint, nrow = length(pred[, 1]))
  se <- (pred[, 2] - pred[, 3]) / (2 * qnorm(0.5 * (1 + level[1] / 100)))
  for (i in 1:nint)
  {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- pmean - qq * se
    upper[, i] <- pmean + qq * se
  }
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")


  if (!is.null(lambda)) {
    fitted <- InvBoxCox(object$fitted[, 1], lambda)
    x <- InvBoxCox(x, lambda)
    pmean <- InvBoxCox(pmean, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }
  else {
    fitted <- object$fitted[, 1]
  }

  # Pad fitted values with NAs
  nf <- length(fitted)
  n <- length(x)
  fitted <- ts(c(rep(NA, n - nf), fitted))
  tsp(fitted) <- tsp(object$x)

  return(structure(
    list(
      method = "HoltWinters", model = object, level = level,
      mean = pmean, lower = lower, upper = upper, x = x, series = deparse(object$call$x),
      fitted = fitted, residuals = x - fitted
    ),
    class = "forecast"
  ))
}


## CROSTON



#' Forecasts for intermittent demand using Croston's method
#'
#' Returns forecasts and other information for Croston's forecasts applied to
#' y.
#'
#' Based on Croston's (1972) method for intermittent demand forecasting, also
#' described in Shenstone and Hyndman (2005). Croston's method involves using
#' simple exponential smoothing (SES) on the non-zero elements of the time
#' series and a separate application of SES to the times between non-zero
#' elements of the time series. The smoothing parameters of the two
#' applications of SES are assumed to be equal and are denoted by \code{alpha}.
#'
#' Note that prediction intervals are not computed as Croston's method has no
#' underlying stochastic model.
#'
#' @param y a numeric vector or time series of class \code{ts}
#' @param h Number of periods for forecasting.
#' @param alpha Value of alpha. Default value is 0.1.
#' @param x Deprecated. Included for backwards compatibility.
#' @return An object of class \code{"forecast"} is a list containing at least
#' the following elements: \item{model}{A list containing information about the
#' fitted model. The first element gives the model used for non-zero demands.
#' The second element gives the model used for times between non-zero demands.
#' Both elements are of class \code{forecast}.} \item{method}{The name of the
#' forecasting method as a character string} \item{mean}{Point forecasts as a
#' time series} \item{x}{The original time series (either \code{object} itself
#' or the time series used to create the model stored as \code{object}).}
#' \item{residuals}{Residuals from the fitted model. That is y minus fitted
#' values.} \item{fitted}{Fitted values (one-step forecasts)}
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{croston} and
#' associated functions.
#' @author Rob J Hyndman
#' @seealso \code{\link{ses}}.
#' @references Croston, J. (1972) "Forecasting and stock control for
#' intermittent demands", \emph{Operational Research Quarterly}, \bold{23}(3),
#' 289-303.
#'
#' Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying
#' Croston's method for intermittent demand forecasting". \emph{Journal of
#' Forecasting}, \bold{24}, 389-402.
#' @keywords ts
#' @examples
#' y <- rpois(20,lambda=.3)
#' fcast <- croston(y)
#' plot(fcast)
#'
#' @export
croston <- function(y, h=10, alpha=0.1, x=y) {
  if (sum(x < 0) > 0) {
    stop("Series should not contain negative values")
  }
  out <- croston2(x, h, alpha)
  out$x <- x
  if (!is.null(out$fitted)) {
    out$residuals <- x - out$fitted
  }
  out$method <- "Croston's method"
  out$series <- deparse(substitute(y))
  return(structure(out, class = "forecast"))
}

croston2 <- function(x, h=10, alpha=0.1, nofits=FALSE) {
  x <- as.ts(x)
  y <- x[x > 0]
  tsp.x <- tsp(x)
  freq.x <- tsp.x[3]
  start.f <- tsp.x[2] + 1 / freq.x
  if (length(y) == 0) # All historical values are equal to zero
  {
    fc <- ts(rep(0, h), start = start.f, frequency = freq.x)
    if (nofits) {
      return(fc)
    } else {
      return(list(mean = fc, fitted = ts(x * 0, start = tsp.x[1], frequency = freq.x)))
    }
  }
  tt <- diff(c(0, (1:length(x))[x > 0])) # Times between non-zero observations
  if (length(y) == 1 && length(tt) == 1) # Only one non-zero observation
  {
    y.f <- list(mean = ts(rep(y, h), start = start.f, frequency = freq.x))
    p.f <- list(mean = ts(rep(tt, h), start = start.f, frequency = freq.x))
  }
  else if (length(y) <= 1 || length(tt) <= 1) { # length(tt)==0 but length(y)>0. How does that happen?
    return(list(mean = ts(rep(NA, h), start = start.f, frequency = freq.x)))
  } else {
    y.f <- ses(y, alpha = alpha, initial = "simple", h = h, PI = FALSE)
    p.f <- ses(tt, alpha = alpha, initial = "simple", h = h, PI = FALSE)
  }
  ratio <- ts(y.f$mean / p.f$mean, start = start.f, frequency = freq.x)
  if (nofits) {
    return(ratio)
  } else {
    n <- length(x)
    fits <- x * NA
    if (n > 1) {
      for (i in 1:(n - 1))
        fits[i + 1] <- croston2(x[1:i], h = 1, alpha = alpha, nofits = TRUE)
    }
    fits <- ts(fits)
    tsp(fits) <- tsp.x
    return(list(mean = ratio, fitted = fits, model = list(demand = y.f, period = p.f)))
  }
}

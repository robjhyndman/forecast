#' Box Cox Transformation
#'
#' BoxCox() returns a transformation of the input variable using a Box-Cox
#' transformation. InvBoxCox() reverses the transformation.
#'
#' The Box-Cox transformation (as given by Bickel & Doksum 1981) is given by
#'
#' \deqn{f_\lambda(x) =(sign(x)|x|^\lambda - 1)/\lambda}{f(x;lambda)=(sign(x)|x|^lambda - 1)/lambda}
#'
#' if \eqn{\lambda\ne0}{lambda is not equal to 0}. For \eqn{\lambda=0}{lambda=0},
#'
#' \deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
#'
#' @param x a numeric vector or time series of class `ts`.
#' @param lambda transformation parameter. If `lambda = "auto"`, then
#' the transformation parameter lambda is chosen using BoxCox.lambda (with a lower bound of -0.9)
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If transformed data is used to produce forecasts and fitted
#' values, a regular back transformation will result in median forecasts. If
#' biasadj is `TRUE`, an adjustment will be made to produce mean forecasts
#' and fitted values.
#' @param fvar Optional parameter required if `biasadj = TRUE`. Can either
#' be the forecast variance, or a list containing the interval `level`,
#' and the corresponding `upper` and `lower` intervals.
#' @return a numeric vector of the same length as x.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso [BoxCox.lambda()]
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#' Bickel, P. J. and Doksum K. A. (1981) An Analysis of Transformations Revisited. \emph{JASA} \bold{76} 296-311.
#' @keywords ts
#' @examples
#'
#' lambda <- BoxCox.lambda(lynx)
#' lynx.fit <- ar(BoxCox(lynx, lambda))
#' plot(forecast(lynx.fit, h = 20, lambda = lambda))
#'
#' @export
BoxCox <- function(x, lambda) {
  if (lambda == "auto") {
    lambda <- BoxCox.lambda(x, lower = -0.9)
  }
  if (lambda < 0) {
    x[x < 0] <- NA
  }
  if (lambda == 0) {
    out <- log(x)
  } else {
    out <- (sign(x) * abs(x)^lambda - 1) / lambda
  }
  if (!is.null(colnames(x))) {
    colnames(out) <- colnames(x)
  }
  attr(out, "lambda") <- lambda
  out
}

#' @rdname BoxCox
#' @export
InvBoxCox <- function(x, lambda, biasadj = FALSE, fvar = NULL) {
  if (lambda < 0) {
    x[x > -1 / lambda] <- NA
  }
  if (lambda == 0) {
    out <- exp(x)
  } else {
    xx <- x * lambda + 1
    out <- sign(xx) * abs(xx)^(1 / lambda)
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
    if (is.list(fvar)) {
      # Create fvar from forecast interval
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
      fvar <- as.numeric((fvar$upper - fvar$lower) / stats::qnorm(level) / 2)^2
    }
    if (NCOL(fvar) > 1) {
      fvar <- diag(fvar)
    }
    out <- out *
      (1 + 0.5 * as.numeric(fvar) * (1 - lambda) / (out)^(2 * lambda))
  }
  out
}

# Deprecated
InvBoxCoxf <- function(x = NULL, fvar = NULL, lambda = NULL) {
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
    fvar <- ((x$upper - x$lower) / stats::qnorm(level) / 2)^2
  } else {
    x <- list(mean = x)
  }
  if (is.matrix(fvar)) {
    fvar <- diag(fvar)
  }

  x$mean * (1 + 0.5 * fvar * (1 - lambda) / (x$mean)^(2 * lambda))
}


#' Forecasting using Structural Time Series models
#'
#' Returns forecasts and other information for univariate structural time
#' series models.
#'
#' This function calls `predict.StructTS` and constructs an object of
#' class `forecast` from the results.
#'
#' @inheritParams forecast.ets
#' @param object An object of class `StructTS`. Usually the result of a
#' call to [stats::StructTS()].
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [stats::StructTS()].
#' @keywords ts
#' @examples
#' fit <- StructTS(WWWusage, "level")
#' plot(forecast(fit))
#'
#' @export
forecast.StructTS <- function(
  object,
  h = if (object$coef["epsilon"] > 1e-10) 2 * object$xtsp[3] else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  ...
) {
  x <- object$data
  pred <- predict(object, n.ahead = h)
  level <- getConfLevel(level, fan)
  nint <- length(level)
  upper <- lower <- matrix(NA, ncol = nint, nrow = length(pred$pred))
  for (i in seq_len(nint)) {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- pred$pred - qq * pred$se
    upper[, i] <- pred$pred + qq * pred$se
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")
  if ("seas" %in% names(object$coef)) {
    method <- "Basic structural model"
  } else if ("slope" %in% names(object$coef)) {
    method <- "Local linear structural model"
  } else {
    method <- "Local level structural model"
  }

  # Compute fitted values and residuals
  sigma2 <- c(predict(object, n.ahead = 1)$se)
  res <- residuals(object) * sigma2
  fits <- x - res

  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
    x <- InvBoxCox(x, lambda)
    pred$pred <- InvBoxCox(
      pred$pred,
      lambda,
      biasadj,
      list(level = level, upper = upper, lower = lower)
    )
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  }

  mean <- future_msts(x, pred$pred)
  lower <- future_msts(x, lower)
  upper <- future_msts(x, upper)
  fits <- copy_msts(x, fits)
  res <- copy_msts(x, res)

  structure(
    list(
      method = method,
      model = object,
      level = level,
      mean = pred$pred,
      lower = lower,
      upper = upper,
      x = x,
      series = object$series,
      fitted = fits,
      residuals = res
    ),
    class = "forecast"
  )
}

#' Forecasting using Holt-Winters objects
#'
#' Returns forecasts and other information for univariate Holt-Winters time
#' series models.
#'
#' This function calls [stats::predict.HoltWinters()] and constructs
#' an object of class `forecast` from the results.
#'
#' It is included for completeness, but the [ets()] is recommended
#' for use instead of [stats::HoltWinters].
#'
#' @inheritParams forecast.ets
#' @param object An object of class `HoltWinters`. Usually the result of
#' a call to [stats::HoltWinters()].
#'
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [stats::predict.HoltWinters], [stats::HoltWinters()].
#' @keywords ts
#' @examples
#' fit <- HoltWinters(WWWusage, gamma = FALSE)
#' plot(forecast(fit))
#'
#' @export
forecast.HoltWinters <- function(
  object,
  h = if (frequency(object$x) > 1) 2 * frequency(object$x) else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  ...
) {
  x <- object$x
  if (!is.null(object$exponential)) {
    if (object$exponential) {
      stop("Forecasting for exponential trend not yet implemented.")
    }
  }

  level <- getConfLevel(level, fan)
  nint <- length(level)

  pred <- predict(
    object,
    n.ahead = h,
    prediction.interval = TRUE,
    level = level[1] / 100
  )
  pmean <- pred[, 1]
  upper <- lower <- matrix(NA, ncol = nint, nrow = length(pred[, 1]))
  se <- (pred[, 2] - pred[, 3]) / (2 * qnorm(0.5 * (1 + level[1] / 100)))
  for (i in seq_len(nint)) {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- pmean - qq * se
    upper[, i] <- pmean + qq * se
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")

  if (!is.null(lambda)) {
    fitted <- InvBoxCox(object$fitted[, 1], lambda)
    x <- InvBoxCox(x, lambda)
    pmean <- InvBoxCox(
      pmean,
      lambda,
      biasadj,
      list(level = level, upper = upper, lower = lower)
    )
    lower <- InvBoxCox(lower, lambda)
    upper <- InvBoxCox(upper, lambda)
  } else {
    fitted <- object$fitted[, 1]
  }

  # Pad fitted values with NAs
  nf <- length(fitted)
  n <- length(x)
  fitted <- ts(c(rep(NA, n - nf), fitted))
  fitted <- copy_msts(object$x, fitted)

  pmean <- future_msts(object$x, pmean)
  lower <- future_msts(object$x, lower)
  upper <- future_msts(object$x, upper)

  structure(
    list(
      method = "HoltWinters",
      model = object,
      level = level,
      mean = pmean,
      lower = lower,
      upper = upper,
      x = x,
      series = deparse(object$call$x),
      fitted = fitted,
      residuals = x - fitted
    ),
    class = "forecast"
  )
}

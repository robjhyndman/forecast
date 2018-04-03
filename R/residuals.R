#' Residuals for various time series models
#'
#' Returns time series of residuals from a fitted model.
#'
#' Innovation residuals correspond to the white noise process that drives the
#' evolution of the time series model. Response residuals are the difference
#' between the observations and the fitted values (equivalent to \code{h}-step
#' forecasts). For functions with no \code{h} argument, \code{h=1}. For
#' homoscedastic models, the innovation residuals and the response residuals
#' for \code{h=1} are identical. Regression residuals are available for
#' regression models with ARIMA errors, and are equal to the original data
#' minus the effect of the regression variables. If there are no regression
#' variables, the errors will be identical to the original series (possibly
#' adjusted to have zero mean).  \code{arima.errors} is a deprecated function
#' which is identical to \code{residuals.Arima(object, type="regression")}.
#' For \code{nnetar} objects, when \code{type="innovations"} and \code{lambda} is used, a
#' matrix of time-series consisting of the residuals from each of the fitted neural networks is returned.
#'
#' @param object An object containing a time series model of class \code{ar},
#' \code{Arima}, \code{bats}, \code{ets}, \code{fracdiff}, \code{nnetar} or
#' \code{stlm}.
#' If \code{object} is of class \code{forecast}, then the function will return
#' \code{object$residuals} if it exists, otherwise it returns the differences between
#' the observations and their fitted values.
#' @param type Type of residual.
#' @param h If \code{type='response'}, then the fitted values are computed for
#' \code{h}-step forecasts.
#' @param ... Other arguments not used.
#' @return A \code{ts} object.
#' @author Rob J Hyndman
#' @seealso \code{\link{fitted.Arima}}, \code{\link{checkresiduals}}.
#' @keywords ts
#'
#' @export
residuals.forecast <- function(object, type=c("innovation", "response"), ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$residuals
  } else {
    getResponse(object) - fitted(object)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.ar <- function(object, type=c("innovation", "response"), ...) {
  type <- match.arg(type)
  # innovation and response residuals are the same for AR models
  object$resid
}

#' @rdname residuals.forecast
#'
#' @examples
#' fit <- Arima(lynx,order=c(4,0,0), lambda=0.5)
#'
#' plot(residuals(fit))
#' plot(residuals(fit, type='response'))
#' @export
residuals.Arima <- function(object, type=c("innovation", "response", "regression"), h=1, ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$residuals
  } else if (type == "response") {
    getResponse(object) - fitted(object, h = h)
  } else {
    x <- getResponse(object)
    if (!is.null(object$lambda)) {
      x <- BoxCox(x, object$lambda)
    }
    xreg <- getxreg(object)
    # Remove intercept
    if (is.element("intercept", names(object$coef))) {
      xreg <- cbind(rep(1, length(x)), xreg)
    }
    # Return errors
    if (is.null(xreg)) {
      return(x)
    } else {
      norder <- sum(object$arma[1:4])
      return(ts(
        c(x - xreg %*% as.matrix(object$coef[(norder + 1):length(object$coef)])),
        frequency = frequency(x), start = start(x)
      ))
    }
  }
}

#' @rdname residuals.forecast
#' @export
residuals.bats <- function(object, type=c("innovation", "response"), h=1, ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$errors
  } else {
    getResponse(object) - fitted(object, h = h)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.tbats <- function(object, type=c("innovation", "response"), h=1, ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$errors
  } else {
    getResponse(object) - fitted(object, h = h)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.ets <- function(object, type=c("innovation", "response"), h=1, ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$residuals
  } else {
    getResponse(object) - fitted(object, h = h)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.fracdiff <- function(object, type=c("innovation", "response"), ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    if (!is.null(object$residuals)) { # Object produced by arfima()
      return(object$residuals)
    } else # Object produced by fracdiff()
    {
      if (is.element("x", names(object))) {
        x <- object$x
      } else {
        x <- eval.parent(parse(text = as.character(object$call)[2]))
      }
      if (!is.null(object$lambda)) {
        x <- BoxCox(x, object$lambda)
      }
      y <- fracdiff::diffseries(x - mean(x), d = object$d)
      fit <- arima(y, order = c(length(object$ar), 0, length(object$ma)), include.mean = FALSE, fixed = c(object$ar, -object$ma))
      return(residuals(fit, type = "innovation"))
    }
  }
  else {
    getResponse(object) - fitted(object)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.nnetar <- function(object, type=c("innovation", "response"), h=1, ...) {
  type <- match.arg(type)
  if (type == "innovation" && !is.null(object$lambda)) {
    res <- matrix(unlist(lapply(object$model, residuals)), ncol = length(object$model))
    if (!is.null(object$scalex$scale)) {
      res <- res * object$scalex$scale
    }
  }
  else {
    res <- getResponse(object) - fitted(object, h = h)
  }

  tspx <- tsp(getResponse(object))
  res <- ts(res, frequency = tspx[3L], end = tspx[2L])

  return(res)
}

#' @rdname residuals.forecast
#' @export
residuals.stlm <- function(object, type=c("innovation", "response"), ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    object$residuals
  } else {
    getResponse(object) - fitted(object)
  }
}

#' @rdname residuals.forecast
#' @export
residuals.tslm <- function(object, ...) {
  object$residuals
}

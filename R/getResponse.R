# Functions to return the response variable for different models.
# If a Box-Cox transformation is used, the series returned here should
# be on the original scale, not the Box-Cox transformed scale.

#' Get response variable from time series model.
#'
#' `getResponse` is a generic function for extracting the historical data from
#' a time series model (including `Arima`, `ets`, `ar`, `fracdiff`), a linear
#' model of class `lm`, or a forecast object. The function invokes particular
#' \emph{methods} which depend on the class of the first argument.
#'
#'
#' @param object a time series model or forecast object.
#' @param ... Additional arguments that are ignored.
#' @return A numerical vector or a time series object of class `ts`.
#' @author Rob J Hyndman
#' @keywords ts
#'
#' @export
getResponse <- function(object, ...) UseMethod("getResponse")

#' @rdname getResponse
#' @export
getResponse.default <- function(object, ...) {
  if (is.list(object)) {
    output <- object$x
    if (is.null(output)) {
      output <- object$y
    }
    return(output)
  } else {
    return(NULL)
  }
}

#' @rdname getResponse
#' @export
getResponse.lm <- function(object, ...) {
  if (!is.null(object[["x"]])) {
    object[["x"]]
  } else {
    responsevar <- deparse(formula(object)[[2]])
    model.frame(object$model)[, responsevar]
  }
}

#' @rdname getResponse
#' @export
getResponse.Arima <- function(object, ...) {
  if ("x" %in% names(object)) {
    x <- object$x
  } else {
    series.name <- object$series
    if (is.null(series.name)) {
      return(NULL)
    } else {
      x <- try(eval.parent(parse(text = series.name)), silent = TRUE)
      if (inherits(x, "try-error")) {
        # Try one level further up the chain
        x <- try(eval.parent(parse(text = series.name), 2), silent = TRUE)
      }
      if (inherits(x, "try-error")) {
        # Give up
        return(NULL)
      }
    }
  }
  as.ts(x)
}

#' @rdname getResponse
#' @export
getResponse.fracdiff <- function(object, ...) {
  if ("x" %in% names(object)) {
    x <- object$x
  } else {
    series.name <- as.character(object$call)[2]
    if (is.null(series.name)) {
      stop("missing original time series")
    } else {
      x <- try(eval.parent(parse(text = series.name)), silent = TRUE)
      if (inherits(x, "try-error")) {
        # Try one level further up the chain
        x <- try(eval.parent(parse(text = series.name), 2), silent = TRUE)
      }
      if (inherits(x, "try-error")) {
        # Give up
        return(NULL)
      }
    }
  }
  as.ts(x)
}

#' @rdname getResponse
#' @export
getResponse.ar <- function(object, ...) {
  getResponse.Arima(object)
}

#' @rdname getResponse
#' @export
getResponse.tbats <- function(object, ...) {
  if ("y" %in% names(object)) {
    y <- object$y
  } else {
    return(NULL)
  }
  as.ts(y)
}

#' @rdname getResponse
#' @export
getResponse.bats <- function(object, ...) {
  getResponse.tbats(object, ...)
}

#' @rdname getResponse
#' @export
getResponse.mforecast <- function(object, ...) {
  do.call(cbind, lapply(object$forecast, function(x) x$x))
}

#' @rdname getResponse
#' @export
getResponse.baggedModel <- function(object, ...) {
  if ("y" %in% names(object)) {
    y <- object$y
  } else {
    return(NULL)
  }
  as.ts(y)
}

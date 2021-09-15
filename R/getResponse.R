# Functions to return the response variable for different models.
# If a Box-Cox transformation is used, the series returned here should
# be on the original scale, not the Box-Cox transformed scale.




#' Get response variable from time series model.
#'
#' \code{getResponse} is a generic function for extracting the historical data
#' from a time series model (including \code{Arima}, \code{ets}, \code{ar},
#' \code{fracdiff}), a linear model of class \code{lm}, or a forecast object.
#' The function invokes particular \emph{methods} which depend on the class of
#' the first argument.
#'
#'
#' @param object a time series model or forecast object.
#' @param ... Additional arguments that are ignored.
#' @return A numerical vector or a time series object of class \code{ts}.
#' @author Rob J Hyndman
#' @keywords ts
#'
#' @export
getResponse <- function(object, ...) UseMethod("getResponse")

#' @rdname getResponse
#' @export
getResponse.default <- function(object, ...) {
  if (is.list(object)) {
    return(object$x)
  } else {
    return(NULL)
  }
}

#' @rdname getResponse
#' @export
getResponse.lm <- function(object, ...) {
  if(!is.null(object[["x"]])){
    object[["x"]]
  }
  else{
    responsevar <- deparse(formula(object)[[2]])
    model.frame(object$model)[, responsevar]
  }
}

#' @rdname getResponse
#' @export
getResponse.Arima <- function(object, ...) {
  if (is.element("x", names(object))) {
    x <- object$x
  } else {
    series.name <- object$series
    if (is.null(series.name)) {
      return(NULL)
    } else {
      x <- try(eval.parent(parse(text = series.name)), silent = TRUE)
      if (is.element("try-error", class(x))) { # Try one level further up the chain
        x <- try(eval.parent(parse(text = series.name), 2), silent = TRUE)
      }
      if (is.element("try-error", class(x))) { # Give up
        return(NULL)
      }
    }
  }
  return(as.ts(x))
}

#' @rdname getResponse
#' @export
getResponse.fracdiff <- function(object, ...) {
  if (is.element("x", names(object))) {
    x <- object$x
  } else {
    series.name <- as.character(object$call)[2]
    if (is.null(series.name)) {
      stop("missing original time series")
    } else {
      x <- try(eval.parent(parse(text = series.name)), silent = TRUE)
      if (is.element("try-error", class(x))) { # Try one level further up the chain
        x <- try(eval.parent(parse(text = series.name), 2), silent = TRUE)
      }
      if (is.element("try-error", class(x))) { # Give up
        return(NULL)
      }
    }
  }
  return(as.ts(x))
}

#' @rdname getResponse
#' @export
getResponse.ar <- function(object, ...) {
  getResponse.Arima(object)
}

#' @rdname getResponse
#' @export
getResponse.tbats <- function(object, ...) {
  if (is.element("y", names(object))) {
    y <- object$y
  } else {
    return(NULL)
  }
  return(as.ts(y))
}

#' @rdname getResponse
#' @export
getResponse.bats <- function(object, ...) {
  return(getResponse.tbats(object, ...))
}

#' @rdname getResponse
#' @export
getResponse.mforecast <- function(object, ...) {
  return(do.call(cbind, lapply(object$forecast, function(x) x$x)))
}

#' @rdname getResponse
#' @export
getResponse.baggedModel <- function(object, ...) {
  if (is.element("y", names(object))) {
    y <- object$y
  } else {
    return(NULL)
  }
  return(as.ts(y))
}

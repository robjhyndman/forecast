#' Check that residuals from a time series model look like white noise
#'
#' If \code{plot=TRUE}, produces a time plot of the residuals, the
#' corresponding ACF, and a histogram. If \code{test} is not \code{FALSE},
#' the output from either a Ljung-Box test or Breusch-Godfrey test is printed.
#'
#' @param object Either a time series model, a forecast object, or a time
#' series (assumed to be residuals).
#' @param lag Number of lags to use in the Ljung-Box or Breusch-Godfrey test.
#' If missing, it is set to \code{min(10,n/5)} for non-seasonal data, and
#' \code{min(2m, n/5)} for seasonal data, where \code{n} is the length of the series,
#' and \code{m} is the seasonal period of the data. It is further constrained to be
#' at least \code{df+3} where \code{df} is the degrees of freedom of the model. This
#' ensures there are at least 3 degrees of freedom used in the chi-squared test.
#' @param test Test to use for serial correlation. By default, if \code{object}
#' is of class \code{lm}, then \code{test="BG"}. Otherwise, \code{test="LB"}.
#' Setting \code{test=FALSE} will prevent the test results being printed.
#' @param plot Logical. If \code{TRUE}, will produce the plot.
#' @param ... Other arguments are passed to \code{\link{ggtsdisplay}}.
#' @return None
#' @author Rob J Hyndman
#' @seealso \code{\link{ggtsdisplay}}, \code{\link[stats]{Box.test}},
#' \code{\link[lmtest]{bgtest}}
#' @examples
#'
#' fit <- ets(WWWusage)
#' checkresiduals(fit)
#'
#' @export
checkresiduals <- function(object, lag, test, plot = TRUE, ...) {
  showtest <- TRUE
  if (missing(test)) {
    if (inherits(object, "lm")) {
      test <- "BG"
    } else {
      test <- "LB"
    }
    showtest <- TRUE
  } else if (test) {
    test <- match.arg(test, c("LB", "BG"))
    showtest <- TRUE
  } else {
    showtest <- FALSE
  }

  # Extract residuals
  if (is.ts(object) || is.numeric(object)) {
    residuals <- object
    object <- list(method = "Missing")
  } else {
    residuals <- residuals(object)
  }

  if (length(residuals) == 0L) {
    stop("No residuals found")
  }

  if (inherits(object, "ar")) {
    method <- paste("AR(", object$order, ")", sep = "")
  } else if (!is.null(object$method)) {
    method <- object$method
  } else if (inherits(object, "HoltWinters")) {
    method <- "HoltWinters"
  } else if (inherits(object, "StructTS")) {
    method <- "StructTS"
  } else {
    method <- try(as.character(object), silent = TRUE)
    if (inherits(method, "try-error")) {
      method <- "Missing"
    } else if (length(method) > 1 || base::nchar(method[1]) > 50) {
      method <- "Missing"
    }
  }
  if (method == "Missing") {
    main <- "Residuals"
  } else {
    main <- paste("Residuals from", method)
  }

  if (plot) {
    suppressWarnings(ggtsdisplay(residuals, plot.type = "histogram", main = main, ...))
  }

  # Check if we have the model
  if (is.forecast(object)) {
    object <- object$model
  }

  if (is.null(object) || !showtest) {
    return(invisible())
  }

  # Seasonality of data
  freq <- frequency(residuals)

  # Find model df
  #if (grepl("STL \\+ ", method)) {
  #  warning("The fitted degrees of freedom is based on the model used for the seasonally adjusted data.")
  #}
  if (inherits(object, "Arima") || test == "BG") {
    df <- modeldf(object)
  } else {
    df <- 0
  }

  if (missing(lag)) {
    lag <- ifelse(freq > 1, 2 * freq, 10)
    lag <- min(lag, round(length(residuals) / 5))
    lag <- max(df + 3, lag)
  }

  if (test == "BG") {
    # Do Breusch-Godfrey test
    BGtest <- lmtest::bgtest(object, order = lag)
    BGtest$data.name <- main
    # print(BGtest)
    return(BGtest)
  } else {
    # Do Ljung-Box test
    LBtest <- Box.test(zoo::na.approx(residuals), fitdf = df, lag = lag, type = "Ljung")
    LBtest$method <- "Ljung-Box test"
    LBtest$data.name <- main
    names(LBtest$statistic) <- "Q*"
    print(LBtest)
    cat(paste("Model df: ", df, ".   Total lags used: ", lag, "\n\n", sep = ""))
    return(invisible(LBtest))
  }
}

#' Compute model degrees of freedom
#'
#' @param object A time series model
#' @param ... Other arguments currently ignored
#' @export
modeldf <- function(object, ...) {
  UseMethod("modeldf")
}

#' @export
modeldf.default <- function(object, ...) {
  warning("Could not find appropriate degrees of freedom for this model.")
  NULL
}

#' @export
modeldf.ets <- function(object, ...) {
  length(object$par)
}

#' @export
modeldf.Arima <- function(object, ...) {
  sum(arimaorder(object)[c("p", "q", "P", "Q")], na.rm = TRUE)
}

#' @export
modeldf.bats <- function(object, ...) {
  length(object$parameters$vect)
}

#' @export
modeldf.lm <- function(object, ...) {
  length(object$coefficients)
}

#' @export
modeldf.lagwalk <- function(object, ...) {
  as.numeric(object$par$includedrift)
}

#' @export
modeldf.meanf <- function(object, ...) {
  1
}

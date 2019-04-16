#' Check that residuals from a time series model look like white noise
#'
#' If \code{plot=TRUE}, produces a time plot of the residuals, the
#' corresponding ACF, and a histogram. If the degrees of freedom for the model
#' can be determined and \code{test} is not \code{FALSE}, the output from
#' either a Ljung-Box test or Breusch-Godfrey test is printed.
#'
#'
#' @param object Either a time series model, a forecast object, or a time
#' series (assumed to be residuals).
#' @param lag Number of lags to use in the Ljung-Box or Breusch-Godfrey test.
#' If missing, it is set to \code{min(10,n/5)} for non-seasonal data, and
#' \code{min(2m, n/5)} for seasonal data, where \code{n} is the length of the series,
#' and \code{m} is the seasonal period of the data. It is further constrained to be
#' at least \code{df+3} where \code{df} is the degrees of freedom of the model. This
#' ensures there are at least 3 degrees of freedom used in the chi-squared test.
#' @param df Number of degrees of freedom for fitted model, required for the
#' Ljung-Box or Breusch-Godfrey test. Ignored if the degrees of freedom can be
#' extracted from \code{object}.
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
checkresiduals <- function(object, lag, df=NULL, test, plot=TRUE, ...) {
  showtest <- TRUE
  if (missing(test)) {
    if (is.element("lm", class(object))) {
      test <- "BG"
    } else {
      test <- "LB"
    }
    showtest <- TRUE
  }
  else if (test != FALSE) {
    test <- match.arg(test, c("LB", "BG"))
    showtest <- TRUE
  }
  else {
    showtest <- FALSE
  }
  
  # Extract residuals
  if (is.element("ts", class(object)) | is.element("numeric", class(object))) {
    residuals <- object
    object <- list(method = "Missing")
  }
  else {
    residuals <- residuals(object)
  }

  if (length(residuals) == 0L) {
    stop("No residuals found")
  }
  
  if ("ar" %in% class(object)) {
    method <- paste("AR(", object$order, ")", sep = "")
  } else if (!is.null(object$method)) {
    method <- object$method
  } else if ("HoltWinters" %in% class(object)) {
    method <- "HoltWinters"
  } else if ("StructTS" %in% class(object)) {
    method <- "StructTS"
  } else {
    method <- try(as.character(object), silent = TRUE)
    if ("try-error" %in% class(method)) {
      method <- "Missing"
    } else if (length(method) > 1 | base::nchar(method[1]) > 50) {
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
  if (is.element("forecast", class(object))) {
    object <- object$model
  }

  if (is.null(object) | !showtest) {
    return(invisible())
  }

  # Seasonality of data
  freq <- frequency(residuals)

  # Find model df
  if(grepl("STL \\+ ", method)){
    warning("The fitted degrees of freedom is based on the model used for the seasonally adjusted data.")
  }
  df <- modeldf(object)
  
  if (missing(lag)) {
    lag <- ifelse(freq > 1, 2 * freq, 10)
    lag <- min(lag, round(length(residuals)/5))
    lag <- max(df+3, lag)
  }

  if (!is.null(df)) {
    if (test == "BG") {
      # Do Breusch-Godfrey test
      BGtest <- lmtest::bgtest(object, order = lag)
      BGtest$data.name <- main
      print(BGtest)
    }
    else {
      # Do Ljung-Box test
      LBtest <- Box.test(zoo::na.approx(residuals), fitdf = df, lag = lag, type = "Ljung")
      LBtest$method <- "Ljung-Box test"
      LBtest$data.name <- main
      names(LBtest$statistic) <- "Q*"
      print(LBtest)
      cat(paste("Model df: ", df, ".   Total lags used: ", lag, "\n\n", sep = ""))
    }
  }
}

modeldf <- function(object, ...){
  UseMethod("modeldf")
}

modeldf.default <- function(object, ...){
  warning("Could not find appropriate degrees of freedom for this model.")
  NULL
}

modeldf.ets <- function(object, ...){
  length(object$par)
}

modeldf.Arima <- function(object, ...){
  length(object$coef)
}

modeldf.bats <- function(object, ...){
  length(object$parameters$vect) + NROW(object$seed.states)
}

modeldf.lm <- function(object, ...){
  length(object$coefficients)
}

modeldf.lagwalk <- function(object, ...){
  as.numeric(object$par$includedrift)
}

modeldf.meanf <- function(object, ...){
  1
}
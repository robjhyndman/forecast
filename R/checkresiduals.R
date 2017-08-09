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
#' If missing, it is set to \code{max(10,df+3)} for non-seasonal data, and
#' \code{max(2m, df+3)} for seasonal data, where \code{df} is the degrees of
#' freedom of the model, and \code{m} is the seasonal period of the data.
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
checkresiduals <- function(object, lag, df=NULL, test, plot=TRUE, ...)
{
  showtest <- TRUE
  if(missing(test))
  {
    if(is.element("lm", class(object)))
      test <- "BG"
    else
      test <- "LB"
    showtest <- TRUE
  }
  else if(test != FALSE)
  {
    test <- match.arg(test, c("LB","BG"))
    showtest <- TRUE
  }
  else
    showtest <- FALSE


  # Extract residuals
  if(is.element("ts",class(object)) | is.element("numeric",class(object)) )
  {
    residuals <- object
    object <- list(method="Missing")
  }
  else
    residuals <- residuals(object)

  if(length(residuals) == 0L)
    stop("No residuals found")

  if("ar" %in% class(object))
    method <- paste("AR(",object$order,")",sep="")
  else if(!is.null(object$method))
    method <- object$method
  else if("HoltWinters" %in% class(object))
    method <- "HoltWinters"
  else if("StructTS" %in% class(object))
    method <- "StructTS"
  else
  {
    method <- try(as.character(object), silent=TRUE)
    if("try-error" %in% class(method))
      method <- "Missing"
    else if(length(method) > 1 | base::nchar(method[1]) > 50)
      method <- "Missing"
  }
  if(method=="Missing")
    main <- "Residuals"
  else
    main <- paste("Residuals from", method)

  if(plot)
  {
    suppressWarnings(ggtsdisplay(residuals, plot.type="histogram", main=main, ...))
  }

  # Check if we have the model
  if(is.element("forecast",class(object)))
    object <- object$model

  if(is.null(object) | !showtest)
    return(invisible())

  # Seasonality of data
  freq <- frequency(residuals)

  # Find model df
  if(is.element("ets",class(object)))
    df <- length(object$par)
  else if(is.element("Arima",class(object)))
    df <- length(object$coef)
  else if(is.element("bats",class(object)))
    df <- length(object$parameters$vect) + NROW(object$seed.states)
  else if(is.element('lm', class(object)))
    df <- length(object$coefficients)
  else if(method=="Mean")
    df <- 1
  else if(grepl("Naive",method, ignore.case=TRUE))
    df <- 0
  else if(method=="Random walk")
    df <- 0
  else if(method=="Random walk with drift")
    df <- 1
  else
    df <- NULL
  if(missing(lag))
  {
    lag <- max(df+3, ifelse(freq>1, 2*freq, 10))
    lag <- min(lag, length(residuals)-1L)
  }

  if(!is.null(df))
  {
    if(test=="BG")
    {
      # Do Breusch-Godfrey test
      BGtest <- lmtest::bgtest(object, order=lag)
      BGtest$data.name <- main
      print(BGtest)
    }
    else
    {
      # Do Ljung-Box test
      LBtest <- Box.test(zoo::na.approx(residuals), fitdf=df, lag=lag, type="Ljung")
      LBtest$method <- "Ljung-Box test"
      LBtest$data.name <- main
      names(LBtest$statistic) <- "Q*"
      print(LBtest)
      cat(paste("Model df: ",df,".   Total lags used: ",lag,"\n\n",sep=""))
    }
  }
}


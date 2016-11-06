checkresiduals <- function(object, lag, df=NULL)
{
  # Extract residuals
  if(is.element("ts",class(object)) | is.element("numeric",class(object)) )
    residuals <- object
  else
    residuals <- residuals(object)

  # Produce plots
  ggtsdisplay(residuals, plot.type="histogram")

  # Check if we have the model
  if(is.element("forecast",class(object)))
    object <- object$model
  if(is.null(object))
    return()

  # Find model df
  if(is.element("ets",class(object)))
    df <- length(object$par)
  else if(is.element("Arima",class(object)))
    df <- length(object$coef)
  else if(is.element("bats",class(object)))
    df <- length(object$parameters$vect) + NROW(object$seed.states)
  else if(object$method=="Mean")
    df <- 1
  else if(grepl("Naive",object$method, ignore.case=TRUE))
    df <- 0
  else if(object$method=="Random walk")
    df <- 0
  else if(object$method=="Random walk with drift")
    df <- 1



  # Do Ljung-Box test
  if(!is.null(df))
  {
    freq <- frequency(residuals)
    if(missing(lag))
      lag <- max(df+3, ifelse(freq>1, 2*freq, 10))
    print(Box.test(residuals, fitdf=df, lag=lag, type="Ljung"))
    cat(paste("Model df: ",df,".   Total lags used: ",lag,"\n\n",sep=""))
  }
}


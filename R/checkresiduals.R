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
    residuals <- object
  else
    residuals <- residuals(object)

  if(length(residuals) == 0L)
    stop("No residuals found")

  if(!is.null(object$method))
    method <- object$method
  else
  {
    method <- try(as.character(object), silent=TRUE)
    if("try-error" %in% class(method))
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


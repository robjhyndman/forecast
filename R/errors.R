## Measures of forecast accuracy
## Forecasts in f. This may be a numerical vector or the output from arima or ets or derivatives.
## Actual values in x
# dx = response variable in historical data
## test enables a subset of x and f to be tested.
testaccuracy <- function(f,x,test="all")
{
  dx <- getResponse(f)
  if(is.data.frame(x))
  {
    responsevar <- as.character(formula(f$model))[2]
    if(is.element(responsevar, colnames(x)))
      x <- x[,responsevar]
    else
      stop("I can't figure out what data to use.")
  }
  if(is.list(f))
  {
    if(is.element("mean",names(f)))
      f <- f$mean
    else
      stop("Unknown list structure")
  }
  if(is.ts(x) & is.ts(f))
  {
    tspf <- tsp(f)
    tspx <- tsp(x)
    start <- max(tspf[1],tspx[1])
    end <- min(tspf[2],tspx[2])
    f <- window(f,start=start,end=end)
    x <- window(x,start=start,end=end)
  }
  n <- length(x)
  if(test=="all")
    test <- 1:n

  ff <- f
  xx <- x

  error <- (xx-ff[1:n])[test]
  pe <- error/xx[test] * 100

  me <- mean(error, na.rm=TRUE)
  mse <- mean(error^2, na.rm=TRUE)
  mae <- mean(abs(error), na.rm=TRUE)
  mape <- mean(abs(pe), na.rm=TRUE)
  mpe <-  mean(pe, na.rm=TRUE)
  out <- c(me,sqrt(mse),mae,mpe,mape)
  names(out) <- c("ME","RMSE","MAE","MPE","MAPE")

  # Compute MASE if historical data available
  if(!is.null(dx))
  {
    if(!is.null(tsp(dx)))
      scale <- mean(abs(diff(dx,lag=max(1,frequency(dx)))),na.rm=TRUE)
    else # not time series
      scale <- mean(abs(dx-mean(dx)),na.rm=TRUE)
    mase <- mean(abs(error/scale))
    out <- c(out,mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if(!is.null(tsp(x)) & n>1)
  {
    fpe <- (c(ff[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    ape <- (c(xx[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    theil <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
    r1 <- acf(error,plot=FALSE,lag.max=2,na.action=na.pass)$acf[2,1,1]
    nj <- length(out)
    out <- c(out,r1,theil)
    names(out)[nj+(1:2)] <- c("ACF1","Theil's U")
  }

  return(out)
}


trainingaccuracy <- function(f,test="all")
{
  # Make sure x is an element of f when f is a fitted model rather than a forecast
  #if(!is.list(f))
  #  stop("f must be a forecast object or a time series model object.")
  dx <- getResponse(f)
  if(is.element("splineforecast",class(f)))
    fits <- f$onestepf
  else
    fits <- fitted(f)    # Don't use f$resid as this may contain multiplicative errors.

  res <- dx-fits
  if(is.numeric(test))
  {
    res <- res[test]
    dx <- dx[test]
  }
  pe <- res/dx * 100 # Percentage error

  me <- mean(res,na.rm=TRUE)
  mse <- mean(res^2,na.rm=TRUE)
  mae <- mean(abs(res),na.rm=TRUE)
  mape <- mean(abs(pe),na.rm=TRUE)
  mpe <-  mean(pe,na.rm=TRUE)
  out <- c(me,sqrt(mse),mae,mpe,mape)
  names(out) <- c("ME","RMSE","MAE","MPE","MAPE")

  # Compute MASE if historical data available
  if(!is.null(dx))
  {
    if(!is.null(tsp(dx)))
      scale <- mean(abs(diff(dx,lag=max(1,frequency(dx)))),na.rm=TRUE)
    else # not time series
      scale <- mean(abs(dx-mean(dx)),na.rm=TRUE)
    mase <- mean(abs(res/scale), na.rm=TRUE)
    out <- c(out,mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if(!is.null(tsp(dx)))
  {
    r1 <- acf(res,plot=FALSE,lag.max=2,na.action=na.pass)$acf[2,1,1]
    nj <- length(out)
    out <- c(out,r1)
    names(out)[nj+1] <- "ACF1"
  }

  return(out)
}

accuracy <- function(f,x,test="all")
{
  if(class(f) == "mforecast")
    return(accuracy.mforecast(f,x,test))

  trainset <- (is.list(f))
  testset <- (!missing(x))
  if(!trainset & !testset)
    stop("Unable to compute forecast accuracy measures")
  if(trainset)
  {
    trainout <- trainingaccuracy(f,test)
    trainnames <- names(trainout)
  }
  else
    trainnames <- NULL
  if(testset)
  {
    testout <- testaccuracy(f,x,test)
    testnames <- names(testout)
  }
  else
    testnames <- NULL
  outnames <- unique(c(trainnames,testnames))

  out <- matrix(NA,nrow=2,ncol=length(outnames))
  colnames(out) <- outnames
  rownames(out) <- c("Training set","Test set")
  if(trainset)
    out[1,names(trainout)] <- trainout
  if(testset)
    out[2,names(testout)] <- testout

  if(!testset)
    out <- out[1,,drop=FALSE]
  if(!trainset)
    out <- out[2,,drop=FALSE]
  return(out)
}

# Compute accuracy for a VAR model (from the vars package)
accuracy.mforecast <- function(object, x, test="all")
{
  fc <- object
  class(fc) <- "forecast"
  vnames <- names(object$mean)
  nox <- missing(x)
  for(i in 1:length(object$mean))
  {
    fc$mean <- object$mean[[i]]
    fc$x <- object$x[,i]
    fc$fitted <- object$fitted[,i]
    if(nox)
      out1 <- accuracy(fc, test=test)
    else
      out1 <- accuracy(fc, x[,i], test)
    rownames(out1) <- paste(vnames[i],rownames(out1))
    if(i==1)
      out <- out1
    else
      out <- rbind(out, out1)
  }
  return(out)
}

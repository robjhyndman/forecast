## Measures of forecast accuracy
## Forecasts in f. This may be a numerical vector or the output from arima or ets or derivatives.
## Actual values in x
# dx = response variable in historical data
## test enables a subset of x and f to be tested.
forecasterrors <- function(f,x,test="all")
{
  dx <- getResponse(f)
  if(is.data.frame(x))
  {
    responsevar <- as.character(formula(f$model))[2]
    x <- x[,responsevar]
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
  me <- mean(error)
  mse <- mean(error^2)
  mae <- mean(abs(error))
  mape <- mean(100*abs(error/xx[test]))
  mpe <-  mean(100*error/xx[test])
  junk <- c(me,sqrt(mse),mae,mpe,mape)
  names(junk) <- c("ME","RMSE","MAE","MPE","MAPE")

  # Compute MASE if historical data available
  if(!is.null(dx))
  {
    if(!is.null(tsp(dx)))
      scale <- mean(abs(diff(dx,lag=frequency(dx))),na.rm=TRUE)
    else # not time series
      scale <- mean(abs(dx-mean(dx)),na.rm=TRUE)
    mase <- mean(abs(error/scale))
    junk <- c(junk,mase)
    names(junk)[length(junk)] <- "MASE"
  }

  # Additional time series measures
  if(!is.null(tsp(x)) & n>1)
  {
    fpe <- (c(ff[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    ape <- (c(xx[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
    theil <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
    r1 <- acf(error,plot=FALSE,lag.max=2)$acf[2,1,1]
    nj <- length(junk)
    junk <- c(junk,r1,theil)
    names(junk)[nj+(1:2)] <- c("ACF1","Theil's U")
  }

  return(junk)
}


accuracy <- function(f,x,test="all")
{
    if(!missing(x))
        return(forecasterrors(f,x,test))
    # Make sure x is an element of f when f is a fitted model rather than a forecast
    if(!is.list(f))
      stop("If no x argument, then f must be a forecast object or a time series model object.")
    f$x <- getResponse(f)
    
    ff <- f$x
    fits <- fitted(f)    # Don't use f$resid as this may contain multiplicative errors.
    res <- ff-fits
    if(is.numeric(test))
    {
      res <- res[test]
      ff <- ff[test]
    }

    pe <- res/ff * 100 # Percentage error
    out <- c(mean(res,na.rm=TRUE), sqrt(mean(res^2,na.rm=TRUE)), mean(abs(res),na.rm=TRUE), mean(pe,na.rm=TRUE), mean(abs(pe),na.rm=TRUE))
    names(out) <- c("ME","RMSE","MAE","MPE","MAPE")
    if(!is.null(tsp(f$x)))
    {
        scale <- mean(abs(diff(f$x)),na.rm=TRUE)
        out <- c(out, mean(abs(res/scale),na.rm=TRUE))
        names(out)[6] <- "MASE"
    }
    return(out)
}

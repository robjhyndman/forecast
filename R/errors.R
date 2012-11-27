## Measures of forecast accuracy
## Forecasts in f. This may be a numerical vector or the output from arima or ets or derivatives.
## Actual values in x
## test enables a subset of x and f to be tested.
forecasterrors <- function(f,x,test="all")
{
  data.x <- dx <- NULL
  if(is.data.frame(x))
  {
    responsevar <- as.character(formula(f$model))[2]
    x <- x[,responsevar]
    dx <- model.frame(f$model)[,responsevar]
  }
  if(is.list(f))
  {
    if(is.element("x",names(f)))
      data.x <- f$x
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
  dx <- data.x

  error <- (xx-ff[1:n])[test]
  me <- mean(error)
  mse <- mean(error^2)
  mae <- mean(abs(error))
  mape <- mean(100*abs(error/xx[test]))
  mpe <-  mean(100*error/xx[test])
  junk <- c(me,sqrt(mse),mae,mpe,mape)
  names(junk) <- c("ME","RMSE","MAE","MPE","MAPE")
  if(!is.null(tsp(data.x)))
  {
    if(!is.null(dx))
    {
      scale <- mean(abs(diff(dx,lag=frequency(data.x))),na.rm=TRUE)
      mase <- mean(abs(error/scale))
      junk <- c(junk,mase)
      names(junk)[6] <- "MASE"
    }
    if(n>1)
    {
      fpe <- (c(ff[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
      ape <- (c(xx[2:n])/c(xx[1:(n-1)]) - 1)[test-1]
      theil <- sqrt(sum((fpe - ape)^2)/sum(ape^2))
      r1 <- acf(error,plot=FALSE,lag.max=2)$acf[2,1,1]
      nj <- length(junk)
      junk <- c(junk,r1,theil)
      names(junk)[nj+(1:2)] <- c("ACF1","Theil's U")
    }
  }
  else # not time series
  {
    if(!is.null(dx))
    {
      scale <- mean(abs(dx-mean(dx)),na.rm=TRUE)
      mase <- mean(abs(error/scale))
      junk <- c(junk,mase)
      names(junk)[6] <- "MASE"
    }
  }
  return(junk)
}


accuracy <- function(f,x,test="all")
{
    if(!missing(x))
        return(forecasterrors(f,x,test))
    # Make sure x is an element of f when f is a fitted model rather than a forecast
    if(is.element("Arima",class(f)) & !is.element("x", names(f)))
        f$x <- eval(parse(text = f$series))
    else if(is.element("lm",class(f)))
    {
      responsevar <- as.character(formula(f$model))[2]
      f$x <- model.frame(f$model)[,responsevar]
    }
    
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

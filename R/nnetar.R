# Defaults:
#For non-seasonal data, p chosen using AIC from linear AR(p) model
#For seasonal data, p=3 and P=1.
#size set to average of number of inputs and number of outputs: (p+P+1)/2

nnetar <- function(x, p, P=1, size, repeats=20)
{
  require(nnet)
  # Scale data
  scale <- max(abs(x))
  xx <- x/scale
  # Set up lagged matrix
  n <- length(xx)
  xx <- as.ts(xx)
  m <- frequency(xx)
  if(m==1)
  {
    if(missing(p))
      p <- length(ar(xx)$ar)
    lags <- 1:p
    P <- 0
  }
  else
  {
    if(missing(p))
    {
      x.sa <- seasadj(stl(xx,s.window=7))
      p <- length(ar(x.sa)$ar)
    }
    if(P > 0)
      lags <- sort(unique(c(1:p,m*(1:P))))
  }
  if(missing(size))
    size <- round((p+P+1)/2)
  maxlag <- max(lags)
  nlag <- length(lags)
  y <- xx[-(1:maxlag)]
  lags.X <- matrix(NA,ncol=nlag,nrow=n-maxlag)
  for(i in 1:nlag)
    lags.X[,i] <- xx[(maxlag-lags[i]+1):(n-lags[i])]
  # Fit ANN a number of times
  fits <- list()
  for(i in 1:repeats)
    fits[[i]] <- nnet(lags.X,y,size=size,linout=1,trace=FALSE)
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$scale <- scale
  out$size <- size
  out$fits <- fits
  out$fitted <- rep(0,length(out$x))
  for(j in 1:repeats)
    out$fitted <- out$fitted + c(rep(NA,maxlag),fits[[j]]$fitted.values)/repeats
  out$fitted <- ts(out$fitted*scale)
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- deparse(substitute(x))
  out$method <- paste("ANN-AR(",p,sep="")
  if(P>0)
    out$method <- paste(out$method,",",P,")",sep="")
  else
    out$method <- paste(out$method,")",sep="")
  out$call <- match.call()
  return(structure(out,class=c("nnetar")))
}

forecast.nnetar <- function(object, h=ifelse(object$m > 1, 2 * object$m, 10), ...)
{
  require(nnet)
  out <- list(method = object$method, x = object$x, model = object)
  out$fitted <- object$fitted
  out$residuals <- object$residuals
  out$model$fitted <- out$model$residuals <- out$model$method <- out$model$x <- NULL
  tspx <- tsp(out$x)

  fcast <- numeric(h)
  flag <- tail(object$x/object$scale, n=max(object$lags))
  rpts <- length(object$fits)
  for(i in 1:h)
  {
    for(j in 1:rpts)
      fcast[i] <- fcast[i] + predict(object$fits[[j]], newdata=flag)/rpts
    flag <- c(flag[-1],fcast[i])
  }
  out$mean <- ts(fcast*object$scale,start=tspx[2]+1/tspx[3],frequency=tspx[3])
  return(structure(out,class="forecast"))
}

print.nnetar <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Series:", x$series, "\n")
    cat("Model: ", x$method, "\n")
    #cat("  one hidden layer with",x$size,"nodes\n")
    cat("Call:   ")
    print(x$call)
    print(x$fits[[1]])
    cat("Average of",length(x$fits),"networks\n")
    cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits),
            "\n", sep = "")
    invisible(x)
}

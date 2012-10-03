# Defaults:
#For non-seasonal data, p chosen using AIC from linear AR(p) model
#For seasonal data, p=3 and P=1.
#size set to average of number of inputs and number of outputs: (p+P+1)/2

nnetar <- function(x, p, P=1, size, repeats=20)
{
  require(caret)
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
  # Fit average ANN
  fit <- avNNet(lags.X,y,size=size,linout=1,trace=FALSE,repeats=repeats)
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$scale <- scale
  out$size <- size
  out$model <- fit
  out$fitted <- c(rep(NA,maxlag),predict(fit))
  out$fitted <- ts(out$fitted*scale)
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- deparse(substitute(x))
  out$method <- paste("NNAR(",p,sep="")
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
  out <- object
  tspx <- tsp(out$x)

  fcast <- numeric(h)
  flag <- tail(object$x/object$scale, n=max(object$lags))
  for(i in 1:h)
  {
    fcast[i] <- predict(object$model, newdata=flag)
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
    print(x$model)
    cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits),
            "\n", sep = "")
    invisible(x)
}



fitted.train <- function(object, ...)
{
  predict(object)
}

residuals.train <- function(object, ...)
{
  object$trainingData[,".outcome"] - predict(object)
}

fitted.avNNet <- function(object, ...)
{
  predict(object)
}

residuals.avNNet <- function(object, ...)
{
  object$model[[1]]$fitted.values + object$model[[1]]$residuals - predict(object)
}
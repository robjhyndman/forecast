# Random walk related forecasts
# Based on lagged walks
# lag=1 corresponds to standard random walk (i.e., naive forecast)
# lag=m corresponds to seasonal naive method

lagwalk <- function(y, lag=1, h=10, drift=FALSE, 
  level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE)
{
  xname <- deparse(substitute(y))
  n <- length(y)
  m <- frequency(y)
  nn <- 1:h
  if(!is.ts(y))
    y <- ts(y)
  if(!is.null(lambda))
  {
    origy <- y
    y <- BoxCox(y,lambda)
  }
  if(drift)
  {
    fit <- summary(lm(diff(y, lag=lag) ~ 1,na.action=na.exclude))
    b <- fit$coefficients[1,1]
    b.se <- fit$coefficients[1,2]
    s <- fit$sigma
    method <- "Lag walk with drift"
  }
  else
  {
    b <- b.se <- 0
    s <- sd(diff(y, lag=lag),na.rm=TRUE)
    method <- "Lag walk"
  }

  fits <- ts(c(rep(NA,lag),head(y,-lag)) + b, start=tsp(y)[1], frequency=m)
  res <- y - fits
  fullperiods <- (h-1)/lag+1
  steps <- rep(1:fullperiods, rep(m,fullperiods))[1:h]
  f <- rep(tail(y,lag), fullperiods)[1:h] + steps*b
  mse <- mean(res^2, na.rm=TRUE)
  se  <- sqrt(mse*steps  + (steps*b.se)^2)

  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nconf <- length(level)
  z <- qnorm(.5 + level/200)
  lower <- upper <- matrix(NA,nrow=h,ncol=nconf)
  for(i in 1:nconf)
  {
    lower[,i] <- f - z[i]*se
    upper[,i] <- f + z[i]*se
  }
  lower <- ts(lower,start=tsp(y)[2]+1/m,frequency=m)
  upper <- ts(upper,start=tsp(y)[2]+1/m,frequency=m)
  colnames(lower) <- colnames(upper) <- paste(level,"%",sep="")
  fcast <- ts(f,start=tsp(y)[2]+1/m,frequency=m)
  if(!is.null(lambda))
  {
    y <- origy
    fcast <- InvBoxCox(fcast,lambda)
    if(biasadj){
      fcast$mean <- InvBoxCoxf(x = list(level = level, mean = fcast, upper = upper, lower = lower), lambda = lambda) 
    }
    fits <- InvBoxCox(fits,lambda)
    upper <- InvBoxCox(upper,lambda)
    lower <- InvBoxCox(lower,lambda)
  }

  out <- list(method=method,level=level,x=y,xname=xname,mean=fcast,lower=lower,upper=upper,
      model=list(drift=b,drift.se=b.se,sd=s), fitted = fits, residuals = res, lambda=lambda)
  out$model$call <- match.call()

  return(structure(out,class="forecast"))
}


# Random walk
rwf <- function(y,h=10,drift=FALSE,level=c(80,95),fan=FALSE,lambda=NULL,biasadj=FALSE,x=y)
{
  fc <- lagwalk(x, lag=1, h=h, drift=drift, level=level, fan=fan, 
    lambda=lambda, biasadj=biasadj)
  fc$xname <- deparse(substitute(y))
  fc$model$call <- match.call()

  if(drift)
    fc$method <- "Random walk with drift"
  else
    fc$method <- "Random walk"
  return(fc)
}
  
# naive <- function(x,h=10,level=c(80,95),fan=FALSE, lambda=NULL)
# {
#     fc <- forecast(Arima(x,order=c(0,1,0),lambda=lambda),h,level=level,fan=fan)
#     # Remove initial fitted values and error
#     fc$fitted[1] <- NA
#     fc$residuals[1] <- NA
#     fc$method <- "Naive method"
#     return(fc)
# }

naive <- function(y,h=10,level=c(80,95),fan=FALSE, lambda=NULL, biasadj=FALSE,x=y)
{
  fc <- rwf(x, h=h, level=level, fan=fan, lambda=lambda, drift=FALSE, biasadj=biasadj)
  fc$xname <- deparse(substitute(y))
  fc$model$call <- match.call()
  fc$method <- "Naive method"
  return(fc)
}

# snaive <- function(x,h=2*frequency(x),level=c(80,95),fan=FALSE, lambda=NULL)
# {
#     fc <- forecast(Arima(x,seasonal=list(order=c(0,1,0),period=frequency(x)), lambda=lambda),
#       h=h,level=level,fan=fan)
#     # Remove initial fitted values and error
#     m <- frequency(x)
#     fc$fitted[1:m] <- NA
#     fc$residuals[1:m] <- NA
#     fc$method <- "Seasonal naive method"
#     return(fc)
# }

snaive <- function(y, h=2*frequency(x), level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE,x=y)
{
  fc <- lagwalk(x, lag=frequency(x), h=h, drift=FALSE, level=level, fan=fan, 
    lambda=lambda, biasadj=biasadj)
  fc$xname <- deparse(substitute(y))
  fc$model$call <- match.call()
  fc$method <- "Seasonal naive method"
  return(fc)
}

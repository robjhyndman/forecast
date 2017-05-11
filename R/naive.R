# Random walk related forecasts
# Based on lagged walks
# lag=1 corresponds to standard random walk (i.e., naive forecast)
# lag=m corresponds to seasonal naive method

lagwalk <- function(y, lag=1, h=10, drift=FALSE,
  level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE)
{
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
  if(lag==1)
    steps <- 1:h
  else
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
    fcast <- InvBoxCox(fcast, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    fits <- InvBoxCox(fits,lambda)
    upper <- InvBoxCox(upper,lambda)
    lower <- InvBoxCox(lower,lambda)
  }

  out <- list(method=method,level=level,x=y,mean=fcast,lower=lower,upper=upper,
      model=structure(list(includedrift=drift,drift=b,drift.se=b.se,sd=s),class='naive'),
      fitted = fits, residuals = res, lambda=lambda)
  out$model$call <- match.call()

  return(structure(out,class="forecast"))
}


# Random walk
#' @rdname naive
#' 
#' @examples 
#' 
#' gold.fcast <- rwf(gold[1:60], h=50)
#' plot(gold.fcast)
#' 
#' @export
rwf <- function(y,h=10,drift=FALSE,level=c(80,95),fan=FALSE,lambda=NULL,biasadj=FALSE,x=y)
{
  fc <- lagwalk(x, lag=1, h=h, drift=drift, level=level, fan=fan,
    lambda=lambda, biasadj=biasadj)
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))

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



#' Naive and Random Walk Forecasts
#' 
#' \code{rwf()} returns forecasts and prediction intervals for a random walk
#' with drift model applied to \code{y}. This is equivalent to an ARIMA(0,1,0)
#' model with an optional drift coefficient. \code{naive()} is simply a wrapper
#' to \code{rwf()} for simplicity. \code{snaive()} returns forecasts and
#' prediction intervals from an ARIMA(0,0,0)(0,1,0)m model where m is the
#' seasonal period.
#' 
#' The random walk with drift model is \deqn{Y_t=c + Y_{t-1} + Z_t}{Y[t]=c +
#' Y[t-1] + Z[t]} where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are
#' given by \deqn{Y_n(h)=ch+Y_n}{Y[n+h]=ch+Y[n]}. If there is no drift (as in
#' \code{naive}), the drift parameter c=0. Forecast standard errors allow for
#' uncertainty in estimating the drift parameter.
#' 
#' The seasonal naive model is \deqn{Y_t= Y_{t-m} + Z_t}{Y[t]=Y[t-m] + Z[t]}
#' where \eqn{Z_t}{Z[t]} is a normal iid error.
#' 
#' @aliases print.naive
#' 
#' @param y a numeric vector or time series of class \code{ts}
#' @param h Number of periods for forecasting
#' @param drift Logical flag. If TRUE, fits a random walk with drift model.
#' @param level Confidence levels for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise,
#' forecasts back-transformed via an inverse Box-Cox transformation.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param x Deprecated. Included for backwards compatibility.
#' @return An object of class "\code{forecast}".
#' 
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#' 
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{naive} or
#' \code{snaive}.
#' 
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
#' That is x minus fitted values.} \item{fitted}{Fitted values (one-step
#' forecasts)}
#' @author Rob J Hyndman
#' @seealso \code{\link{Arima}}
#' @keywords ts
#' @examples
#' 
#' plot(naive(gold,h=50),include=200)
#' 
#' @export
naive <- function(y,h=10,level=c(80,95),fan=FALSE, lambda=NULL, biasadj=FALSE,x=y)
{
  fc <- rwf(x, h=h, level=level, fan=fan, lambda=lambda, drift=FALSE, biasadj=biasadj)
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))
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

#' @rdname naive
#' 
#' @examples 
#' 
#' plot(snaive(wineind))
#' 
#' @export
snaive <- function(y, h=2*frequency(x), level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE,x=y)
{
  fc <- lagwalk(x, lag=frequency(x), h=h, drift=FALSE, level=level, fan=fan,
    lambda=lambda, biasadj=biasadj)
  fc$model$call <- match.call()
  fc$series <- deparse(substitute(y))
  fc$method <- "Seasonal naive method"
  return(fc)
}

#' @export
print.naive <- function(x, ...)
{
  cat(paste("Call:", deparse(x$call), "\n\n"))
  if(x$includedrift)
    cat(paste("Drift: ",round(x$drift,4),"  (se ",round(x$drift.se,4), ")\n", sep=""))
  cat(paste("Residual sd:", round(x$sd,4), "\n"))
}

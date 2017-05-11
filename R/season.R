### Functions to handle seasonality



#' Number of days in each season
#' 
#' Returns number of days in each month or quarter of the observed time period.
#' 
#' Useful for month length adjustments
#' 
#' @param x time series
#' @return Time series
#' @author Rob J Hyndman
#' @seealso \code{\link[forecast]{bizdays}}
#' @keywords ts
#' @examples
#' 
#' par(mfrow=c(2,1))
#' plot(ldeaths,xlab="Year",ylab="pounds",
#'     main="Monthly deaths from lung disease (UK)")
#' ldeaths.adj <- ldeaths/monthdays(ldeaths)*365.25/12
#' plot(ldeaths.adj,xlab="Year",ylab="pounds",
#'     main="Adjusted monthly deaths from lung disease (UK)")
#' 
#' @export
monthdays <- function(x)
{
    if(!is.ts(x))
        stop("Not a time series")
    f <- frequency(x)
    if(f==12)
        days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    else if(f==4)
        days <- c(90,91,92,92)
    else
        stop("Not monthly or quarterly data")
    nyears <- round(length(x)/f+1)+1
    years <- (1:nyears) + (start(x)[1] - 1)
    leap.years <- ((years %% 4 == 0) & !(years %% 100 ==0 & years %% 400 != 0))[1:nyears]
    dummy <- t(matrix(rep(days,nyears),nrow=f))
    if(f==12)
        dummy[leap.years,2] <- 29
    else
        dummy[leap.years,1] <- 91
    xx <- c(t(dummy))[start(x)[2]-1+(1:length(x))]
    return(ts(xx,start=start(x),frequency=f))
}



#' Forecast seasonal index
#' 
#' Returns vector containing the seasonal index for \code{h} future periods. If
#' the seasonal index is non-periodic, it uses the last values of the index.
#' 
#' 
#' @param object Output from \code{\link[stats]{decompose}} or
#' \link[stats]{stl}.
#' @param h Number of periods ahead to forecast
#' @return Time series
#' @author Rob J Hyndman
#' @keywords ts
#' @examples
#' uk.stl <- stl(UKDriverDeaths,"periodic")
#' uk.sa <- seasadj(uk.stl)
#' uk.fcast <- holt(uk.sa,36)
#' seasf <- sindexf(uk.stl,36)
#' uk.fcast$mean <- uk.fcast$mean + seasf
#' uk.fcast$lower <- uk.fcast$lower + cbind(seasf,seasf)
#' uk.fcast$upper <- uk.fcast$upper + cbind(seasf,seasf)
#' uk.fcast$x <- UKDriverDeaths
#' plot(uk.fcast,main="Forecasts from Holt's method with seasonal adjustment")
#' 
#' @export
sindexf <- function(object,h)
{
    if(class(object)=="stl")
    {
        ss <- object$time.series[,1]
        m <- frequency(ss)
        ss <- ss[length(ss)-(m:1)+1]
        tsp.x <- tsp(object$time.series)
    }
    else if(class(object)=="decomposed.ts")
    {
        ss <- object$figure
        m <- frequency(object$seasonal)
        n <- length(object$trend)
        ss <- rep(ss,n/m+1)[1:n]
        ss <- ss[n-(m:1)+1]
        tsp.x <- tsp(object$seasonal)
    }
    else
        stop("Object of unknown class")
    out <- ts(rep(ss,h/m+1)[1:h], frequency=m, start=tsp.x[2]+1/m)

    return(out)
}




#' Seasonal dummy variables
#' 
#' \code{seasonaldummy} returns a matrix of dummy variables suitable for use in
#' \code{\link{Arima}}, \code{\link{auto.arima}} or \code{\link{tslm}}. The
#' last season is omitted and used as the control.
#' 
#' \code{seasonaldummyf} is deprecated, instead use the \code{h} argument in
#' \code{seasonaldummy}.
#' 
#' The number of dummy variables is determined from the time series
#' characteristics of \code{x}. When \code{h} is missing, the length of
#' \code{x} also determines the number of rows for the matrix returned by
#' \code{seasonaldummy}. the value of \code{h} determines the number of rows
#' for the matrix returned by \code{seasonaldummy}, typically used for
#' forecasting. The values within \code{x} are not used.
#' 
#' @param x Seasonal time series: a \code{ts} or a \code{msts} object
#' @param h Number of periods ahead to forecast (optional)
#' @return Numerical matrix.
#' @author Rob J Hyndman
#' @seealso \code{\link{fourier}}
#' @keywords ts
#' @examples
#' 
#' plot(ldeaths)
#' 
#' # Using seasonal dummy variables
#' month <- seasonaldummy(ldeaths)
#' deaths.lm  <- tslm(ldeaths ~ month)
#' tsdisplay(residuals(deaths.lm))
#' ldeaths.fcast <- forecast(deaths.lm,
#'     data.frame(month=I(seasonaldummy(ldeaths,36))))
#' plot(ldeaths.fcast)
#' 
#' # A simpler approach to seasonal dummy variables
#' deaths.lm  <- tslm(ldeaths ~ season)
#' ldeaths.fcast <- forecast(deaths.lm, h=36)
#' plot(ldeaths.fcast)
#' 
#' @export
seasonaldummy <- function(x, h=NULL)
{
  if(!is.ts(x))
    stop("Not a time series")
  else
    fr.x <- frequency(x)
  if(is.null(h)){
    if(fr.x==1)
      stop("Non-seasonal time series")
    dummy <- as.factor(cycle(x))
    dummy.mat <- matrix(0,ncol=frequency(x)-1,nrow=length(x))
    nrow <- 1:length(x)
    for(i in 1:(frequency(x)-1))
      dummy.mat[dummy==paste(i),i] = 1
    colnames(dummy.mat) <- if (fr.x == 12)
      month.abb[1:11]
    else if(fr.x == 4)
      c("Q1", "Q2", "Q3")
    else paste("S",1:(fr.x-1),sep="")

    return(dummy.mat)
  }
  else{
    return(seasonaldummy(ts(rep(0,h),start=tsp(x)[2]+1/fr.x,frequency=fr.x)))
  }
}

#' @rdname seasonaldummy
#' @export
seasonaldummyf <- function(x, h)
{
  warning("seasonaldummyf() is deprecated, please use seasonaldummy()")
  if(!is.ts(x))
    stop("Not a time series")
  f <- frequency(x)
  return(seasonaldummy(ts(rep(0,h),start=tsp(x)[2]+1/f,frequency=f)))
}



#' Forecasting using stl objects
#' 
#' Forecasts of STL objects are obtained by applying a non-seasonal forecasting
#' method to the seasonally adjusted data and re-seasonalizing using the last
#' year of the seasonal component.
#' 
#' \code{stlm} takes a time series \code{y}, applies an STL decomposition, and
#' models the seasonally adjusted data using the model passed as
#' \code{modelfunction} or specified using \code{method}. It returns an object
#' that includes the original STL decomposition and a time series model fitted
#' to the seasonally adjusted data. This object can be passed to the
#' \code{forecast.stlm} for forecasting.
#' 
#' \code{forecast.stlm} forecasts the seasonally adjusted data, then
#' re-seasonalizes the results by adding back the last year of the estimated
#' seasonal component.
#' 
#' \code{stlf} combines \code{stlm} and \code{forecast.stlm}. It takes a
#' \code{ts} argument, applies an STL decomposition, models the seasonally
#' adjusted data, reseasonalizes, and returns the forecasts. However, it allows
#' more general forecasting methods to be specified via
#' \code{forecastfunction}.
#' 
#' \code{forecast.stl} is similar to \code{stlf} except that it takes the STL
#' decomposition as the first argument, instead of the time series.
#' 
#' Note that the prediction intervals ignore the uncertainty associated with
#' the seasonal component. They are computed using the prediction intervals
#' from the seasonally adjusted series, which are then reseasonalized using the
#' last year of the seasonal component. The uncertainty in the seasonal
#' component is ignored.
#' 
#' The time series model for the seasonally adjusted data can be specified in
#' \code{stlm} using either \code{method} or \code{modelfunction}. The
#' \code{method} argument provides a shorthand way of specifying
#' \code{modelfunction} for a few special cases. More generally,
#' \code{modelfunction} can be any function with first argument a \code{ts}
#' object, that returns an object that can be passed to \code{\link{forecast}}.
#' For example, \code{forecastfunction=ar} uses the \code{\link{ar}} function
#' for modelling the seasonally adjusted series.
#' 
#' The forecasting method for the seasonally adjusted data can be specified in
#' \code{stlf} and \code{forecast.stl} using either \code{method} or
#' \code{forecastfunction}. The \code{method} argument provides a shorthand way
#' of specifying \code{forecastfunction} for a few special cases. More
#' generally, \code{forecastfunction} can be any function with first argument a
#' \code{ts} object, and other \code{h} and \code{level}, which returns an
#' object of class \code{\link{forecast}}. For example,
#' \code{forecastfunction=thetaf} uses the \code{\link{thetaf}} function for
#' forecasting the seasonally adjusted series.
#' 
#' @param y A univariate numeric time series of class \code{ts}
#' @param object An object of class \code{stl} or \code{stlm}. Usually the
#' result of a call to \code{\link[stats]{stl}} or \code{stlm}.
#' @param method Method to use for forecasting the seasonally adjusted series.
#' @param modelfunction An alternative way of specifying the function for
#' modelling the seasonally adjusted series. If \code{modelfunction} is not
#' \code{NULL}, then \code{method} is ignored. Otherwise \code{method} is used
#' to specify the time series model to be used.
#' @param model Output from a previous call to \code{stlm}. If a \code{stlm}
#' model is passed, this same model is fitted to y without re-estimating any
#' parameters.
#' @param forecastfunction An alternative way of specifying the function for
#' forecasting the seasonally adjusted series. If \code{forecastfunction} is
#' not \code{NULL}, then \code{method} is ignored. Otherwise \code{method} is
#' used to specify the forecasting method to be used.
#' @param etsmodel The ets model specification passed to
#' \code{\link[forecast]{ets}}. By default it allows any non-seasonal model. If
#' \code{method!="ets"}, this argument is ignored.
#' @param xreg Historical regressors to be used in
#' \code{\link[forecast]{auto.arima}()} when \code{method=="arima"}.
#' @param newxreg Future regressors to be used in
#' \code{\link[forecast]{forecast.Arima}()}.
#' @param h Number of periods for forecasting.
#' @param level Confidence level for prediction intervals.
#' @param fan If \code{TRUE}, level is set to seq(51,99,by=3). This is suitable
#' for fan plots.
#' @param lambda Box-Cox transformation parameter. Ignored if \code{NULL}.
#' Otherwise, data transformed before decomposition and back-transformed after
#' forecasts are computed.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param s.window Either the character string ``periodic'' or the span (in
#' lags) of the loess window for seasonal extraction.
#' @param t.window A number to control the smoothness of the trend. See
#' \code{\link[stats]{stl}} for details.
#' @param robust If \code{TRUE}, robust fitting will used in the loess
#' procedure within \code{\link[stats]{stl}}.
#' @param allow.multiplicative.trend If TRUE, then ETS models with
#' multiplicative trends are allowed. Otherwise, only additive or no trend ETS
#' models are permitted.
#' @param x Deprecated. Included for backwards compatibility.
#' @param ... Other arguments passed to \code{forecast.stl},
#' \code{modelfunction} or \code{forecastfunction}.
#' @return \code{stlm} returns an object of class \code{stlm}. The other
#' functions return objects of class \code{forecast}.
#' 
#' There are many methods for working with \code{\link{forecast}} objects
#' including \code{summary} to obtain and print a summary of the results, while
#' \code{plot} produces a plot of the forecasts and prediction intervals. The
#' generic accessor functions \code{fitted.values} and \code{residuals} extract
#' useful features.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{stl}}, \code{\link{forecast.ets}},
#' \code{\link{forecast.Arima}}.
#' @keywords ts
#' @examples
#' 
#' tsmod <- stlm(USAccDeaths, modelfunction=ar)
#' plot(forecast(tsmod, h=36))
#' 
#' decomp <- stl(USAccDeaths,s.window="periodic")
#' plot(forecast(decomp))
#' 
#' @export
forecast.stl <- function(object, method=c("ets","arima","naive","rwdrift"), etsmodel="ZZN",
     forecastfunction=NULL,
     h = frequency(object$time.series)*2, level = c(80, 95), fan = FALSE,
     lambda=NULL, biasadj=NULL, xreg=NULL, newxreg=NULL, allow.multiplicative.trend=FALSE, ...)
{
  method <- match.arg(method)
  if(is.null(forecastfunction))
  {
    if(method!="arima" & (!is.null(xreg) | !is.null(newxreg)))
      stop("xreg and newxreg arguments can only be used with ARIMA models")
    if(method=="ets")
    {
      # Ensure non-seasonal model
      if(substr(etsmodel,3,3) != "N")
      {
        warning("The ETS model must be non-seasonal. I'm ignoring the seasonal component specified.")
        substr(etsmodel,3,3) <- "N"
      }
      forecastfunction <- function(x,h,level,...){
        fit <- ets(x,model=etsmodel, allow.multiplicative.trend=allow.multiplicative.trend, ...)
        return(forecast(fit,h=h,level=level))
      }
    }
    else if(method=="arima")
    {
      forecastfunction <- function(x,h,level,...){
        fit <- auto.arima(x,xreg=xreg,seasonal=FALSE,...)
        return(forecast(fit,h=h,level=level,xreg=newxreg))
      }
    }
    else if(method=="naive")
    {
      forecastfunction <- function(x, h,level,...){
        rwf(x,drift=FALSE,h=h,level=level,...)
      }
    }
    else if(method=="rwdrift")
    {
      forecastfunction <- function(x, h,level,...){
        rwf(x,drift=TRUE,h=h,level=level,...)
      }
    }
  }
  if(is.null(xreg) != is.null(newxreg))
    stop("xreg and newxreg arguments must both be supplied")
  if(!is.null(newxreg))
  {
    if(nrow(as.matrix(newxreg))!=h)
      stop("newxreg should have the same number of rows as the forecast horizon h")
  }
  if(fan)
    level <- seq(51, 99, by = 3)

  m <- frequency(object$time.series)
  n <- nrow(object$time.series)
  lastseas <- rep(object$time.series[n-(m:1)+1,"seasonal"],trunc(1+(h-1)/m))[1:h]
  # De-seasonalize
  x.sa <- seasadj(object)
  # Forecast
  fcast <- forecastfunction(x.sa, h=h, level=level, ...)

  # Reseasonalize
  fcast$mean <- fcast$mean + lastseas
  fcast$upper <- fcast$upper + lastseas
  fcast$lower <- fcast$lower + lastseas
  fcast$x <- ts(rowSums(object$time.series))
  tsp(fcast$x) <- tsp(object$time.series)
  fcast$method <- paste("STL + ",fcast$method)
  fcast$series <- deparse(object$call$x)
  fcast$seasonal <- ts(lastseas[1:m],frequency=m,start=tsp(object$time.series)[2]-1+1/m)
  fcast$fitted <- fitted(fcast)+object$time.series[,1]
  fcast$residuals <- fcast$x - fcast$fitted

	if (!is.null(lambda))
	{
		fcast$x <- InvBoxCox(fcast$x,lambda)
		fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
		fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast)
		fcast$lower <- InvBoxCox(fcast$lower, lambda)
		fcast$upper <- InvBoxCox(fcast$upper, lambda)
		attr(lambda, "biasadj") <- biasadj
		fcast$lambda <- lambda
	}

   return(fcast)
}


# Function takes time series, does STL decomposition, and fits a model to seasonally adjusted series
# But it does not forecast. Instead, the result can be passed to forecast().
#' @rdname forecast.stl
#' @export
stlm <- function(y ,s.window=7, robust=FALSE, method=c("ets","arima"), modelfunction=NULL, model=NULL,
                 etsmodel="ZZN", lambda=NULL, biasadj=FALSE, xreg=NULL, allow.multiplicative.trend=FALSE, x=y, ...)
{
  method <- match.arg(method)

  # Check univariate
  if(NCOL(x)>1L)
    stop("y must be a univariate time series")
  else
  {
    if(!is.null(ncol(x)))
    {
      if(ncol(x)==1L) # Probably redundant check
        x <- x[,1L]
    }
  }
  # Check x is a seasonal time series
  tspx <- tsp(x)
  if(is.null(tspx))
    stop("y is not a seasonal ts object")
  else if(tspx[3] <= 1L)
    stop("y is not a seasonal ts object")

  # Transform data if necessary
  origx <- x
  if (!is.null(lambda))
    x <- BoxCox(x, lambda)

  # Do STL decomposition
  stld <- stl(x,s.window=s.window,robust=robust)

  if(!is.null(model)){
    if(inherits(model$model, "ets")){
      modelfunction <- function(x,...){return(ets(x,model=model$model, use.initial.values = TRUE, ...))}
    }
    else if(inherits(model$model, "Arima")){
      modelfunction <- function(x,...){return(Arima(x,model=model$model, ...))}
    }
    else if(!is.null(model$modelfunction)){
      if("model"%in%names(formals(model$modelfunction))){
        modelfunction <- function(x,...){return(model$modelfunction(x,model=model$model, ...))}
      }
    }
    if(is.null(modelfunction)){
      stop("Unknown model type")
    }
  }
  # Construct modelfunction if not passed as an argument
  else if(is.null(modelfunction))
  {
    if(method!="arima" & !is.null(xreg))
      stop("xreg arguments can only be used with ARIMA models")
    if(method=="ets")
    {
      # Ensure non-seasonal model
      if(substr(etsmodel,3,3) != "N")
      {
        warning("The ETS model must be non-seasonal. I'm ignoring the seasonal component specified.")
        substr(etsmodel,3,3) <- "N"
      }
      modelfunction <- function(x,...){return(ets(x,model=etsmodel,
        allow.multiplicative.trend=allow.multiplicative.trend,...))}
    }
    else if(method=="arima")
      modelfunction <- function(x,...){return(auto.arima(x,xreg=xreg,seasonal=FALSE,...))}
  }

  # De-seasonalize
  x.sa <- seasadj(stld)

  # Model seasonally adjusted data
  fit <- modelfunction(x.sa, ...)
  fit$x <- x.sa

  # Fitted values and residuals
  fits <- fitted(fit) + stld$time.series[,"seasonal"]
  res <- residuals(fit)
  if(!is.null(lambda)){
    fits <- InvBoxCox(fits, lambda, biasadj, var(res))
    attr(lambda, "biasadj") <- biasadj
  }

  return(structure(list(stl=stld, model=fit, modelfunction=modelfunction, lambda=lambda,
                        x=origx, series=deparse(substitute(y)), m=frequency(origx), fitted=fits, residuals=res),class="stlm"))
}

#' @rdname forecast.stl
#' @export
forecast.stlm <- function(object, h = 2*object$m, level = c(80, 95), fan = FALSE,
     lambda=object$lambda, biasadj=NULL, newxreg=NULL, allow.multiplicative.trend=FALSE, ...)
{
  if(!is.null(newxreg))
  {
    if(nrow(as.matrix(newxreg))!=h)
      stop("newxreg should have the same number of rows as the forecast horizon h")
  }
  if(fan)
    level <- seq(51, 99, by = 3)

  m <- frequency(object$stl$time.series)
  n <- nrow(object$stl$time.series)
  lastseas <- rep(object$stl$time.series[n-(m:1)+1,"seasonal"],trunc(1+(h-1)/m))[1:h]

  # Forecast seasonally adjusted series
  if(is.element("Arima",class(object$model)) & !is.null(newxreg))
    fcast <- forecast(object$model, h=h, level=level, xreg=newxreg, ...)
  else if(is.element("ets",class(object$model)))
    fcast <- forecast(object$model, h=h, level=level,
      allow.multiplicative.trend=allow.multiplicative.trend, ...)
  else
    fcast <- forecast(object$model, h=h, level=level, ...)

  # Reseasonalize
  fcast$mean <- fcast$mean + lastseas
  fcast$upper <- fcast$upper + lastseas
  fcast$lower <- fcast$lower + lastseas
  fcast$method <- paste("STL + ",fcast$method)
  fcast$series <- object$series
  fcast$seasonal <- ts(lastseas[1:m],frequency=m,start=tsp(object$stl$time.series)[2]-1+1/m)
  #fcast$residuals <- residuals()
  fcast$fitted <- fitted(fcast)+object$stl$time.series[,1]

  if (!is.null(lambda))
  {
    fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
    fcast$mean <- InvBoxCox(fcast$mean, lambda, biasadj, fcast)
    fcast$lower <- InvBoxCox(fcast$lower, lambda)
    fcast$upper <- InvBoxCox(fcast$upper, lambda)
    attr(lambda, "biasadj") <- biasadj
    fcast$lambda <- lambda
  }
  fcast$x <- object$x

  return(fcast)
}

#' @rdname forecast.stl
#' 
#' @examples 
#' 
#' plot(stlf(AirPassengers, lambda=0))
#' 
#' @export
stlf <- function(y, h=frequency(x)*2, s.window=7, t.window=NULL, robust=FALSE, lambda=NULL, biasadj=FALSE, x=y, ...)
{
  seriesname <- deparse(substitute(y))

  # Check univariate
  if(NCOL(x)>1L)
    stop("y must be a univariate time series")
  else
  {
    if(!is.null(ncol(x)))
    {
      if(ncol(x)==1L) # Probably redundant check
        x <- x[,1L]
    }
  }
  # Check x is a seasonal time series
  tspx <- tsp(x)
  if(is.null(tspx))
    stop("y is not a seasonal ts object")
  else if(tspx[3] <= 1L)
    stop("y is not a seasonal ts object")

	if (!is.null(lambda))
		x <- BoxCox(x, lambda)

	fit <- stl(x,s.window=s.window,t.window=t.window,robust=robust)
	fcast <- forecast(fit,h=h,lambda=lambda,biasadj=biasadj, ...)

	# if (!is.null(lambda))
	# {
	# 	fcast$x <- origx
	# 	fcast$fitted <- InvBoxCox(fcast$fitted, lambda)
	# 	fcast$mean <- InvBoxCox(fcast$mean, lambda)
	# 	fcast$lower <- InvBoxCox(fcast$lower, lambda)
	# 	fcast$upper <- InvBoxCox(fcast$upper, lambda)
	# 	fcast$lambda <- lambda
	# }

  fcast$series <- seriesname

	return(fcast)
}



#' Fourier terms for modelling seasonality
#' 
#' \code{fourier} returns a matrix containing terms from a Fourier series, up
#' to order \code{K}, suitable for use in \code{\link{Arima}},
#' \code{\link{auto.arima}}, or \code{\link{tslm}}.
#' 
#' \code{fourierf} is deprecated, instead use the \code{h} argument in
#' \code{fourier}.
#' 
#' The period of the Fourier terms is determined from the time series
#' characteristics of \code{x}. When \code{h} is missing, the length of
#' \code{x} also determines the number of rows for the matrix returned by
#' \code{fourier}. Otherwise, the value of \code{h} determines the number of
#' rows for the matrix returned by \code{fourier}, typically used for
#' forecasting. The values within \code{x} are not used.
#' 
#' When \code{x} is a \code{ts} object, the value of \code{K} should be an
#' integer and specifies the number of sine and cosine terms to return. Thus,
#' the matrix returned has \code{2*K} columns.
#' 
#' When \code{x} is a \code{msts} object, then \code{K} should be a vector of
#' integers specifying the number of sine and cosine terms for each of the
#' seasonal periods. Then the matrix returned will have \code{2*sum(K)}
#' columns.
#' 
#' @param x Seasonal time series: a \code{ts} or a \code{msts} object
#' @param K Maximum order(s) of Fourier terms
#' @param h Number of periods ahead to forecast (optional)
#' @return Numerical matrix.
#' @author Rob J Hyndman
#' @seealso \code{\link{seasonaldummy}}
#' @keywords ts
#' @examples
#' 
#' library(ggplot2)
#' 
#' # Using Fourier series for a "ts" object
#' # K is chosen to minimize the AICc
#' deaths.model  <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths,K=5), seasonal=FALSE)
#' deaths.fcast <- forecast(deaths.model, xreg=fourier(USAccDeaths, K=5, h=36))
#' autoplot(deaths.fcast) + xlab("Year")
#' 
#' # Using Fourier series for a "msts" object
#' taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
#' taylor.fcast <- forecast(taylor.lm,
#'     data.frame(fourier(taylor, K = c(3, 3), h = 270)))
#' autoplot(taylor.fcast)
#' 
#' @export
fourier <- function(x, K, h=NULL)
{
  if(is.null(h)){
    return(...fourier(x, K, 1:NROW(x)))
  }
  else{
    return(...fourier(x, K, NROW(x)+(1:h)))
  }
}

#' @rdname fourier
#' @export
fourierf <- function(x, K, h)
{
  warning("fourierf() is deprecated, please use fourier()")
  return(...fourier(x, K, length(x)+(1:h)))
}


# Function to do the work.
...fourier <- function(x, K, times)
{
  if (any(class(x) == "msts")) {
    period <- attr(x, "msts")
  } else {
    period <- frequency(x)
  }

  # Patch for older versions of R that do not have sinpi and cospi functions.
  if(!exists("sinpi"))
  {
    sinpi <- function(x){sin(pi*x)}
    cospi <- function(x){cos(pi*x)}
  }

  if(length(period) != length(K))
    stop("Number of periods does not match number of orders")
  if(any(2*K > period))
    stop("K must be not be greater than period/2")

  # Compute periods of all Fourier terms
  p <- numeric(0)
  labels <- character(0)
  for(j in seq_along(period))
  {
    if(K[j]>0)
    {
      p <- c(p, (1:K[j])/period[j])
      labels <- c(labels, paste(paste0(c("S","C"),rep(1:K[j],rep(2,K[j]))),
                      round(period[j]), sep="-"))
    }
  }
  # Remove equivalent seasonal periods due to multiple seasonality
  k <- duplicated(p)
  p <- p[!k]
  labels <- labels[!rep(k,rep(2,length(k)))]

  # Remove columns where sinpi=0
  k <- abs(2*p - round(2*p)) > .Machine$double.eps

  # Compute matrix of Fourier terms
  X <- matrix(NA_real_,nrow=length(times),ncol=2L*length(p))
  for(j in seq_along(p))
  {
    if(k[j])
      X[,2L*j-1L] <- sinpi(2*p[j]*times)
    X[,2L*j] <- cospi(2*p[j]*times)
  }
  colnames(X) <- labels

  # Remove missing columns
  X <- X[,!is.na(colSums(X)),drop=FALSE]

  return(X)
}



#' Moving-average smoothing
#' 
#' \code{ma} computes a simple moving average smoother of a given time series.
#' 
#' The moving average smoother averages the nearest \code{order} periods of
#' each observation. As neighbouring observations of a time series are likely
#' to be similar in value, averaging eliminates some of the randomness in the
#' data, leaving a smooth trend-cycle component. \deqn{\hat{T}_{t} =
#' \frac{1}{m} \sum_{j=-k}^k
#' y_{t+j}}{T[t]=1/m(y[t-k]+y[t-k+1]+\ldots+y[t]+\ldots+y[t+k-1]+y[t+k])} where
#' \eqn{k=\frac{m-1}{2}}{k=(m-1)/2}
#' 
#' When an even \code{order} is specified, the observations averaged will
#' include one more observation from the future than the past (k is rounded
#' up). If centre is TRUE, the value from two moving averages (where k is
#' rounded up and down respectively) are averaged, centering the moving
#' average.
#' 
#' @param x Univariate time series
#' @param order Order of moving average smoother
#' @param centre If TRUE, then the moving average is centred for even orders.
#' @return Numerical time series object containing the simple moving average
#' smoothed values.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{decompose}}
#' @keywords ts
#' @examples
#' 
#' plot(wineind)
#' sm <- ma(wineind,order=12)
#' lines(sm,col="red")
#' 
#' @export
ma <- function(x, order, centre=TRUE)
{
  if(abs(order - round(order)) > 1e-8)
    stop("order must be an integer")

  if(order%%2 == 0 & centre) #centred and even
    w <- c(0.5,rep(1,order-1),0.5)/order
  else # odd or not centred
    w <- rep(1,order)/order

  return(filter(x, w))
}

#' @rdname is.ets
#' @export
is.stlm <- function(x){
  inherits(x, "stlm")
}

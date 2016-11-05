\name{Arima}
\alias{Arima}
\title{Fit ARIMA model to univariate time series}
\usage{
Arima(y, order=c(0,0,0), seasonal=c(0,0,0),
    xreg=NULL, include.mean=TRUE, include.drift=FALSE, 
    include.constant, lambda=model$lambda, method=c("CSS-ML","ML","CSS"), 
    model=NULL, x=y,...)
}

\arguments{
\item{y}{a univariate time series of class \code{ts}.}
\item{order}{A specification of the non-seasonal part of the ARIMA model: the three components (p, d, q) are the AR order, the degree of differencing, and the MA order.}
\item{seasonal}{A specification of the seasonal part of the ARIMA model, plus the period (which defaults to frequency(y)). This should be a list with components order and period, but a specification of just a numeric vector of length 3 will be turned into a suitable list with the specification as the order.}
\item{xreg}{Optionally, a vector or matrix of external regressors, which must have the same number of rows as y.}
\item{include.mean}{Should the ARIMA model include a mean term? The default is TRUE for undifferenced series, FALSE for differenced ones (where a mean would not affect the fit nor predictions).}
\item{include.drift}{Should the ARIMA model include a linear drift term? (i.e., a linear regression with ARIMA errors is fitted.) 
    The default is FALSE.}
\item{include.constant}{If TRUE, then \code{include.mean} is set to be TRUE for undifferenced series and \code{include.drift} is set to be TRUE for differenced series. Note that if there is more than one difference taken, no constant is included regardless of the value of this argument. This is deliberate as otherwise quadratic and higher order polynomial trends would be induced.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated.}
\item{method}{Fitting method: maximum likelihood or minimize conditional sum-of-squares. The default (unless there are missing values) is to use conditional-sum-of-squares to find starting values, then maximum likelihood.}
\item{model}{Output from a previous call to \code{Arima}. If model is passed, this same model is fitted to \code{y} without re-estimating any parameters.}
\item{x}{Deprecated. Included for backwards compatibility.}
\item{...}{Additional arguments to be passed to \code{\link[stats]{arima}}.}
}

\description{Largely a wrapper for the \code{\link[stats]{arima}} function in the stats package. The main difference is that this function
allows a drift term. It is also possible to 
take an ARIMA model from a previous call to \code{Arima} and re-apply it to the data \code{y}.
}
\details{See the \code{\link[stats]{arima}} function in the stats package.}

\value{See the \code{\link[stats]{arima}} function in the stats package. The additional objects returned are
\item{x}{The time series data}
\item{xreg}{The regressors used in fitting (when relevant).}
}

\seealso{\code{\link{auto.arima}}, \code{\link{forecast.Arima}}.}

\author{Rob J Hyndman}
\examples{fit <- Arima(WWWusage,order=c(3,1,0))
plot(forecast(fit,h=20))

# Fit model to first few years of AirPassengers data
air.model <- Arima(window(AirPassengers,end=1956+11/12),order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12),lambda=0)
plot(forecast(air.model,h=48))
lines(AirPassengers)

# Apply fitted model to later data
air.model2 <- Arima(window(AirPassengers,start=1957),model=air.model)

# Forecast accuracy measures on the log scale.
# in-sample one-step forecasts.
accuracy(air.model)
# out-of-sample one-step forecasts.
accuracy(air.model2)
# out-of-sample multi-step forecasts
accuracy(forecast(air.model,h=48,lambda=NULL), 
         log(window(AirPassengers,start=1957)))
}
\keyword{ts}

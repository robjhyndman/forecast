\name{Arima}
\alias{Arima}
\title{Fit ARIMA model to univariate time series}
\usage{
Arima(x, order=c(0,0,0), seasonal=list(order=c(0,0,0), period=NA),
    xreg=NULL, include.mean=TRUE, include.drift=FALSE, 
    include.constant, lambda=model$lambda, transform.pars=TRUE, 
    fixed=NULL, init=NULL, method=c("CSS-ML","ML","CSS"), n.cond, 
    optim.control=list(), kappa=1e6, model=NULL)
}

\arguments{
\item{x}{a univariate time series of class \code{ts}.}
\item{order}{A specification of the non-seasonal part of the ARIMA model: the three components (p, d, q) are the AR order, the degree of differencing, and the MA order.}
\item{seasonal}{A specification of the seasonal part of the ARIMA model, plus the period (which defaults to frequency(x)). This should be a list with components order and period, but a specification of just a numeric vector of length 3 will be turned into a suitable list with the specification as the order.}
\item{xreg}{Optionally, a vector or matrix of external regressors, which must have the same number of rows as x.}
\item{include.mean}{Should the ARIMA model include a mean term? The default is TRUE for undifferenced series, FALSE for differenced ones (where a mean would not affect the fit nor predictions).}
\item{include.drift}{Should the ARIMA model include a linear drift term? (i.e., a linear regression with ARIMA errors is fitted.) 
    The default is FALSE.}
\item{include.constant}{If TRUE, then \code{include.mean} is set to be TRUE for undifferenced series and \code{include.drift} is set to be TRUE for differenced series. Note that if there is more than one difference taken, no constant is included regardless of the value of this argument. This is deliberate as otherwise quadratic and higher order polynomial trends would be induced.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated.}
\item{transform.pars}{Logical. If true, the AR parameters are transformed to ensure that they remain in the region of stationarity. Not used for method="CSS".}
\item{fixed}{optional numeric vector of the same length as the total number of parameters. If supplied, only NA entries in fixed will be varied. transform.pars=TRUE will be overridden (with a warning) if any AR parameters are fixed. It may be wise to set transform.pars=FALSE when fixing MA parameters, especially near non-invertibility.}
\item{init}{optional numeric vector of initial parameter values. Missing values will be filled in, by zeroes except for regression coefficients. Values already specified in fixed will be ignored.}
\item{method}{Fitting method: maximum likelihood or minimize conditional sum-of-squares. The default (unless there are missing values) is to use conditional-sum-of-squares to find starting values, then maximum likelihood.}
\item{n.cond}{Only used if fitting by conditional-sum-of-squares: the number of initial observations to ignore. It will be ignored if less than the maximum lag of an AR term.}
\item{optim.control}{List of control parameters for optim.}
\item{kappa}{the prior variance (as a multiple of the innovations variance) for the past observations in a differenced model. Do not reduce this.}
\item{model}{Output from a previous call to \code{Arima}. If model is passed, this same model is fitted to
\code{x} without re-estimating any parameters.}
}

\description{Largely a wrapper for the \code{\link[stats]{arima}} function in the stats package. The main difference is that this function
allows a drift term. It is also possible to 
take an ARIMA model from a previous call to \code{Arima} and re-apply it to the data \code{x}.
}
\details{See the \code{\link[stats]{arima}} function in the stats package.}

\value{See the \code{\link[stats]{arima}} function in the stats package. The additional objects returned are
\item{x}{The time series data}
\item{xreg}{The regressors used in fitting (when relevant).}
}

\seealso{\code{\link[stats]{arima}}}

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

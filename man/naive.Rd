\name{naive}
\alias{naive}
\alias{snaive}
\title{Naive forecasts}
\usage{naive(x, h=10, level=c(80,95), fan=FALSE, lambda=NULL)
snaive(x, h=2*frequency(x), level=c(80,95), fan=FALSE, lambda=NULL)
}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence levels for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
}
\description{\code{naive()} returns forecasts and prediction intervals for an ARIMA(0,1,0) random walk model applied to x.
\code{snaive()} returns forecasts and prediction intervals from an ARIMA(0,0,0)(0,1,0)m model where m is the seasonal period.
}

\details{These functions are simply convenient wrappers to \code{\link{Arima}} with the appropriate arguments
to return naive and seasonal naive forecasts.}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{naive} or \code{snaive}.

An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts)}
}

\seealso{\code{\link{Arima}}, \code{\link{rwf}}}

\author{Rob J Hyndman}
\examples{
plot(naive(gold,h=50),include=200)
plot(snaive(wineind))
}
\keyword{ts}

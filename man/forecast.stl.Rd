\name{forecast.stl}
\alias{forecast.stl}
\alias{stlf}
\alias{stl}
\title{Forecasting using stl objects}
\usage{\method{forecast}{stl}(object, method=c("ets","arima", "naive", "rwdrift"), etsmodel="ZZN",
    h=frequency(object$time.series)*2, level=c(80,95), fan=FALSE, lambda=NULL, ...)
stlf(x, h=frequency(x)*2, s.window=7, robust=FALSE, method=c("ets","arima", "naive", "rwdrift"), etsmodel="ZZN",
    level=c(80,95), fan=FALSE, lambda=NULL, ...)
}

\arguments{
\item{object}{An object of class "\code{stl}". Usually the result of a call to \code{\link[stats]{stl}}.}
\item{x}{A univariate numeric time series of class "\code{ts}"}
\item{method}{Method to use for forecasting the seasonally adjusted series.}
\item{etsmodel}{The ets model specification passed to \code{\link[forecast]{ets}}. By default it allows any non-seasonal model. If \code{method!="ets"}, this argument is ignored.}
\item{h}{Number of periods for forecasting.}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If \code{TRUE}, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated and back-transformed after forecasts are computed.}
\item{s.window}{Either the character string "periodic" (default) or the span (in lags) of the loess window for seasonal extraction.}
\item{robust}{If \code{TRUE}, robust fitting will used in the loess procedure within \code{\link[stats]{stl}}.}
\item{...}{Other arguments passed to \code{\link[forecast]{ets}()} or \code{\link[forecast]{auto.arima}()}.}
}

\description{Returns forecasts obtained by either ETS or ARIMA models applied to the seasonally adjusted data from an STL decomposition.}

\details{\code{forecast.stl} seasonally adjusts the data from an STL decomposition, then uses either ETS or ARIMA models to forecast the result. The seasonal component from the last year of data is added back in to the forecasts. Note that the prediction intervals ignore the uncertainty associated with the seasonal component.

\code{stlf} takes a \code{ts} argument and applies a stl decomposition before calling \code{forecast.stl}.
}


\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{forecast.stl}.

An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model. That is (possibly tranformed) x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts) on transformed scale if lambda is not NULL.}
}

\seealso{\code{\link{forecast.ets}}, \code{\link{forecast.Arima}}.}

\author{Rob J Hyndman}

\examples{fit <- stl(USAccDeaths,s.window="periodic")
plot(forecast(fit))

plot(stlf(AirPassengers, lambda=BoxCox.lambda(AirPassengers)))

}
\keyword{ts}

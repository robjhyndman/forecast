\name{ses}
\alias{ses}
\alias{holt}
\alias{hw}
\title{Exponential smoothing forecasts}
\usage{ses(x, h=10, level=c(80,95), fan=FALSE, ...)
holt(x, h=10, damped=FALSE, level=c(80,95), fan=FALSE, ...)
hw(x, h=2*frequency(x), seasonal="additive", damped=FALSE, 
   level=c(80,95), fan=FALSE, ...)
}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting.}
\item{damped}{If TRUE, use a damped trend.}
\item{seasonal}{Type of seasonality in \code{hw} model. "additive" or "multiplicative"}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{...}{Other arguments passed to \code{ets}.}
}

\description{Returns forecasts and other information for exponential smoothing forecasts applied to x.
}
\details{ses, holt and hw are simply convenient wrapper functions for \code{forecast(ets(...))}.
}


\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{ets} and associated functions.

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


\references{Hyndman, R.J., Koehler, A.B., Snyder,
R.D., Grose, S. (2002) "A state space framework for automatic
forecasting using exponential smoothing methods",
\emph{International J. Forecasting}, \bold{18}(3), 439--454.

Hyndman, R.J., Akram, Md., and Archibald, B. (2008) "The
admissible parameter space for exponential smoothing models".
\emph{Annals of Statistical Mathematics}, \bold{60}(2),
407--426.}



\seealso{\code{\link{ets}}, \code{\link[stats]{HoltWinters}}, \code{\link{rwf}}, \code{\link[stats]{arima}}.}

\author{Rob J Hyndman}
\examples{
fcast <- holt(airmiles)
plot(fcast)
deaths.fcast <- hw(USAccDeaths,h=48)
plot(deaths.fcast)
}
\keyword{ts}

\name{forecast.baggedETS}
\alias{forecast.baggedETS}
\title{Forecasting using the bagged ETS method}
\usage{\method{forecast}{baggedETS}(object,
    h=ifelse(frequency(object$x)>1, 2*frequency(object$x), 10), ...)
}

\arguments{
\item{object}{An object of class "\code{baggedETS}" resulting from a call to \code{\link{baggedETS}}.}
\item{h}{Number of periods for forecasting.}
\item{...}{Additional arguments passed to \code{\link{ets}}}
}

\description{Returns forecasts and other information for bagged ETS models.}

\details{Intervals are calculated as min and max values over the point forecasts from the ETS models in the ensemble. I.e., the intervals are not prediction intervals, but give an indication of how different the forecasts within the ensemble are.
}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

An object of class "\code{forecast}" is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{xreg}{The external regressors used in fitting (if given).}
\item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts)}
\item{...}{Other arguments}
}

\references{Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging Exponential Smoothing Methods
using STL Decomposition and Box-Cox Transformation. International Journal of Forecasting 32,
303-312.
}

\seealso{\code{\link{baggedETS}}.}

\author{Christoph Bergmeir, Fotios Petropoulos}

\examples{fit <- baggedETS(WWWusage)
fcast <- forecast(fit)
plot(fcast)
}
\keyword{ts}

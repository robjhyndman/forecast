\name{forecast}
\alias{forecast}
\alias{forecast.default}
\alias{forecast.ts}
\alias{print.forecast}
\alias{summary.forecast}
\title{Forecasting time series}


\description{
\code{forecast} is a generic function for forecasting from time series or time series models.
The function invokes particular \emph{methods} which depend on the class of the first argument.

For example, the function \code{\link{forecast.Arima}} makes forecasts based on the results produced by \code{\link[stats]{arima}}.

The function \code{\link{forecast.ts}} makes forecasts using \code{\link{ets}} models (if the data are non-seasonal or the seasonal period is 12 or less) or \code{\link{stlf}} (if the seasonal period is 13 or more).
}

\usage{forecast(object,...)
\method{forecast}{ts}(object, h, level=c(80,95), fan=FALSE, ...)}

\arguments{
\item{object}{a time series or time series model for which forecasts are required}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{...}{Additional arguments affecting the forecasts produced. \code{forecast.ts} passes these to \code{\link{forecast.ets}} or \code{\link{stlf}} depending on the frequency of the time series.}
}
\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain
and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract various useful features of
the value returned by \code{forecast$model}.

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

\seealso{Other functions which return objects of class \code{"forecast"} are
\code{\link{forecast.ets}}, \code{\link{forecast.Arima}}, \code{\link{forecast.HoltWinters}}, \code{\link{forecast.StructTS}},
\code{\link{meanf}}, \code{\link{rwf}}, \code{\link{splinef}}, \code{\link{thetaf}}, \code{\link{croston}}, \code{\link{ses}}, \code{\link{holt}},
\code{\link{hw}}.
}

\author{Rob J Hyndman}
\keyword{ts}

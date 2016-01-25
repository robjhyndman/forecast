\name{forecast.bats}
\alias{forecast.bats}
\alias{forecast.tbats}
\title{Forecasting using BATS and TBATS models}

\usage{\method{forecast}{bats}(object, h, level=c(80,95), fan=FALSE, biasadj=FALSE, ...)
\method{forecast}{tbats}(object, h, level=c(80,95), fan=FALSE, biasadj=FALSE, ...)
}
\arguments{
\item{object}{An object of class "\code{bats}". Usually the result of a call to \code{\link{bats}}.}
\item{h}{Number of periods for forecasting. Default value is twice the largest seasonal period (for seasonal data) or ten (for non-seasonal data).}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If TRUE, level is set to \code{seq(51,99,by=3)}. This is suitable for fan plots.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
\item{...}{Other arguments, currently ignored.}
}

\description{Forecasts \code{h} steps ahead with a BATS model. Prediction intervals are also produced.}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{forecast.bats}.

An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A copy of the \code{bats} object}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model.}
\item{fitted}{Fitted values (one-step forecasts)}
}

\references{De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011), Forecasting time series with complex seasonal patterns using exponential smoothing, \emph{Journal of the American Statistical Association}, \bold{106}(496), 1513-1527.
}

\seealso{\code{\link{bats}}, \code{\link{tbats}},\code{\link{forecast.ets}}.}

\author{Slava Razbash and Rob J Hyndman}

\examples{
\dontrun{
fit <- bats(USAccDeaths)
plot(forecast(fit))

taylor.fit <- bats(taylor)
plot(forecast(taylor.fit))
}
}


\keyword{ts}

\name{fitted.Arima}
\alias{fitted.Arima}
\alias{fitted.fracdiff}
\alias{fitted.bats}
\alias{fitted.tbats}
\alias{fitted.ets}
\alias{fitted.nnetar}
\title{h-step in-sample forecasts for time series models.}
\usage{\method{fitted}{Arima}(object, h=1, ...)
\method{fitted}{fracdiff}(object, h=1, ...)
\method{fitted}{bats}(object, h=1, ...)
\method{fitted}{tbats}(object, h=1, ...)
\method{fitted}{ets}(object, h=1, ...)
\method{fitted}{nnetar}(object, h=1, ...)
}

\arguments{
\item{object}{An object of class "\code{Arima}", "\code{bats}", "\code{tbats}", "\code{ets}" or "\code{nnetar}".}
\item{h}{The number of steps to forecast ahead.}
\item{...}{Other arguments.}
}

\description{Returns h-step forecasts for the data used in fitting the model.}

\value{A time series of the h-step forecasts.}

\seealso{\code{\link{forecast.Arima}}, \code{\link{forecast.bats}}, \code{\link{forecast.tbats}}, \code{\link{forecast.ets}}, \code{\link{forecast.nnetar}},
\code{\link{residuals.Arima}}, \code{\link{residuals.bats}}, \code{\link{residuals.tbats}}, \code{\link{residuals.ets}}, \code{\link{residuals.nnetar}}.}

\author{Rob J Hyndman & Mitchell O'Hara-Wild}

\examples{fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col='red')
lines(fitted(fit, h=2), col='green')
lines(fitted(fit, h=3), col='blue')
legend("topleft", legend=paste("h =",1:3), col=2:4, lty=1)
}
\keyword{ts}

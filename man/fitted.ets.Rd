\name{fitted.ets}
\alias{fitted.ets}
\title{h-step in-sample forecasts using ets models}
\usage{\method{fitted}{ets}(object, h=1, ...)
}

\arguments{
\item{object}{An object of class "\code{ets}". Usually the result of a call to \code{\link{ets}}.}
\item{h}{The number of steps to forecast ahead.}
\item{...}{Other arguments.}
}

\description{Returns h-step forecasts for the data used in fitting the ets model.}

\value{A time series of the h-step forecasts.}

\seealso{\code{\link{forecast.ets}}.}

\author{Rob J Hyndman & Mitchell O'Hara-Wild}

\examples{fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col='red')
lines(fitted(fit, h=2), col='green')
lines(fitted(fit, h=3), col='blue')
legend("topleft", legend=paste("h =",1:3), col=2:4, lty=1)
}
\keyword{ts}

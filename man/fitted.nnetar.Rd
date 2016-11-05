\name{fitted.nnetar}
\alias{fitted.nnetar}
\title{h-step in-sample forecasts using nnetar models}
\usage{\method{fitted}{nnetar}(object, h=1, ...)
}

\arguments{
\item{object}{An object of class "\code{nnetar}". Usually the result of a call to \code{\link{nnetar}}.}
\item{h}{The number of steps to forecast ahead.}
\item{...}{Other arguments.}
}

\description{Returns h-step forecasts for the data used in fitting the nnetar model.}

\value{A time series of the h-step forecasts.}

\seealso{\code{\link{forecast.nnetar}}.}

\author{Rob J Hyndman & Mitchell O'Hara-Wild}

\examples{fit <- nnetar(WWWusage)
plot(WWWusage)
lines(fitted(fit), col='red')
lines(fitted(fit, h=2), col='green')
lines(fitted(fit, h=3), col='blue')
legend("topleft", legend=paste("h =",1:3), col=2:4, lty=1)
}
\keyword{ts}

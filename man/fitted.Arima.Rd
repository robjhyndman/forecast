\name{fitted.Arima}
\alias{fitted.Arima}
\title{h-step in-sample forecasts using ARIMA models}
\usage{\method{fitted}{Arima}(object, biasadj=FALSE, h=1, ...)
}

\arguments{
\item{object}{An object of class "\code{Arima}". Usually the result of a call to \code{\link[stats]{arima}}.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
\item{h}{The number of steps to forecast ahead.}
\item{...}{Other arguments.}
}

\description{Returns h-step forecasts for the data used in fitting the ARIMA model.}

\value{A time series of the h-step forecasts.}

\seealso{\code{\link{forecast.Arima}}.}

\author{Rob J Hyndman}

\examples{fit <- Arima(WWWusage,c(3,1,0))

plot(WWWusage)
lines(fitted(fit, h=1), col='red')
lines(fitted(fit, h=2), col='green')
lines(fitted(fit, h=3), col='blue')
legend("topleft", legend=paste("h =",1:3), col=2:4, lty=1)
}
\keyword{ts}

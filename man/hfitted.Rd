\name{hfitted}
\alias{hfitted}
\title{h-step forecasts from model objects}
\usage{hfitted(object, h=1, FUN=class(object), ...)
}

\arguments{
\item{object}{An model object which can be re-fitted using a model parameter (such as Arima).}
\item{h}{The number of steps to forecast ahead.}
\item{FUN}{The function used to re-fit with new data.}
\item{...}{Other arguments passed on to forecast().}
}

\description{Returns h-step forecasts for the data used in fitting the model.}

\value{A time series of the h-step forecasts.}

\seealso{\code{\link[stats]{fitted}}.}

\author{Mitchell O'Hara-Wild & Rob Hyndman}

\examples{fit <- Arima(WWWusage,c(3,1,0))
plot(WWWusage)
lines(fitted(fit),col=2)
}
\keyword{ts}

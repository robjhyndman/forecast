\name{tscv}
\alias{tscv}
\title{Time series cross-validation}
\usage{
tscv(y, forecastfunction, h, ...)
}

\arguments{
\item{y}{Univariate time series}
\item{forecastfunction}{Function to return an object of class \code{forecast}. Its first argument must be a univariate time series, and it must have an argument \code{h} for the forecast horizon.}
\item{h}{Forecast horizon}
\item{...}{Other arguments are passed to \code{forecastfunction}.}
}

\description{\code{tscv} computes the forecast errors obtained by applying \code{forecastfunction} to subsets of the time series \code{y} using a rolling forecast origin.}

\value{Numerical time series object containing the forecast errors.}

\details{Let \code{y} contain the time series $y_1,\dots,y_n$.}


\author{Rob J Hyndman}
\examples{
far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
e <- tscv(lynx, far2, h=1)
}
\keyword{ts}
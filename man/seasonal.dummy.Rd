\name{seasonaldummy}
\alias{seasonaldummy}
\alias{seasonaldummyf}
\alias{fourier}
\alias{fourierf}
\title{Seasonal dummy variables}
\usage{seasonaldummy(x)
seasonaldummyf(x,h)
fourier(x,K)
fourierf(x,K,h)
}

\arguments{
\item{x}{Seasonal time series}
\item{h}{Number of periods ahead to forecast}
\item{K}{Maximum order of Fourier terms}
}
\description{\code{seasonaldummy} and \code{seasonaldummyf} return matrices of dummy variables suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}. The last season is omitted and used as the control.

\code{fourier} and \code{fourierf} return matrices containing terms from a Fourier series, up to order \code{K}, suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}. 
}

\value{Numerical matrix with number of rows equal to the \code{length(x)} and number of columns equal to \code{frequency(x)-1} (for \code{seasonaldummy} and \code{seasonaldummyf} or \code{2*K} (for \code{fourier} or \code{fourierf}).
}

\author{Rob J Hyndman}
\examples{
plot(ldeaths)

# Using seasonal dummy variables
month <- seasonaldummy(ldeaths)
deaths.lm  <- tslm(ldeaths ~ month)
tsdisplay(residuals(deaths.lm))
ldeaths.fcast <- forecast(deaths.lm, 
  data.frame(month=I(seasonaldummyf(ldeaths,36))))
plot(ldeaths.fcast)

# A simpler approach to seasonal dummy variables
deaths.lm  <- tslm(ldeaths ~ season)
ldeaths.fcast <- forecast(deaths.lm, h=36)
plot(ldeaths.fcast)

# Using Fourier series
X <- fourier(ldeaths,3)
deaths.lm  <- tslm(ldeaths ~ X)
ldeaths.fcast <- forecast(deaths.lm, 
  data.frame(X=I(fourierf(ldeaths,3,36))))
plot(ldeaths.fcast)

}
\keyword{ts}

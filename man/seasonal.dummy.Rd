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
\item{x}{Seasonal time series: a \code{ts} or a \code{msts} object}
\item{h}{Number of periods ahead to forecast}
\item{K}{Maximum order(s) of Fourier terms}
}
\description{\code{seasonaldummy} and \code{seasonaldummyf} return matrices of dummy variables suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}. The last season is omitted and used as the control.

\code{fourier} and \code{fourierf} return matrices containing terms from a Fourier series, up to order \code{K}, suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}. 
}

\details{The number of dummy variables, or the period of the Fourier terms, is determined from the time series characteristics of \code{x}. The length of \code{x} also determines the number of rows for the matrices returned by \code{seasonaldummy} and \code{fourier}. The value of \code{h} determines the number of rows for the matrices returned by \code{seasonaldummyf} and \code{fourierf}. The values within \code{x} are not used in any function.

When \code{x} is a \code{ts} object, the value of \code{K} should be an integer and specifies the number of sine and cosine terms to return. Thus, the matrix returned has \code{2*K} columns.

When \code{x} is a \code{msts} object, then \code{K} should be a vector of integers specifying the number of sine and cosine terms for each of the seasonal periods. Then the matrix returned will have \code{2*sum(K)} columns.  
}

\value{Numerical matrix.
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

# Using Fourier series for a "msts" object
Z <- fourier(taylor, K = c(3, 3))
taylor.lm <- tslm(taylor ~ Z)
taylor.fcast <- forecast(taylor.lm, 
  data.frame(Z = I(fourierf(taylor, K = c(3, 3), h = 270))))
plot(taylor.fcast)
}
\keyword{ts}

\name{seasonaldummy}
\alias{seasonaldummy}
\alias{seasonaldummyf}
\alias{fourier}
\alias{fourierf}
\title{Seasonal dummy variables}
\usage{seasonaldummy(x,h)
fourier(x,K,h)
}

\arguments{
\item{x}{Seasonal time series: a \code{ts} or a \code{msts} object}
\item{h}{Number of periods ahead to forecast (optional)}
\item{K}{Maximum order(s) of Fourier terms}
}
\description{\code{seasonaldummy} returns matrices of dummy variables suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}. The last season is omitted and used as the control.

\code{fourier} returns matrices containing terms from a Fourier series, up to order \code{K}, suitable for use in \code{\link[stats]{arima}}, \code{\link[stats]{lm}} or \code{\link{tslm}}.

\code{fourierf} and \code{seasonaldummyf} are deprecated, instead use the \code{h} argument in \code{fourier} and \code{seasonaldummy}.
}

\details{The number of dummy variables, or the period of the Fourier terms, is determined from the time series characteristics of \code{x}. The length of \code{x} also determines the number of rows for the matrices returned by \code{seasonaldummy} and \code{fourier}. The value of \code{h} determines the number of rows for the matrices returned by \code{seasonaldummy} and \code{fourier}, typically used for forecasting. The values within \code{x} are not used in any function.

When \code{x} is a \code{ts} object, the value of \code{K} should be an integer and specifies the number of sine and cosine terms to return. Thus, the matrix returned has \code{2*K} columns.

When \code{x} is a \code{msts} object, then \code{K} should be a vector of integers specifying the number of sine and cosine terms for each of the seasonal periods. Then the matrix returned will have \code{2*sum(K)} columns.
}

\value{Numerical matrix.}

\author{Rob J Hyndman}
\examples{
plot(ldeaths)

# Using seasonal dummy variables
month <- seasonaldummy(ldeaths)
deaths.lm  <- tslm(ldeaths ~ month)
tsdisplay(residuals(deaths.lm))
ldeaths.fcast <- forecast(deaths.lm,
  data.frame(month=I(seasonaldummy(ldeaths,36))))
plot(ldeaths.fcast)

# A simpler approach to seasonal dummy variables
deaths.lm  <- tslm(ldeaths ~ season)
ldeaths.fcast <- forecast(deaths.lm, h=36)
plot(ldeaths.fcast)

# Using Fourier series
deaths.lm  <- tslm(ldeaths ~ fourier(ldeaths,3))
ldeaths.fcast <- forecast(deaths.lm, fourier(ldeaths,3,36))
plot(ldeaths.fcast)

# Using Fourier series for a "msts" object
taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
taylor.fcast <- forecast(taylor.lm,
                  data.frame(fourier(taylor, K = c(3, 3), h = 270)))
plot(taylor.fcast)
}
\keyword{ts}

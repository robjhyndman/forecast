\name{findfrequency}
\alias{findfrequency}
\title{Find dominant frequency of a time series}
\usage{findfrequency(x)
}
\arguments{
\item{x}{a numeric vector or time series}
}
\description{\code{findfrequency} returns the period of the dominant frequency of a time series. For seasonal data, it will return the seasonal period. For cyclic data, it will return the average cycle length.
}
\details{The dominant frequency is determined from a spectral analysis of the time series. First, a linear trend is removed, then the spectral density function is estimated from the best fitting autoregressive model (based on the AIC). If there is a large (possibly local) maximum in the spectral density function at frequency \eqn{f}, then the function will return the period \eqn{1/f} (rounded to the nearest integer). If no such dominant frequency can be found, the function will return 1.
}
\value{an integer value
}

\author{Rob J Hyndman}
\examples{
findfrequency(USAccDeaths) # Monthly data
findfrequency(taylor) # Half-hourly data
findfrequency(lynx) # Annual data
}
\keyword{ts}

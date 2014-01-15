\name{easter}
\alias{easter}
\title{Easter holidays in each season}
\usage{easter(x, easter.mon = FALSE)}

\arguments{
\item{x}{Monthly or quarterly time series}
\item{easter.mon}{If TRUE, the length of Easter holidays includes Easter Monday.}
}
\description{Returns a vector of 0's and 1's or fractional results if Easter
  spans March and April in the observed time period. Easter is defined as the days from Good Friday to Easter Sunday inclusively, plus optionally Easter Monday if \code{easter.mon=TRUE}.}

\details{Useful for adjusting calendar effects.}

\value{Time series}


\author{Earo Wang}
\examples{
  easter(wineind, easter.mon = TRUE)
}
\keyword{ts}

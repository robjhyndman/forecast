\name{subset.ts}
\alias{subset.ts}
\title{Subsetting a time series }
\usage{
\method{subset}{ts}(x, subset=NULL, month=NULL, quarter=NULL, season=NULL, ...)
}

\arguments{
\item{x}{a univariate time series to be subsetted}
\item{subset}{optional logical expression indicating elements to keep; missing values are taken as false.}
\item{month}{Character list of months to retain. Partial matching on month names used.}
\item{quarter}{Numeric list of quarters to retain.}
\item{season}{Numeric list of seasons to retain.}
\item{...}{Other arguments, unused.}
}

\description{The main purpose of this function is to extract the values of a specific season in each year. For example, to extract all values for the month of May from a time series.
}

\value{If one season per year is extracted, then a ts object is returned with frequency 1. Otherwise, a numeric vector is returned with no ts attributes.}

\seealso{\code{\link[base]{subset}}}

\author{Rob J Hyndman}
\examples{plot(subset(gas,month="November"))
subset(woolyrnq,quarter=3)
}
\keyword{ts}

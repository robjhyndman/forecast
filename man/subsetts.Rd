\name{subset.ts}
\alias{subset.ts}
\title{Subsetting a time series }
\usage{
\method{subset}{ts}(x, subset=NULL, month=NULL, quarter=NULL, season=NULL, ...)
}

\arguments{
\item{x}{a univariate time series to be subsetted}
\item{subset}{optional logical expression indicating elements to keep; missing values are taken as false. \code{subset} must be the same length as \code{x}.}
\item{month}{Numeric or character vector of months to retain. Partial matching on month names used.}
\item{quarter}{Numeric or character vector of quarters to retain.}
\item{season}{Numeric vector of seasons to retain.}
\item{...}{Other arguments, unused.}
}

\description{Extracts the values of a specific season or subset of seasons in each year. For example, to extract all values for the month of May from a time series.}

\details{If character values for months are used, either upper or lower case may be used, and partial unambiguous names are acceptable. Possible character values for quarters are \code{"Q1"}, \code{"Q2"}, \code{"Q3"}, and \code{"Q4"}.
}

\value{If \code{subset} is used, a numeric vector is returned with no ts attributes. Otherwise, a ts object is returned with frequency equal to the length of \code{month}, \code{quarter} or \code{season}.}

\seealso{\code{\link[base]{subset}}}

\author{Rob J Hyndman}
\examples{plot(subset(gas,month="November"))
subset(woolyrnq,quarter=3)
}
\keyword{ts}

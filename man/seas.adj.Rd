\name{seasadj}
\alias{seasadj}
\title{Seasonal adjustment}
\usage{seasadj(object, ...)
}

\arguments{
\item{object}{Object created by \code{\link[stats]{decompose}}, \code{\link[stats]{stl}} or \code{\link{tbats}}.}
\item{...}{Other arguments not currently used.}
}
\description{Returns seasonally adjusted data constructed by removing the seasonal component.}

\value{Univariate time series.}

\seealso{\code{\link[stats]{stl}}, \code{\link[stats]{decompose}}, \code{\link{tbats}}.}
\author{Rob J Hyndman}
\examples{plot(AirPassengers)
lines(seasadj(decompose(AirPassengers,"multiplicative")),col=4)
}
\keyword{ts}

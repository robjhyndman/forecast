\name{seasadj}
\alias{seasadj}
\title{Seasonal adjustment}
\usage{seasadj(object)
}

\arguments{
\item{object}{Object created by \code{\link[stats]{decompose}} or \code{\link[stats]{stl}}.}
}
\description{Returns seasonally adjusted data constructed by removing the seasonal component.}

\value{Univariate time series.}

\seealso{\code{\link[stats]{stl}}, \code{\link[stats]{decompose}}}
\author{Rob J Hyndman}
\examples{plot(AirPassengers)
lines(seasadj(decompose(AirPassengers,"multiplicative")),col=4)
}
\keyword{ts}

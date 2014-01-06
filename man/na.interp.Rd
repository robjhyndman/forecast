\name{na.interp}
\alias{na.interp}
\title{Interpolate missing values in a time series}
\usage{na.interp(x, lambda = NULL)}

\arguments{  
\item{x}{time series}
\item{lambda}{a numeric value suggesting Box-cox transformation}
}
\description{Uses linear interpolation for non-seasonal series and a periodic stl
  decomposition with seasonal series to replace missing values.}

\details{A more general and flexible approach is available using \code{na.approx} in the \code{zoo} package.}

\value{Time series}

\author{Rob J Hyndman}
\seealso{
  \code{\link[forecast]{na.interp}}, \code{\link[forecast]{tsoutliers}}
}
\examples{
data(gold)
plot(na.interp(gold))
}
\keyword{ts}

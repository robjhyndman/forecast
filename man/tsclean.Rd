\name{tsclean}
\alias{tsclean}
\title{Identify and replace outliers and missing values in a time series}
\usage{tsclean(x, replace.missing = TRUE, lambda = NULL)}

\arguments{  
\item{x}{time series}
\item{replace.missing}{If TRUE, it not only replace outliers, but also interpolate
missing values}
\item{lambda}{a numeric value suggesting Box-Cox transformation}
}
\description{Uses loess for non-seasonal series and a periodic stl decompostion
  with seasonal series to identify and replace outliers. As formissing values, 
  linear interpolation for non-seasonal series and a periodic stl decompostion
  with seasonal series to replace missing values.}

\value{Time series}

\author{Rob J Hyndman}

\seealso{
  \code{\linek[forecast]{na.interp}}, \code{\line[forecast]}{tsoutliers}
}
\examples{
data(gold)
tsclean(gold)
}
\keyword{ts}

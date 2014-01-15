\name{tsclean}
\alias{tsclean}
\title{Identify and replace outliers and missing values in a time series}
\usage{tsclean(x, replace.missing = TRUE, lambda = NULL)}

\arguments{  
\item{x}{time series}
\item{replace.missing}{If TRUE, it not only replaces outliers, but also interpolates missing values}
\item{lambda}{a numeric value giving the Box-Cox transformation parameter}
}
\description{Uses loess for non-seasonal series and a periodic stl decompostion
  with seasonal series to identify and replace outliers. To estimate missing values, 
  linear interpolation is used for non-seasonal series, and a periodic stl decompostion is used
  with seasonal series.}

\value{Time series}

\author{Rob J Hyndman}

\seealso{
  \code{\link[forecast]{na.interp}}, \code{\link[forecast]{tsoutliers}}
}
\examples{
data(gold)
tsclean(gold)
}
\keyword{ts}

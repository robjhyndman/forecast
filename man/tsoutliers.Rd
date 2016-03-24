\name{tsoutliers}
\alias{tsoutliers}
\title{Identify and replace outliers in a time series}
\usage{tsoutliers(x, iterate = 2, lambda = NULL)}

\arguments{  
\item{x}{time series}
\item{iterate}{the number of iteration only for non-seasonal series}
\item{lambda}{Allowing Box-cox transformation}
}
\description{Uses supsmu for non-seasonal series and a periodic stl decompostion
  with seasonal series to identify outliers and estimate their replacements.}

\value{
  \item{index}{Indicating the index of outlier(s)}
  \item{replacement}{Suggested numeric values to replace identified outliers}
}

\author{Rob J Hyndman}
\seealso{
  \code{\link[forecast]{na.interp}}, \code{\link[forecast]{tsclean}}
}
\examples{
data(gold)
tsoutliers(gold)
}
\keyword{ts}

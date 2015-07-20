\name{bizdays}
\alias{bizdays}
\title{Number of trading days in each season}
\usage{bizdays(x, FinCenter = c("New York", "London", "NERC", "Tokyo", "Zurich"))}

\arguments{
\item{x}{Monthly or quarterly time series}
\item{FinCenter}{Major financial center.}
}
\description{Returns number of trading days in each month or quarter of the observed time period in a major financial center.}

\details{Useful for trading days length adjustments. More on how to define "business days",
please refer to \code{\link[timeDate]{isBizday}}.} 
\value{Time series}
\seealso{\code{\link[forecast]{monthdays}}}

\author{Earo Wang}
\examples{
  x <-  ts(rnorm(30), start = c(2013, 2), frequency = 12)
  bizdays(x, FinCenter = "New York")
}
\keyword{ts}

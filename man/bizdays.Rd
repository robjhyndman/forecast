\name{bizdays}
\alias{bizdays}
\title{Number of trading days in each season}
\usage{bizdays(x, FinCenter)}

\arguments{
\item{x}{Monthly or quarterly time series}
\item{FinCenter}{A string indicating the financial center. More details on \code{\link[timeDate]finCenter}.}
}
\description{Returns number of trading days in each month or quarter of the observed time period.}

\details{Useful for trading days length adjustments. More on how to define "business days",
please refer to \code{\link[timeDate]isBizday}.}

\value{Time series}


\author{Yiru Wang}
\examples{
  bizdays(windind, FinCenter = "Sydney")
}
\keyword{ts}

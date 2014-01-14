\name{bizdays}
\alias{bizdays}
\title{Number of trading days in each season}
\usage{bizdays(x, FinCenter)}

\arguments{
\item{x}{Monthly or quarterly time series}
\item{FinCenter}{A character with the the location of the financial center named as "continent/city". 
  This concept allows to handle data records collected in different time zones and mix them up to have
  always the proper time stamps with respect to your personal financial center, or alternatively to the
  GMT reference time. More details on \code{\link[timeDate]{finCenter}}.}
}
\description{Returns number of trading days in each month or quarter of the observed time period.}

\details{Useful for trading days length adjustments. More on how to define "business days",
please refer to \code{\link[timeDate]{isBizday}}.} 
\value{Time series}
\seealso{\code{\link[forecast]{monthdays}}}

\author{Earo Wang}
\examples{
  bizdays(wineind, FinCenter = "Sydney")
}
\keyword{ts}

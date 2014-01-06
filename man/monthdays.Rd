\name{monthdays}
\alias{monthdays}
\title{Number of days in each season}
\usage{monthdays(x)}

\arguments{
\item{x}{time series}
}
\description{Returns number of days in each month or quarter of the observed time period.}

\details{Useful for month length adjustments}

\value{Time series}
\seealso{\code{\link[forecast]{bizdays}}}


\author{Rob J Hyndman}
\examples{
par(mfrow=c(2,1))
plot(ldeaths,xlab="Year",ylab="pounds",
    main="Monthly deaths from lung disease (UK)")
ldeaths.adj <- ldeaths/monthdays(ldeaths)*365.25/12
plot(ldeaths.adj,xlab="Year",ylab="pounds",
    main="Adjusted monthly deaths from lung disease (UK)")
}
\keyword{ts}

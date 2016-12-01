\name{seasonal}
\alias{seasonal}
\alias{trendcycle}
\alias{remainder}
\title{Extract components from a time series decomposition}
\usage{seasonal(object)
trendcycle(object)
remainder(object)
}

\arguments{
\item{object}{Object created by \code{\link[stats]{decompose}}, \code{\link[stats]{stl}} or \code{\link{tbats}}.}
}
\description{Returns a univariate time series equal to either a seasonal component, trend-cycle component or remainder component from a time series decomposition.}

\value{Univariate time series.}

\seealso{\code{\link[stats]{stl}}, \code{\link[stats]{decompose}}, \code{\link{tbats}}, \code{\link{seasadj}}.}
\author{Rob J Hyndman}
\examples{plot(USAccDeaths)
fit <- stl(USAccDeaths, s.window="periodic")
lines(trendcycle(fit),col="red")

library(ggplot2)
autoplot(cbind(
	    Data=USAccDeaths,
	    Seasonal=seasonal(fit),
  	  Trend=trendcycle(fit),
	    Remainder=remainder(fit)),
    facets=TRUE) +
  ylab("") + xlab("Year")
}
\keyword{ts}

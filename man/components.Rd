\name{seasonal}
\alias{seasonal}
\alias{trend}
\alias{irregular}
\title{Extract components from a time series decomposition}
\usage{seasonal(object)
trend(object)
irregular(object)
}

\arguments{
\item{object}{Object created by \code{\link[stats]{decompose}}, \code{\link[stats]{stl}} or \code{\link{tbats}}.}
}
\description{Returns a univariate time series equal to either a seasonal component, trend component or irregular component from a time series decomposition.}

\value{Univariate time series.}

\seealso{\code{\link[stats]{stl}}, \code{\link[stats]{decompose}}, \code{\link{tbats}}, \code{\link{seasadj}}.}
\author{Rob J Hyndman}
\examples{plot(USAccDeaths)
fit <- stl(USAccDeaths, s.window="periodic")
lines(trend(fit),col="red")

library(ggplot2)
autoplot(cbind(
	    Data=USAccDeaths,
	    Seasonal=seasonal(fit),
  	  Trend=trend(fit),
	    Irregular=irregular(fit)),
    facets=TRUE) + 
  ylab("") + xlab("Year")
}
\keyword{ts}

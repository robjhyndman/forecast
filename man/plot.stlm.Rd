\name{plot.stlm}
\alias{plot.stlm}
\title{Plot the STL decomposition and seasonally-adjusted ETS/ARIMA model from a stlm model}
\usage{\method{plot}{stlm}(x, ...)
}

\arguments{
\item{x}{Object of class \dQuote{stlm}.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a plot of the STL decomposition and the non-seasonal ETS/ARIMA that constitute a stlm model.
}

\value{None. Function produces a plot}

\seealso{\code{\link{stlm}}}

\author{David Shaub}
\examples{
fit <- stlm(USAccDeaths)
plot(fit)
}
\keyword{hplot}

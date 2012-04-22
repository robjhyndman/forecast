\name{plot.ets}
\alias{plot.ets}
\title{Plot components from ETS model}
\usage{\method{plot}{ets}(x, ...)
}

\arguments{
\item{x}{Object of class \dQuote{ets}.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}

\description{Produces a plot of the level, slope and seasonal components from an ETS model.}

\value{None. Function produces a plot}

\seealso{\code{\link{ets}}}

\author{Rob J Hyndman}
\examples{
fit <- ets(USAccDeaths)
plot(fit)
plot(fit,plot.type="single",ylab="",col=1:3)
}
\keyword{hplot}

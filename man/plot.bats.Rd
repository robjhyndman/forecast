\name{plot.bats}
\alias{plot.bats}
\alias{plot.tbats}
\alias{autoplot.bats}
\alias{autoplot.tbats}
\title{Plot components from BATS model}
\usage{\method{plot}{bats}(x, main="Decomposition by BATS model", ...) 
\method{autoplot}{bats}(object, range.bars = FALSE, ...) 
\method{plot}{tbats}(x, main="Decomposition by TBATS model", ...) 
\method{autoplot}{tbats}(object, range.bars = FALSE, ...) 
}

\arguments{
\item{x}{Object of class \dQuote{bats/tbats}.}
\item{object}{Object of class \dQuote{bats/tbats}.}
\item{main}{Main title for plot.}
\item{range.bars}{Logical indicating if each plot should have a bar at its right side representing relative size. If NULL, automatic selection takes place.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}

\description{Produces a plot of the level, slope and seasonal components from a BATS or TBATS model.}

\value{None. Function produces a plot}

\seealso{\code{\link{bats}},\code{\link{tbats}}}

\author{Rob J Hyndman}
\examples{
\dontrun{
fit <- tbats(USAccDeaths)
plot(fit)
autoplot(fit, range.bars = TRUE)}
}
\keyword{hplot}

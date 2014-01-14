\name{plot.bats}
\alias{plot.bats}
\alias{plot.tbats}
\title{Plot components from BATS model}
\usage{\method{plot}{bats}(x, main="Decomposition by BATS model", ...) 
\method{plot}{tbats}(x, main="Decomposition by TBATS model", ...) 
}

\arguments{
\item{x}{Object of class \dQuote{ets}.}
\item{main}{Main title for plot.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}

\description{Produces a plot of the level, slope and seasonal components from a BATS or TBATS model.}

\value{None. Function produces a plot}

\seealso{\code{\link{bats}},\code{\link{tbats}}}

\author{Rob J Hyndman}
\examples{
\dontrun{
fit <- tbats(USAccDeaths)
plot(fit)}
}
\keyword{hplot}

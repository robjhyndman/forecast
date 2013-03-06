\name{tbats.components}
\alias{tbats.components}

\title{Extract components of a TBATS model}
\usage{tbats.components(x)
}

\arguments{
\item{x}{A tbats object created by \code{\link{tbats}}.}
}

\description{Extract the level, slope and seasonal components of a TBATS model.}

\value{A multiple time series (\code{mts}) object.}

\references{De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011), Forecasting time series with complex seasonal patterns using exponential smoothing, \emph{Journal of the American Statistical Association}, \bold{106}(496), 1513-1527.
}

\author{Slava Razbash and Rob J Hyndman}

\examples{
fit <- tbats(USAccDeaths, use.parallel=FALSE)
components <- tbats.components(fit)
plot(components)
}

\keyword{ts}


\name{ma}
\alias{ma}
\title{Moving-average smoothing}
\usage{
ma(x, order, centre=TRUE)
}

\arguments{
\item{x}{Univariate time series}
\item{order}{Order of moving average smoother}
\item{centre}{If TRUE, then the moving average is centred.}
}

\description{Computes a simple moving average smoother.}

\value{Numerical time series object containing the smoothed values.}

\seealso{\code{\link[stats]{ksmooth}}, \code{\link[stats]{decompose}}}

\author{Rob J Hyndman}
\examples{
plot(wineind)
sm <- ma(wineind,order=12)
lines(sm,col="red")
}
\keyword{ts}
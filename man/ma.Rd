\name{ma}
\alias{ma}
\title{Moving-average smoothing}
\usage{
ma(x, order, centre=TRUE)
}

\arguments{
\item{x}{Univariate time series}
\item{order}{Order of moving average smoother}
\item{centre}{If TRUE, then the moving average is centred for even orders.}
}

\description{\code{ma} computes a simple moving average smoother of a given time series.}

\value{Numerical time series object containing the simple moving average smoothed values.}

\details{The moving average smoother averages the nearest \code{order} periods of each observation. As neighbouring observations of a time series are likely to be similar in value, averaging eliminates some of the randomness in the data, leaving a smooth trend-cycle component.
\deqn{\hat{T}_{t} = \frac{1}{m} \sum_{j=-k}^k y_{t+j}}{T[t]=1/m(y[t-k]+y[t-k+1]+\ldots+y[t]+\ldots+y[t+k-1]+y[t+k])}
where \eqn{k=\frac{m-1}{2}}{k=(m-1)/2}

When an even \code{order} is specified, the observations averaged will include one more observation from the future than the past (k is rounded up). If centre is TRUE, the value from two moving averages (where k is rounded up and down respectively) are averaged, centering the moving average.}

\seealso{\code{\link[stats]{decompose}}}

\author{Rob J Hyndman}
\examples{
plot(wineind)
sm <- ma(wineind,order=12)
lines(sm,col="red")
}
\keyword{ts}
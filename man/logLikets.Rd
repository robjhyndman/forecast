\name{logLik.ets}
\alias{logLik.ets}

\title{Log-Likelihood of an ets object}
\usage{
\method{logLik}{ets}(object, ...)
}

\arguments{
\item{object}{an object of class \code{ets}, representing an exponential smoothing state space model.}
\item{...}{some methods for this generic require additional arguments. None are used in this method.}
}

\description{Returns the log-likelihood of the ets model represented by \code{object} evaluated at the estimated parameters.}


\value{the log-likelihood of the model represented by \code{object} evaluated at the estimated parameters.}


\references{Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
\emph{Forecasting with exponential smoothing: the state space approach},
Springer-Verlag. \url{http://www.exponentialsmoothing.net}.
}


\seealso{\code{\link{ets}}}

\author{Rob J Hyndman}
\examples{fit <- ets(USAccDeaths)
logLik(fit)
}
\keyword{ts}

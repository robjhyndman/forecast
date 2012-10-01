\name{HoltWintersNew}
\alias{HoltWintersNew}

\title{Holt Winters Seasonal method}
\usage{
HoltWintersNew(x, alpha    = NULL, beta     = NULL, gamma    = NULL,	seasonal = c("additive", "multiplicative"),
				start.periods = 2,	l.start  = NULL, b.start  = NULL, s.start  = NULL, 	
				optim.start = NULL, optim.control = list()
)
}

\arguments{
\item{x}{a numeric vector or time series}
\item{alpha}{Value of alpha. If NULL, it is estimated.}
\item{beta}{Value of beta. If NULL, it is estimated.}
\item{gamma}{Value of gamma. If NULL, it is estimated.}
\item{seasonal}{additive or multiplicative}
\item{start.periods}{currently no use}
\item{l.start}{initial value l0, if NULL, it is estimated according to table 7.9 at http://otexts.com/fpp/7/6/}
\item{b.start}{initial value b0, if NULL, it is estimated according to table 7.9 at http://otexts.com/fpp/7/6/}
\item{s.start}{initial value s0, if NULL, it is estimated according to table 7.9 at http://otexts.com/fpp/7/6/} 
\item{optim.start} {initial values of alpha, beta, gamma while optimisation}
\item{optim.control}{currently no use}
}

\description{Returns Holt Winters model applied to \code{y}.
}
\details{Based on Rob Hyndman's online book: http://otexts.com/fpp/7/5/}






\references{http://otexts.com/fpp/7/5/}


\author{Zhenyu Zhou}
\examples{fit <- HoltWintersNew(austourists))
}
\keyword{ts}

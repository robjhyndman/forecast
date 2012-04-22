\name{simulate.ets}
\alias{simulate.ets}
\alias{simulate.ar}
\alias{simulate.Arima}
\alias{simulate.fracdiff}
\title{Simulation from a time series model}
\usage{
\method{simulate}{ets}(object, nsim=length(object$x), seed=NULL, future=TRUE, 
    bootstrap=FALSE, ...)
\method{simulate}{ar}(object, nsim=object$n.used, seed=NULL, future=TRUE, 
    bootstrap=FALSE, ...)
\method{simulate}{Arima}(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, 
    bootstrap=FALSE, ...)
\method{simulate}{fracdiff}(object, nsim=object$n, seed=NULL, future=TRUE, 
    bootstrap=FALSE, ...)
}

\arguments{
\item{object}{An object of class "\code{ets}", "\code{Arima}" or "\code{ar}".}
\item{nsim}{Number of periods for the simulated series}
\item{seed}{Either NULL or an integer that will be used in a call to \code{\link{set.seed}} before simulating the time series.
    The default, NULL will not change the random generator state.}
\item{future}{Produce sample paths that are future to and conditional on the data in \code{object}.}
\item{bootstrap}{If TRUE, simulation uses resampled errors rather than normally distributed errors.}
\item{xreg}{New values of xreg to be used for forecasting. Must have nsim rows.}
\item{...}{Other arguments.}
}

\description{Returns a time series based on the model object \code{object}.}

\value{An object of class "\code{ts}".}

\seealso{\code{\link{ets}}, \code{\link{Arima}}, \code{\link{auto.arima}}, \code{\link{ar}}, \code{\link{arfima}}.}

\author{Rob J Hyndman}

\examples{fit <- ets(USAccDeaths)
plot(USAccDeaths,xlim=c(1973,1982))
lines(simulate(fit, 36),col="red")
}
\keyword{ts}

\name{simulate.ets}
\alias{simulate.ets}
\alias{simulate.ar}
\alias{simulate.Arima}
\alias{simulate.fracdiff}
\alias{simulate.nnetar}
\title{Simulation from a time series model}
\usage{
\method{simulate}{ets}(object, nsim=length(object$x), seed=NULL, future=TRUE, 
    bootstrap=FALSE, innov=NULL, ...)
\method{simulate}{ar}(object, nsim=object$n.used, seed=NULL, future=TRUE, 
    bootstrap=FALSE,  innov=NULL, ...)
\method{simulate}{Arima}(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, 
    bootstrap=FALSE,  innov=NULL, lambda=object$lambda, ...)
\method{simulate}{fracdiff}(object, nsim=object$n, seed=NULL, future=TRUE, 
    bootstrap=FALSE,  innov=NULL, ...)
\method{simulate}{nnetar}(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, 
    bootstrap=FALSE,  innov=NULL, lambda=object$lambda, ...)
}

\arguments{
\item{object}{An object of class "\code{ets}", "\code{Arima}",
  "\code{ar}" or "\code{nnetar}".}
\item{nsim}{Number of periods for the simulated series}
\item{seed}{Either NULL or an integer that will be used in a call to \code{\link[base]{set.seed}} 
before simulating the time series. The default, NULL will not change the random generator state.}
\item{future}{Produce sample paths that are future to and conditional on the data in \code{object}.}
\item{bootstrap}{If TRUE, simulation uses resampled errors rather than normally distributed errors or errors provided as \code{innov}.}
\item{innov}{A vector of innovations to use as the error series. Ignored if \code{bootstrap==TRUE}.}
\item{xreg}{New values of xreg to be used for forecasting. Must have nsim rows.}
\item{lambda}{Box-Cox parameter. If not \code{NULL}, the simulated series is transformed using an inverse Box-Cox transformation with parameter \code{lamda}.}
\item{...}{Other arguments.}
}

\description{Returns a time series based on the model object \code{object}.}
\details{With \code{simulate.Arima()}, the \code{object} should be produced by \code{\link{Arima}} or \code{\link{auto.arima}}, rather than \code{\link[stats]{arima}}. By default, the error series is assumed normally distributed and generated using \code{\link[stats]{rnorm}}. If \code{innov} is present, it is used instead. If \code{bootstrap=TRUE} and \code{innov=NULL}, the residuals are resampled instead.

When \code{future=TRUE}, the sample paths are conditional on the data. When \code{future=FALSE} and the model is stationary, the sample paths do not depend on the data at all. When \code{future=FALSE} and the model is non-stationary, the location of the sample paths is arbitrary, so they all start at the value of the first observation. 
}

\value{An object of class "\code{ts}".}

\seealso{\code{\link{ets}}, \code{\link{Arima}}, \code{\link{auto.arima}}, \code{\link{ar}}, \code{\link{arfima}}, \code{\link{nnetar}}.}

\author{Rob J Hyndman}

\examples{fit <- ets(USAccDeaths)
plot(USAccDeaths,xlim=c(1973,1982))
lines(simulate(fit, 36),col="red")
}
\keyword{ts}

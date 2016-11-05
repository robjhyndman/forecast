\name{arfima}
\alias{arfima}
\title{Fit a fractionally differenced ARFIMA model}
\description{An ARFIMA(p,d,q) model is selected and estimated automatically using the 
Hyndman-Khandakar (2008) algorithm to select p and q and the Haslett and Raftery (1989) algorithm 
to estimate the parameters including d.
}
\usage{arfima(y, drange=c(0, 0.5), estim=c("mle","ls"), lambda=NULL, biasadj=FALSE, x=y, ...)
}
\arguments{
\item{y}{a univariate time series (numeric vector).}
\item{drange}{Allowable values of d to be considered. Default of \code{c(0,0.5)} ensures a stationary model is returned.}
\item{estim}{If \code{estim=="ls"}, then the ARMA parameters are calculated using the Haslett-Raftery algorithm. If \code{estim=="mle"}, then the ARMA parameters are calculated using full MLE via the \code{\link[stats]{arima}} function.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
\item{x}{Deprecated. Included for backwards compatibility.}
\item{\dots}{Other arguments passed to \code{\link{auto.arima}} when selecting p and q.}
} 
\details{This function combines \code{\link[fracdiff]{fracdiff}} and \code{\link{auto.arima}} to 
automatically select and estimate an ARFIMA model.  The fractional differencing parameter is chosen 
first assuming an ARFIMA(2,d,0) model. Then the data are fractionally differenced using 
the estimated d and an ARMA model is selected for the resulting time series using 
\code{\link{auto.arima}}. Finally, the full ARFIMA(p,d,q) model is re-estimated using 
\code{\link[fracdiff]{fracdiff}}. If \code{estim=="mle"}, the ARMA coefficients are refined using 
\code{\link[stats]{arima}}.} 

\value{A list object of S3 class \code{"fracdiff"}, which is described in the \code{\link[fracdiff]{fracdiff}} 
documentation. A few additional objects are added to the list including \code{x} (the original time series), 
and the \code{residuals} and \code{fitted} values.}

\author{Rob J Hyndman and Farah Yasmeen}

\references{
J. Haslett and A. E. Raftery (1989) Space-time Modelling with Long-memory Dependence: Assessing 
Ireland's Wind Power Resource (with discussion); \emph{Applied Statistics} \bold{38}, 1-50.

Hyndman, R.J. and Khandakar, Y. (2008) "Automatic time series forecasting: The forecast package for R", 
\emph{Journal of Statistical Software}, \bold{26}(3).
}

\seealso{\code{\link[fracdiff]{fracdiff}}, \code{\link{auto.arima}}, \code{\link{forecast.fracdiff}}.}

\examples{
library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
fit <- arfima(x)
tsdisplay(residuals(fit))
}

\keyword{ts}

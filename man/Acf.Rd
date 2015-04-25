\name{Acf}
\alias{Acf}
\alias{Pacf}
\alias{taperedacf}
\alias{taperedpacf}
\title{(Partial) Autocorrelation Function Estimation}
\usage{
Acf(x, lag.max=NULL, type=c("correlation", "partial"), 
   plot=TRUE, main=NULL, xlim=NULL, ylim=NULL, xlab="Lag", ylab=NULL, 
   na.action=na.contiguous, ...)
Pacf(x, main=NULL, ...) 
taperedacf(x, lag.max=NULL, type=c("correlation", "partial"), 
   plot=TRUE, calc.ci=TRUE, level=95, nsim=100, 
   xlim=NULL, ylim=NULL, xlab="Lag",  ylab=NULL, ...)
taperedpacf(x, ...) }

\arguments{
\item{x}{a univariate time series}
\item{lag.max}{maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.}
\item{type}{character string giving the type of acf to be computed. Allowed values are 
"\code{correlation}" (the default) or "\code{partial}".}
\item{plot}{logical. If TRUE (the default) the acf is plotted.}
\item{main}{Title for plot}
\item{xlim}{The x limits of the plot}
\item{ylim}{The y limits of the plot}
\item{xlab}{The label on the x-axis of the plot}
\item{ylab}{The label on the y-axis of the plot}
\item{na.action}{function to handle missing values. Default is \code{\link[stats]{na.contiguous}}.  Useful alternatives are \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.}
\item{calc.ci}{If \code{TRUE}, confidence intervals for the ACF/PACF estimates are calculated.}
\item{level}{Percentage level used for the confidence intervals.}
\item{nsim}{The number of bootstrap samples used in estimating the confidence intervals.}
\item{...}{Additional arguments passed to \code{\link[stats]{acf}} or to the plotting function.}
}

\description{The function \code{Acf} computes (and by default plots) an estimate of the autocorrelation function of a univariate time series. Function \code{Pacf} computes (and by default plots) an estimate of the partial autocorrelation function of a univariate time series.}

\details{The functions improve the \code{\link[stats]{acf}} and \code{\link[stats]{pacf}} functions when applied to univariate time series. The main differences are that \code{Acf} does not plot a spike at lag 0 (which is redundant) and the horizontal axes show lags in time units rather than seasonal units.

The tapered versions implement the ACF and PACF estimates and plots described in Hyndman (2015), based on the banded and tapered estimates of autocovariance proposed by McMurry and Politis (2010).
} 

\references{
  Hyndman, R.J. (2015). Discussion of ``High-dimensional autocovariance matrices and optimal linear prediction''. \emph{Electronic Journal of Statistics}, 9, 792-796.

  McMurry, T. L., & Politis, D. N. (2010). Banded and tapered estimates for autocovariance matrices and the linear process bootstrap. \emph{Journal of Time Series Analysis}, 31(6), 471-482.
}

\value{The \code{Acf} and \code{Pacf} functions return objects of class "acf" as described in \code{\link[stats]{acf}} from the stats package. The \code{taperedacf} and \code{taperedpacf} functions return objects of class "mpacf".}

\seealso{\code{\link[stats]{acf}}, \code{\link[stats]{pacf}}, \code{\link{tsdisplay}}}

\author{Rob J Hyndman}
\examples{
Acf(wineind)
Pacf(wineind)
\dontrun{
taperedacf(wineind, nsim=50)
taperedpacf(wineind, nsim=50)
}
}
\keyword{ts}
\name{Acf}
\alias{Acf}
\alias{Pacf}
\title{(Partial) Autocorrelation Function Estimation}
\usage{
Acf(x, lag.max=NULL, type=c("correlation", "partial"), 
   plot=TRUE, main=NULL, ylim=NULL, na.action=na.contiguous, ...)
Pacf(x, main=NULL, ...) }


\arguments{
\item{x}{a univariate time series}
\item{lag.max}{maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.}
\item{type}{character string giving the type of acf to be computed. Allowed values are "\code{correlation}" (the default) or "\code{partial}".}
\item{plot}{logical. If TRUE (the default) the acf is plotted.}
\item{main}{Title for plot}
\item{ylim}{The y limits of the plot}
\item{na.action}{function to handle missing values. Default is \code{\link[stats]{na.contiguous}}.  Useful alternatives are \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.}
\item{...}{Additional arguments passed to \code{\link[stats]{acf}}.}
}

\description{The function \code{Acf} computes (and by default plots) an estimate of the autocorrelation function of a univariate time series. Function \code{Pacf} computes (and by default plots) an estimate of the partial autocorrelation function of a univariate time series. These improve the \code{\link[stats]{acf}} and \code{\link[stats]{pacf}} functions when applied to univariate time series. 
The main differences are that \code{Acf} does not plot a spike at lag 0 (which is redundant)
and the horizontal axes show lags in time units rather than seasonal units.} 

\details{See the \code{\link[stats]{acf}} function in the stats package.}

\value{See the \code{\link[stats]{acf}} function in the stats package. 
}

\seealso{\code{\link[stats]{acf}}, \code{\link[stats]{pacf}}, \code{\link{tsdisplay}}}

\author{Rob J Hyndman}
\examples{
Acf(wineind)
Pacf(wineind)
}
\keyword{ts}
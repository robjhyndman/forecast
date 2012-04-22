\name{Acf}
\alias{Acf}
\alias{Pacf}
\title{(Partial) Autocorrelation Function Estimation}
\usage{
Acf(x, lag.max=NULL, type=c("correlation", "partial"), 
   plot=TRUE, main=NULL, ...)
Pacf(x, main=NULL, ...) }


\arguments{
\item{x}{a univariate time series}
\item{lag.max}{maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.}
\item{type}{character string giving the type of acf to be computed. Allowed values are "\code{correlation}" (the default) or "\code{partial}".}
\item{plot}{logical. If TRUE (the default) the acf is plotted.}
\item{main}{Title for plot}
\item{...}{Additional arguments passed to \code{\link[stats]{acf}}.}
}

\description{Largely wrappers for the \code{\link[stats]{acf}} function in the stats package. The main difference is that \code{Acf} does not plot a spike at lag 0 (which is redundant). \code{Pacf} is included for consistency.} 

\details{See the \code{\link[stats]{acf}} function in the stats package.}

\value{See the \code{\link[stats]{acf}} function in the stats package. 
}

\seealso{\code{\link[stats]{acf}}}

\author{Rob J Hyndman}
\examples{
Acf(wineind)
Pacf(wineind)
}
\keyword{ts}
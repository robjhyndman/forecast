\name{autoplot.acf}
\alias{autoplot.acf}
\alias{autoplot.mpacf}
\alias{ggAcf}
\alias{ggPacf}
\alias{ggCcf}
\alias{ggtaperedacf}
\alias{ggtaperedpacf}
\title{ggplot (Partial) Autocorrelation and Cross-Correlation Function Estimation}
\usage{\method{autoplot}{acf}(object, ci=0.95, ...)
\method{autoplot}{mpacf}(object, ...)

ggAcf(x, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.contiguous, demean=TRUE, ...)
ggPacf(x, ...)
ggCcf(x, y, lag.max=NULL, type=c("correlation","covariance"),
    plot=TRUE, na.action=na.contiguous, ...)
ggtaperedacf(x, lag.max=NULL, type=c("correlation", "partial"),
   plot=TRUE, calc.ci=TRUE, level=95, nsim=100, ...)
ggtaperedpacf(x, ...)
}

\arguments{
\item{object}{Object of class \dQuote{acf}.}
\item{x}{a univariate or multivariate (not Ccf) numeric time series object or a numeric vector or matrix.}
\item{y}{a univariate numeric time series object or a numeric vector.}
\item{ci}{coverage probability for confidence interval. Plotting of the confidence interval is suppressed if ci is zero or negative.}
\item{lag.max}{maximum lag at which to calculate the acf.}
\item{type}{character string giving the type of acf to be computed. Allowed values are
"\code{correlation}" (the default), "\code{covariance}"  or "\code{partial}".}
\item{plot}{logical. If \code{TRUE} (the default) the resulting acf, pacf or ccf is plotted.}
\item{na.action}{function to handle missing values. Default is \code{\link[stats]{na.contiguous}}.  Useful alternatives are \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.}
\item{demean}{Should covariances be about the sample means?}
\item{calc.ci}{If \code{TRUE}, confidence intervals for the ACF/PACF estimates are calculated.}
\item{level}{Percentage level used for the confidence intervals.}
\item{nsim}{The number of bootstrap samples used in estimating the confidence intervals.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of their equivelent Acf, Pacf, Ccf, taperedacf and taperedpacf functions.

If \code{autoplot} is given an \code{acf} or \code{mpacf} function, then an appropriate ggplot object will be created.}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{plot.acf}}, \code{\link{Acf}}, \code{\link[stats]{acf}},
\code{\link{taperedacf}}}

\author{Mitchell O'Hara-Wild}
\examples{
library(ggplot2)
ggAcf(wineind)
autoplot(Acf(wineind, plot=FALSE))
\dontrun{autoplot(taperedacf(wineind, plot=FALSE))
ggtaperedacf(wineind)
ggtaperedpacf(wineind)}
ggCcf(mdeaths, fdeaths)
}

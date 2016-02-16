\name{autoplot.acf}
\alias{autoplot.acf}
\title{ggplot autocovariance and autocorrelation functions}
\usage{\method{autoplot}{acf}(x, ci=0.95, main=NULL, xlab=NULL, ylab=NULL, ...)
}

\arguments{
\item{x}{Object of class \dQuote{acf}.}
\item{ci}{coverage probability for confidence interval. Plotting of the confidence interval is suppressed if ci is zero or negative.}
\item{main}{Main title. Default is the series name.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of lagged autocorrelations for objects of class "acf"}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{plot.acf}}, \code{\link{Acf}}, \code{\link{Pacf}}, \code{\link[stats]{acf}}}

\author{Mitchell O'Hara-Wild}
\examples{
autoplot(Acf(wineind, plot=FALSE))
}

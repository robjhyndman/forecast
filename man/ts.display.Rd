\name{tsdisplay}
\alias{tsdisplay}
\title{Time series display}
\usage{tsdisplay(x, plot.type="partial", points=TRUE, ci.type="white", 
    lag.max, na.action=na.interp, 
    main=NULL, ylab="", xlab="", pch=1, cex=0.5, ...)
}

\arguments{  \item{x}{a numeric vector or time series.}
\item{plot.type}{type of plot to include in lower right corner. Possible values are "partial", "scatter" or "spectrum".}
\item{points}{logical flag indicating whether to show the individual points or not in the time plot.}
\item{ci.type}{type of confidence limits for ACF. Possible values are as for \code{\link[stats]{acf}}.}
\item{lag.max}{the maximum lag to plot for the acf and pacf. A suitable value is selected by default if the argument is missing.}
\item{na.action}{how to handle missing values. Default is to use linear interpolation.}
\item{main}{Main title.}
\item{ylab}{Y-axis label}
\item{xlab}{X-axis label}
\item{pch}{Plotting character}
\item{cex}{Character size}
\item{\dots}{additional arguments to \code{\link[stats]{acf}}.}
}
\description{Plots a time series along with its acf and either its pacf, lagged scatterplot or spectrum.
}
\value{  None.
}

\references{  Hyndman and Athanasopoulos (2012) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://otexts.com/fpp/}
}
\author{Rob J Hyndman}
\seealso{\code{\link[stats]{plot.ts}}, \code{\link[stats]{acf}}}
\examples{tsdisplay(diff(WWWusage))
}
\keyword{ts}

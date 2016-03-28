\name{tsdisplay}
\alias{tsdisplay}
\alias{ggtsdisplay}
\title{Time series display}
\usage{tsdisplay(x, plot.type=c("partial","scatter","spectrum"),
    points=TRUE, ci.type=c("white", "ma"), 
    lag.max, na.action=na.contiguous, 
    main=NULL, xlab="", ylab="", pch=1, cex=0.5, ...)
    
ggtsdisplay(x, plot.type=c("partial","scatter","spectrum"),
    points=TRUE, lag.max, na.action=na.contiguous, ...)
}

\arguments{  \item{x}{a numeric vector or time series.}
\item{plot.type}{type of plot to include in lower right corner.}
\item{points}{logical flag indicating whether to show the individual points or not in the time plot.}
\item{ci.type}{type of confidence limits for ACF that is passed to \code{\link[stats]{acf}}. Should the confidence limits assume a white noise
                 input or for lag \eqn{k} an MA(\eqn{k-1}) input?}
\item{lag.max}{the maximum lag to plot for the acf and pacf. A suitable value is selected by default if the argument is missing.}
\item{na.action}{function to handle missing values in acf, pacf and spectrum calculations. The default is \code{\link[stats]{na.contiguous}}. Useful alternatives are \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{pch}{Plotting character.}
\item{cex}{Character size.}
\item{\dots}{additional arguments to \code{\link[stats]{acf}}.}
}
\description{Plots a time series along with its acf and either its pacf, lagged scatterplot or spectrum.

\code{ggtsdisplay} will produce the equivelant plot using ggplot graphics.
}
\value{  None.
}

\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}
\author{Rob J Hyndman}
\seealso{\code{\link[stats]{plot.ts}}, \code{\link{Acf}}, \code{\link[stats]{spec.ar}}}
\examples{tsdisplay(diff(WWWusage))
ggtsdisplay(USAccDeaths, plot.type="scatter")
}
\keyword{ts}

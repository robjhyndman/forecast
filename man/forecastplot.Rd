\name{plot.forecast}
\alias{plot.forecast}
\alias{plot.splineforecast}
\title{Forecast plot}

\usage{
\method{plot}{forecast}(x, include, plot.conf=TRUE, shaded=TRUE, 
    shadebars=(length(x$mean)<5), shadecols=NULL, col=1, fcol=4, 
    pi.col=1, pi.lty=2, ylim=NULL, main=NULL, ylab="", xlab="", type="l", ...)

\method{plot}{splineforecast}(x, fitcol=2, type="o", pch=19, ...)
}
\arguments{
\item{x}{Forecast object produced by \code{\link{forecast}}.}
\item{include}{number of values from time series to include in plot}
\item{plot.conf}{Logical flag indicating whether to plot prediction intervals.}
\item{shaded}{Logical flag indicating whether prediction intervals should be shaded (TRUE) or lines (FALSE)}
\item{shadebars}{Logical flag indicating if prediction intervals should be plotted as shaded bars (if TRUE) or a shaded polygon (if FALSE).
    Ignored if \code{shaded=FALSE}. Bars are plotted by default if there are fewer than five forecast horizons.}
\item{shadecols}{Colors for shaded prediction intervals}
\item{col}{the colour for the data line.}
\item{fcol}{the colour for the forecast line.}
\item{pi.col}{If \code{shade=FALSE} and \code{plot.conf=TRUE}, the prediction intervals are plotted in this colour.}
\item{pi.lty}{If \code{shade=FALSE} and \code{plot.conf=TRUE}, the prediction intervals are plotted using this line type.}
\item{ylim}{Limits on y-axis}
\item{main}{Main title}
\item{ylab}{Y-axis label}
\item{xlab}{X-axis label}
\item{fitcol}{Line colour for fitted values.}
\item{type}{1-character string giving the type of plot desired. As for \code{\link[graphics]{plot.default}}.}
\item{pch}{Plotting character (if \code{type=="p"} or \code{type=="o"}).}
\item{\dots}{additional arguments to \code{\link[graphics]{plot}}.} }

\description{Plots historical data with forecasts and prediction intervals.}

\value{None.
}
\references{Makridakis, Wheelwright and Hyndman (1998) \emph{Forecasting:
methods and applications}, Wiley: New York. \url{http://robjhyndman.com/forecasting/}.
}
\author{Rob J Hyndman}
\seealso{\code{\link[stats]{plot.ts}}}
\examples{deaths.fit <- hw(USAccDeaths,h=48)
plot(deaths.fit)
}
\keyword{ts}

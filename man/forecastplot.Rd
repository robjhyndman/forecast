\name{plot.forecast}
\alias{plot.forecast}
\alias{autoplot.forecast}
\alias{plot.splineforecast}
\alias{autoplot.splineforecast}
\title{Forecast plot}

\usage{
\method{plot}{forecast}(x, include, plot.conf=TRUE, shaded=TRUE,
    shadebars=(length(x$mean)<5), shadecols=NULL, col=1, fcol=4,
    pi.col=1, pi.lty=2, ylim=NULL, main=NULL, xlab="", ylab="", type="l",
    flty=1, flwd=2, ...)
\method{autoplot}{forecast}(object, include, plot.conf=TRUE,
    shadecols=c("#596DD5","#D5DBFF"), fcol="#0000AA", flwd=0.5, ...)
\method{plot}{splineforecast}(x, fitcol=2, type="o", pch=19, ...)
\method{autoplot}{splineforecast}(object, plot.conf=TRUE, ...)
}
\arguments{
\item{x}{Forecast object produced by \code{\link{forecast}}.}
\item{object}{Forecast object produced by \code{\link{forecast}}. Used for ggplot graphics (S3 method consistency).}
\item{include}{number of values from time series to include in plot. Default is all values.}
\item{plot.conf}{Logical flag indicating whether to plot prediction intervals.}
\item{shaded}{Logical flag indicating whether prediction intervals should be shaded (TRUE) or lines (FALSE)}
\item{shadebars}{Logical flag indicating if prediction intervals should be plotted as shaded bars (if TRUE) or a shaded polygon (if FALSE).
    Ignored if \code{shaded=FALSE}. Bars are plotted by default if there are fewer than five forecast horizons.}
\item{shadecols}{Colors for shaded prediction intervals. To get default colors used prior to v3.26, set \code{shadecols="oldstyle"}.}
\item{col}{Colour for the data line.}
\item{fcol}{Colour for the forecast line.}
\item{flty}{Line type for the forecast line.}
\item{flwd}{Line width for the forecast line.}
\item{pi.col}{If \code{shade=FALSE} and \code{plot.conf=TRUE}, the prediction intervals are plotted in this colour.}
\item{pi.lty}{If \code{shade=FALSE} and \code{plot.conf=TRUE}, the prediction intervals are plotted using this line type.}
\item{ylim}{Limits on y-axis.}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{fitcol}{Line colour for fitted values.}
\item{type}{1-character string giving the type of plot desired. As for \code{\link[graphics]{plot.default}}.}
\item{pch}{Plotting character (if \code{type=="p"} or \code{type=="o"}).}
\item{...}{Other plotting parameters to affect the plot.}}

\description{Plots historical data with forecasts and prediction intervals.

\code{autoplot} will produce an equivelant plot as a ggplot object.}

\value{None.}

\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}
\author{Rob J Hyndman & Mitchell O'Hara-Wild}
\seealso{\code{\link[stats]{plot.ts}}}
\examples{library(ggplot2)
library(fpp)

deaths.fit <- hw(USAccDeaths,h=48)
plot(deaths.fit)
autoplot(deaths.fit)

fit <- lm(Carbon ~ City, data=fuel)
fcast <- forecast(fit, newdata=data.frame(City=30))
plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")
autoplot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")

fcast <- splinef(uspop,h=5)
plot(fcast)
autoplot(fcast)
}
\keyword{ts}

\name{plot.mforecast}
\alias{plot.mforecast}
\alias{autoplot.mforecast}
\title{Multivariate forecast plot}

\usage{
\method{plot}{mforecast}(x, main=paste("Forecasts from",x$method),xlab="time",...)
\method{autoplot}{mforecast}(x=NULL, plot.conf=TRUE, main=NULL, xlab=NULL, ylab=NULL, gridlayout=NULL, ...)
}
\arguments{
\item{x}{Multivariate forecast object of class \code{mforecast}.}
\item{main}{Main title. Default is the forecast method. For autoplot, specify a vector of titles for each plot.}
\item{xlab}{X-axis label. For autoplot, specify a vector of labels for each plot.}
\item{ylab}{Y-axis label. For autoplot, specify a vector of labels for each plot.}
\item{\dots}{additional arguments to each individual \code{plot}.} }

\description{Plots historical data with multivariate forecasts and prediction intervals.

\code{autoplot} will produce an equivelant plot as a ggplot object.}

\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}
\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[forecast]{plot.forecast}}, \code{\link[stats]{plot.ts}}}
\examples{lungDeaths <- cbind(mdeaths, fdeaths)
fit <- tslm(lungDeaths ~ trend + season)
fcast <- forecast(fit, h=10)
plot(fcast)
autoplot(fcast)

carPower <- as.matrix(mtcars[,c("qsec","hp")])
carmpg <- mtcars[,"mpg"]
fit <- lm(carPower ~ carmpg)
fcast <- forecast(fit, newdata=data.frame(carmpg=30))
plot(fcast, xlab="Year")
autoplot(fcast, xlab=rep("Year",2))
}
\keyword{ts}

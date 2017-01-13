\name{plot.mforecast}
\alias{plot.mforecast}
\alias{autoplot.mforecast}
\alias{autolayer.mforecast}
\title{Multivariate forecast plot}

\usage{
\method{plot}{mforecast}(x, main=paste("Forecasts from",unique(x$method)),xlab="time",...)
\method{autoplot}{mforecast}(object, PI = TRUE, facets = TRUE, colour = FALSE, ...)
\method{autolayer}{mforecast}(object, series = NULL, PI = TRUE, ...)
}
\arguments{
\item{x}{Multivariate forecast object of class \code{mforecast}.}
\item{object}{Multivariate forecast object of class \code{mforecast}. Used for ggplot graphics (S3 method consistency).}
\item{main}{Main title. Default is the forecast method. For autoplot, specify a vector of titles for each plot.}
\item{xlab}{X-axis label. For autoplot, specify a vector of labels for each plot.}
\item{PI}{If \code{FALSE}, confidence intervals will not be plotted, giving only the forecast line.}
\item{facets}{If TRUE, multiple time series will be faceted. If FALSE, each series will be assigned a colour.}
\item{colour}{If TRUE, the time series will be assigned a colour aesthetic}
\item{series}{Matches an unidentified forecast layer with a coloured object on the plot.}
\item{\dots}{additional arguments to each individual \code{plot}.} }

\description{Plots historical data with multivariate forecasts and prediction intervals.

\code{autoplot} will produce an equivelant plot as a ggplot object.}

\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}
\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[forecast]{plot.forecast}}, \code{\link[stats]{plot.ts}}}
\examples{library(ggplot2)

lungDeaths <- cbind(mdeaths, fdeaths)
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

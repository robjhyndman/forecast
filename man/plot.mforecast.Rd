\name{plot.mforecast}
\alias{plot.mforecast}
\title{Multivariate forecast plot}

\usage{
\method{plot}{mforecast}(x, main=paste("Forecasts from",x$method),xlab="time",...)
}
\arguments{
\item{x}{Multivariate forecast object of class \code{mforecast}.}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{\dots}{additional arguments to each individual \code{plot}.} }

\description{Plots historical data with multivariate forecasts and prediction intervals.}

\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}
\author{Rob J Hyndman}
\seealso{\code{\link[forecast]{plot.forecast}}, \code{\link[stats]{plot.ts}}}
\examples{lungDeaths <- cbind(mdeaths, fdeaths)
fit <- tslm(lungDeaths ~ trend + season)
fcast <- forecast(fit, h=10)
plot(fcast)
}
\keyword{ts}

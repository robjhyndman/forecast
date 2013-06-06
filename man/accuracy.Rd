\name{accuracy}
\alias{accuracy}
\title{Accuracy measures for forecast model}
\usage{accuracy(f, x, test="all")
}

\arguments{
\item{f}{An object of class \code{"forecast"}, or a numerical vector containing forecasts. It will also work with \code{Arima}, \code{ets} and \code{lm} objects if \code{x} is omitted -- in which case in-sample accuracy measures are returned.}
\item{x}{An optional numerical vector containing actual values of the same length as object, or a time series overlapping with the times of \code{f}.}
\item{test}{Indicator of which elements of x and f to test. If \code{test=="all"}, all elements are used. Otherwise test is a numeric vector containing the indices of the elements to use in the test.}
}

\description{Returns range of summary measures of the forecast accuracy. If \code{x} is provided, the function measures out-of-sample (test set) forecast accuracy
based on x-f. If \code{x} is not provided, the function only produces in-sample (training set) accuracy measures of the forecasts based on f["x"]-fitted(f).
All measures are defined and discussed in Hyndman and Koehler (2006).}
\details{MASE calculation is scaled using MAE of in-sample naive forecasts for non-seasonal time series, in-sample seasonal naive forecasts for seasonal time series and in-sample mean forecasts for non-time series data.}

\value{Matrix giving forecast accuracy measures.
}

\references{Hyndman, R.J. and Koehler, A.B. (2006) "Another look at measures of forecast accuracy". \emph{International Journal of Forecasting},
\bold{22}(4).}

\author{Rob J Hyndman}
\examples{
fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
accuracy(fit1)
accuracy(fit2)
accuracy(fit1,EuStockMarkets[201:300,1])
accuracy(fit2,EuStockMarkets[201:300,1])
plot(fit1)
lines(EuStockMarkets[1:300,1])
}
\keyword{ts}

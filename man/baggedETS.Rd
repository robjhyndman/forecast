\name{baggedETS}
\alias{baggedETS}

\title{Forecasting using the bagged ETS method}
\usage{baggedETS(x, bootstrapped_series=bld.mbb.bootstrap(x, 100), ...)
}

\arguments{
\item{x}{A numeric vector or time series.}
\item{bootstrapped_series}{bootstrapped versions of x.}
\item{\dots}{Other arguments passed to \code{\link{ets}}.}
}

\description{Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series.}

\details{A feed-forward neural network is fitted with lagged values of \code{y} as inputs and a single hidden layer with \code{size} nodes. The inputs are for lags 1 to \code{p}, and lags \code{m} to \code{mP} where \code{m=frequency(y)}. If there are missing values in \code{y} or \code{xreg}), the corresponding rows (and any others which depend on them as lags) are omitted from the fit. A total of \code{repeats} networks are fitted, each with random starting weights. These are then averaged when computing forecasts. The network is trained for one-step forecasting. Multi-step forecasts are computed recursively. 

For non-seasonal data, the fitted model is denoted as an NNAR(p,k) model, where k is the number of hidden nodes. This is analogous to an AR(p) model but with nonlinear functions. For seasonal data, the fitted model is called an NNAR(p,P,k)[m] model, which is analogous to an ARIMA(p,0,0)(P,0,0)[m] model but with nonlinear functions.
}


\value{Returns an object of class "\code{baggedETS}".

The function \code{print} is used to obtain and print a summary of the
results.

\item{models}{A list containing the fitted ETS ensemble models.}
\item{method}{The name of the forecasting method as a character string}
\item{x}{The original time series.}
\item{bootstrapped_series}{The bootstrapped series.}
\item{etsargs}{The arguments passed through to \code{\link{ets}}.}
\item{fitted}{Fitted values (one-step forecasts). The mean is of the fitted values is calculated over the ensemble.}
\item{residuals}{Original values minus fitted values.}
}

\references{Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging Exponential Smoothing Methods
using STL Decomposition and Box-Cox Transformation. International Journal of Forecasting 32,
303-312.
}

\author{Christoph Bergmeir, Fotios Petropoulos}

\examples{fit <- baggedETS(lynx)
fcast <- forecast(fit)
plot(fcast)
}

\keyword{ts}

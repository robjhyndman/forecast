\name{tbats}
\alias{tbats}

\title{TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)}
\usage{tbats(y, use.box.cox=NULL, use.trend=NULL, use.damped.trend=NULL, 
	seasonal.periods=NULL, use.arma.errors=TRUE, use.parallel=TRUE, num.cores=NULL, ...)}

\arguments{
\item{y}{The time series to be forecast. Can be \code{numeric}, \code{msts} or \code{ts}. Only univariate time series are supported.}
\item{use.box.cox}{\code{TRUE/FALSE} indicates whether to use the Box-Cox transformation or not. If \code{NULL} then both are tried and the best fit is selected by AIC.}
\item{use.trend}{\code{TRUE/FALSE} indicates whether to include a trend or not. If \code{NULL} then both are tried and the best fit is selected by AIC.}
\item{use.damped.trend}{\code{TRUE/FALSE} indicates whether to include a damping parameter in the trend or not. If \code{NULL} then both are tried and the best fit is selected by AIC.}
\item{seasonal.periods}{If \code{y} is \code{numeric} then seasonal periods can be specified with this parameter.}
\item{use.arma.errors}{\code{TRUE/FALSE} indicates whether to include ARMA errors or not. If \code{TRUE} the best fit is selected by AIC. If \code{FALSE} then the selection algorithm does not consider ARMA errors.}
\item{use.parallel}{\code{TRUE/FALSE} indicates whether or not to use parallel processing.}
\item{num.cores}{The number of parallel processes to be used if using parallel processing. If \code{NULL} then the number of logical cores is detected.}
\item{...}{Additional parameters to be passed to \code{auto.arima} when choose an ARMA(p, q) model for the errors.}
}

\description{Fits a TBATS model applied to \code{y}, as described in De Livera, Hyndman & Snyder (2012). Parallel processing is used by default to speed up the computatons.}

\value{An object with class \code{c("tbats", "bats")}. The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{bats} and associated functions.}

\references{De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2012), Forecasting time series with complex seasonal patterns using exponential smoothing, \emph{Journal of the American Statistical Association}, to appear. 
}

\author{Slava Razbash and Rob J Hyndman}

\examples{
fit <- tbats(USAccDeaths)
plot(forecast(fit))
\dontrun{
taylor.fit<-tbats(taylor)
plot(forecast(taylor.fit))
}
}

\keyword{ts}


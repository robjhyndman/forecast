\name{dshw}
\alias{dshw}
\title{Double-Seasonal Holt-Winters Forecasting}
\usage{dshw(y, period1, period2, h=2*max(period1,period2), 
    alpha=NULL, beta=NULL, gamma=NULL, omega=NULL, phi=NULL, 
    lambda=NULL, armethod=TRUE)}

\arguments{
\item{y}{Either an \code{\link{msts}} object with two seasonal periods or a numeric vector.}
\item{period1}{Period of the shorter seasonal period. Only used if \code{y} is not an \code{\link{msts}} object.}
\item{period2}{Period of the longer seasonal period.  Only used if \code{y} is not an \code{\link{msts}} object.}
\item{h}{Number of periods for forecasting.}
\item{alpha}{Smoothing parameter for the level. If \code{NULL}, the parameter is estimated using least squares.}
\item{beta}{Smoothing parameter for the slope. If \code{NULL}, the parameter is estimated using least squares.}
\item{gamma}{Smoothing parameter for the first seasonal period. If \code{NULL}, the parameter is estimated using least squares.}
\item{omega}{Smoothing parameter for the second seasonal period. If \code{NULL}, the parameter is estimated using least squares.}
\item{phi}{Autoregressive parameter. If \code{NULL}, the parameter is estimated using least squares.}
\item{lambda}{Box-Cox transformation parameter. Ignored if \code{NULL}. Otherwise, data transformed before model is estimated.}
\item{armethod}{If TRUE, the forecasts are adjusted using an AR(1) model for the errors.}
}

\description{Returns forecasts using Taylor's (2003) Double-Seasonal Holt-Winters method.}

\details{Taylor's (2003) double-seasonal Holt-Winters method uses additive trend and multiplicative seasonality, where there are two seasonal components which are multiplied together. For example, with a series of half-hourly data, one would set \code{period1=48} for the daily period and \code{period2=336} for the weekly period. The smoothing parameter notation used here is different from that in Taylor (2003); instead it matches that used in Hyndman et al (2008) and that used for the \code{\link{ets}} function.}


\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{meanf}.

An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts)}
}

\references{Taylor, J.W. (2003) Short-term electricity demand forecasting using double seasonal exponential smoothing. \emph{Journal of the Operational Reseach Society}, \bold{54}, 799-805.

Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
\emph{Forecasting with exponential smoothing: the state space approach},
Springer-Verlag. \url{http://www.exponentialsmoothing.net}.
}

\seealso{\code{\link[stats]{HoltWinters}}, \code{\link{ets}}.}

\author{Rob J Hyndman}

\examples{
\dontrun{
fcast <- dshw(taylor)
plot(fcast)
}

t <- seq(0,5,by=1/20)
x <- exp(sin(2*pi*t) + cos(2*pi*t*4) + rnorm(length(t),0,.1))
fit <- dshw(x,20,5)
plot(fit)
}

\keyword{ts}

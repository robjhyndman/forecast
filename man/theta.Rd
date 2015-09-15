\name{thetaf}
\alias{thetaf}
\title{Theta method forecast}
\usage{thetaf(x, h=10, level=c(80,95), fan=FALSE)}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence levels for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(51,99,by=3). This is suitable for fan plots.}
}
\description{Returns forecasts and prediction intervals for a theta method forecast.
}
\details{The theta method of Assimakopoulos and Nikolopoulos (2000) is
equivalent to simple exponential smoothing with drift. This is demonstrated in Hyndman and Billah (2003).
Prediction intervals are computed using the underlying state space model.
}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{rwf}.

An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals}
\item{upper}{Upper limits for prediction intervals}
\item{level}{The confidence values associated with the prediction intervals}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts)}
}

\references{Assimakopoulos, V. and Nikolopoulos, K. (2000). The
theta model: a decomposition approach to forecasting. \emph{International Journal of Forecasting}
\bold{16}, 521-530.

Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method. \emph{International J. Forecasting},
\bold{19}, 287-290.}



\seealso{\code{\link[stats]{arima}}, \code{\link{meanf}}, \code{\link{rwf}}, \code{\link{ses}}}

\author{Rob J Hyndman}
\examples{nile.fcast <- thetaf(Nile)
plot(nile.fcast)
}
\keyword{ts}

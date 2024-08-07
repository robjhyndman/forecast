% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast2.R
\name{meanf}
\alias{meanf}
\title{Mean Forecast}
\usage{
meanf(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  bootstrap = FALSE,
  npaths = 5000,
  x = y
)
}
\arguments{
\item{y}{a numeric vector or time series of class \code{ts}}

\item{h}{Number of periods for forecasting}

\item{level}{Confidence levels for prediction intervals.}

\item{fan}{If TRUE, level is set to seq(51,99,by=3). This is suitable for
fan plots.}

\item{lambda}{Box-Cox transformation parameter. If \code{lambda="auto"},
then a transformation is automatically selected using \code{BoxCox.lambda}.
The transformation is ignored if NULL. Otherwise,
data transformed before model is estimated.}

\item{biasadj}{Use adjusted back-transformed mean for Box-Cox
transformations. If transformed data is used to produce forecasts and fitted values,
a regular back transformation will result in median forecasts. If biasadj is TRUE,
an adjustment will be made to produce mean forecasts and fitted values.}

\item{bootstrap}{If TRUE, use a bootstrap method to compute prediction intervals.
Otherwise, assume a normal distribution.}

\item{npaths}{Number of bootstrapped sample paths to use if \code{bootstrap==TRUE}.}

\item{x}{Deprecated. Included for backwards compatibility.}
}
\value{
An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and
prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals}
extract useful features of the value returned by \code{meanf}.

An object of class \code{"forecast"} is a list containing at least the
following elements: \item{model}{A list containing information about the
fitted model} \item{method}{The name of the forecasting method as a
character string} \item{mean}{Point forecasts as a time series}
\item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
limits for prediction intervals} \item{level}{The confidence values
associated with the prediction intervals} \item{x}{The original time series
(either \code{object} itself or the time series used to create the model
stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
That is x minus fitted values.} \item{fitted}{Fitted values (one-step
forecasts)}
}
\description{
Returns forecasts and prediction intervals for an iid model applied to y.
}
\details{
The iid model is

\deqn{Y_t=\mu + Z_t}{Y[t]=mu + Z[t]}

where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are given by

\deqn{Y_n(h)=\mu}{Y[n+h]=mu}

where \eqn{\mu}{mu} is estimated by the sample mean.
}
\examples{
nile.fcast <- meanf(Nile, h=10)
plot(nile.fcast)

}
\seealso{
\code{\link{rwf}}
}
\author{
Rob J Hyndman
}
\keyword{ts}

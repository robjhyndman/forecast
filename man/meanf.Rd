\name{meanf}
\alias{meanf}
\title{Mean Forecast}
\usage{meanf(x, h=10, level=c(80,95), fan=FALSE, lambda=NULL)}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence levels for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(51,99,by=3). This is suitable for fan plots.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
}
\description{Returns forecasts and prediction intervals for an iid model applied to x.
}
\details{The iid model is
\deqn{Y_t=\mu + Z_t}{Y[t]=mu + Z[t]}
where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are given by
\deqn{Y_n(h)=\mu}{Y[n+h]=mu}
where \eqn{\mu}{mu} is estimated by the sample mean.
}
\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{meanf}.

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

\seealso{\code{\link{rwf}}}

\author{Rob J Hyndman}
\examples{nile.fcast <- meanf(Nile, h=10)
plot(nile.fcast)
}
\keyword{ts}

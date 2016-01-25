\name{rwf}
\alias{rwf}
\title{Random Walk Forecast}
\usage{rwf(x, h=10, drift=FALSE, level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE)}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting}
\item{drift}{Logical flag. If TRUE, fits a random walk with drift model.}
\item{level}{Confidence levels for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(51,99,by=3). This is suitable for fan plots.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
}
\description{Returns forecasts and prediction intervals for a random walk with drift model applied to x.
}
\details{The random walk with drift model is
\deqn{Y_t=c + Y_{t-1} + Z_t}{Y[t]=c + Y[t-1] + Z[t]}
where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are given by
\deqn{Y_n(h)=ch+Y_n}{Y[n+h]=ch+Y[n]}.
If there is no drift, the drift parameter c=0. Forecast standard errors allow
for uncertainty in estimating the drift parameter.
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

\seealso{\code{\link{Arima}}, \code{\link{meanf}}}

\author{Rob J Hyndman}
\examples{gold.fcast <- rwf(gold[1:60],h=50)
plot(gold.fcast)
}
\keyword{ts}

\name{forecast.ets}
\alias{forecast.ets}
\title{Forecasting using ETS models}
\usage{\method{forecast}{ets}(object, h=ifelse(object$m>1, 2*object$m, 10), 
    level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE, 
    npaths=5000, PI=TRUE, lambda=object$lambda, ...)
}

\arguments{
\item{object}{An object of class "\code{ets}". Usually the result of a call to \code{\link{ets}}.}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{simulate}{If TRUE, prediction intervals produced by simulation rather than using analytic formulae.}
\item{bootstrap}{If TRUE, and if \code{simulate=TRUE}, then simulation uses resampled errors rather than normally distributed 
errors.}
\item{npaths}{Number of sample paths used in computing simulated prediction intervals.}
\item{PI}{If TRUE, prediction intervals are produced, otherwise only point forecasts are calculated. If \code{PI} is FALSE, then \code{level}, \code{fan}, \code{simulate}, \code{bootstrap} and \code{npaths} are all ignored.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{...}{Other arguments.}
}

\description{Returns forecasts and other information for univariate ETS models.}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{forecast.ets}.

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

\seealso{\code{\link{ets}}, \code{ses}, \code{holt}, \code{hw}.}

\author{Rob J Hyndman}

\examples{fit <- ets(USAccDeaths)
plot(forecast(fit,h=48))
}
\keyword{ts}

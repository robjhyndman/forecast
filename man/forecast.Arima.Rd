\name{forecast.Arima}
\alias{forecast.Arima}
\alias{forecast.ar}
\alias{forecast.fracdiff}
\title{Forecasting using ARIMA or ARFIMA models}
\usage{\method{forecast}{Arima}(object, h=ifelse(object$arma[5]>1,2*object$arma[5],10),
    level=c(80,95), fan=FALSE, xreg=NULL, lambda=object$lambda, bootstrap=FALSE, npaths=5000, ...)
\method{forecast}{ar}(object, h=10, level=c(80,95), fan=FALSE, lambda=NULL, bootstrap=FALSE, npaths=5000, ...)
\method{forecast}{fracdiff}(object, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ...)
}

\arguments{
\item{object}{An object of class "\code{Arima}", "\code{ar}" or "\code{fracdiff}". Usually the result of a call to 
\code{\link[stats]{arima}}, \code{\link{auto.arima}}, \code{\link[stats]{ar}}, \code{\link{arfima}} or \code{\link[fracdiff]{fracdiff}}.}
\item{h}{Number of periods for forecasting. If \code{xreg} is used, \code{h} is ignored and the number of forecast periods is 
set to the number of rows of \code{xreg}.}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If \code{TRUE}, level is set to \code{seq(50,99,by=1)}. This is suitable for fan plots.}
\item{xreg}{Future values of an regression variables (for class \code{Arima} objects only).}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{bootstrap}{If \code{TRUE}, then prediction intervals computed using simulation with resampled errors.}
\item{npaths}{Number of sample paths used in computing simulated prediction intervals when \code{bootstrap=TRUE}.}
\item{...}{Other arguments.}
}

\description{Returns forecasts and other information for univariate ARIMA models.}

\details{For \code{Arima} or \code{ar} objects, the function calls \code{\link[stats]{predict.Arima}} or \code{\link[stats]{predict.ar}} and 
constructs an object of class "\code{forecast}" from the results. For \code{fracdiff} objects, the calculations are all done
within \code{\link{forecast.fracdiff}} using the equations given by Peiris and Perera (1988).
}

\references{Peiris, M. & Perera, B. (1988), On prediction with fractionally differenced ARIMA models, \emph{Journal of Time Series Analysis},
\bold{9}(3), 215-220.}

\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{forecast.Arima}.

An object of class "\code{forecast}" is a list containing at least the following elements:
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

\seealso{\code{\link[stats]{predict.Arima}}, \code{\link[stats]{predict.ar}}, \code{\link{auto.arima}}, \code{\link{Arima}}, 
\code{\link[stats]{arima}}, \code{\link[stats]{ar}}, \code{\link{arfima}}.}

\author{Rob J Hyndman}

\examples{fit <- Arima(WWWusage,c(3,1,0))
plot(forecast(fit))

library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
fit <- arfima(x)
plot(forecast(fit,h=30))
}
\keyword{ts}

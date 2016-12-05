\name{residuals.Arima}
\alias{residuals.ar}
\alias{residuals.Arima}
\alias{residuals.bats}
\alias{residuals.tbats}
\alias{residuals.ets}
\alias{residuals.forecast}
\alias{residuals.fracdiff}
\alias{residuals.nnetar}
\alias{residuals.stlm}
\alias{arima.errors}
\title{Residuals for various time series models}
\usage{
\method{residuals}{ar}(object, type=c("innovation","response"),...) 
\method{residuals}{Arima}(object, type=c("innovation","response","regression"), h=1, ...) 
\method{residuals}{bats}(object, type=c("innovation","response"), h=1, ...) 
\method{residuals}{ets}(object, type=c("innovation","response"), h=1, ...) 
\method{residuals}{forecast}(object, type=c("innovation","response"), ...) 
\method{residuals}{fracdiff}(object, type=c("innovation","response"), ...) 
\method{residuals}{nnetar}(object, type=c("innovation","response"), h=1, ...) 
\method{residuals}{stlm}(object, type=c("innovation","response"), ...) 
arima.errors(object)
}

\arguments{
\item{object}{An object containing a time series model of class \code{ar}, \code{Arima}, \code{bats}, \code{ets}, \code{fracdiff}, \code{nnetar} or \code{stlm}. If object is of class \code{forecast},  }
\item{type}{Type of residual.}
\item{h}{If \code{type='response'}, then the fitted values are computed for \code{h}-step forecasts.}
\item{...}{Other arguments not used.}
}

\description{Returns time series of residuals from a fitted model.
}

\details{Innovation residuals correspond to the white noise process that drives the evolution of the time series model. Response residuals are the difference between the observations and the fitted values (equivalent to \code{h}-step forecasts). For functions with no \code{h} argument, \code{h=1}. For homoscedastic models, the innovation residuals and the response residuals for \code{h=1} are identical. Regression residuals are available for regression models with ARIMA errors, and are equal to the original data minus the effect of the regression variables. If there are no regression variables, the errors will be identical to the original series (possibly adjusted to have zero mean). 
\code{arima.errors} is a deprecated function which is identical to \code{residuals.Arima(object, type="regression")}.
} 

\value{A \code{ts} object}

\seealso{\code{\link{fitted.Arima}}, \code{\link{checkresiduals}}.}

\author{Rob J Hyndman}

\examples{fit <- Arima(lynx,order=c(4,0,0), lambda=0.5)

plot(residuals(fit))
plot(residuals(fit, type='response'))
}
\keyword{ts}

\name{residuals.Arima}
\alias{residuals.Arima}
\alias{residuals.ets}
\alias{residuals.stlm}
\title{Residuals for various time series models}
\usage{\method{residuals}{Arima}(object, type=c("innovation","response"),...) 
\method{residuals}{ets}(object, type=c("innovation","response"),...) 
\method{residuals}{stlm}(object, type=c("innovation","response"),...) 
}

\arguments{
\item{object}{An object of class "\code{Arima}", "\code{ets}" or "\code{stlm}". }
\item{type}{Type of residual. Innovation residuals should be white noise. Response residuals are the difference between observations and fitted values (equivalent to one-step forecasts)}
\item{...}{Other arguments not used.}
}

\description{Returns time series of residuals.}

\value{A \code{ts} object}

\seealso{\code{\link{fitted.Arima}}.}

\author{Rob J Hyndman}

\examples{fit <- Arima(lynx,order=c(4,0,0), lambda=0.5)

plot(residuals(fit))
plot(residuals(fit, type='response'))
}
\keyword{ts}

\name{arima.errors}
\alias{arima.errors}
\title{ARIMA errors}
\usage{arima.errors(z)}

\arguments{ \item{z}{Fitted ARIMA model from \code{\link[stats]{arima}}} } 

\description{Returns original time series after adjusting for 
regression variables. These are not the same as the residuals. 
If there are no regression variables in the ARIMA model, then 
the errors will be identical to the original series. If there 
are regression variables in the ARIMA model, then the errors 
will be equal to the original series minus the effect of the 
regression variables, but leaving in the serial correlation 
that is modelled with the AR and MA terms. If you want the 
"residuals", then use \code{residuals(z).}. } 

\value{A time series containing the "errors".
}

\author{Rob J Hyndman}

\seealso{\code{\link[stats]{arima}}, \code{\link[stats]{residuals}}}

\examples{ukdeaths.fit <- Arima(UKDriverDeaths,c(1,0,1),c(0,1,1),xreg=Seatbelts[,"law"])
ukdeaths.errors <- arima.errors(ukdeaths.fit)
par(mfrow=c(2,1))
plot(UKDriverDeaths)
plot(ukdeaths.errors)
}
\keyword{ts}

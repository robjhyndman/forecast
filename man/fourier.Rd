\name{fourier}
\alias{fourier}
\alias{fourierf}
\title{Fourier terms for modelling seasonality}
\usage{fourier(x,K,h)
}

\arguments{
\item{x}{Seasonal time series: a \code{ts} or a \code{msts} object}
\item{K}{Maximum order(s) of Fourier terms}
\item{h}{Number of periods ahead to forecast (optional)}
}
\description{\code{fourier} returns a matrix containing terms from a Fourier series, up to order \code{K}, suitable for use in \code{\link{Arima}}, \code{\link{auto.arima}}, or \code{\link{tslm}}.

\code{fourierf} is deprecated, instead use the \code{h} argument in \code{fourier}.
}

\details{The period of the Fourier terms is determined from the time series characteristics of \code{x}. When \code{h} is missing, the length of \code{x} also determines the number of rows for the matrix returned by \code{fourier}. Otherwise, the value of \code{h} determines the number of rows for the matrix returned by \code{fourier}, typically used for forecasting. The values within \code{x} are not used.

When \code{x} is a \code{ts} object, the value of \code{K} should be an integer and specifies the number of sine and cosine terms to return. Thus, the matrix returned has \code{2*K} columns.

When \code{x} is a \code{msts} object, then \code{K} should be a vector of integers specifying the number of sine and cosine terms for each of the seasonal periods. Then the matrix returned will have \code{2*sum(K)} columns.
}

\value{Numerical matrix.}

\seealso{\code{\link{seasonaldummy}}}

\author{Rob J Hyndman}
\examples{
library(ggplot2)

# Using Fourier series for a "ts" object
# K is chosen to minimize the AICc
deaths.model  <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths,K=5), seasonal=FALSE)
deaths.fcast <- forecast(deaths.model, xreg=fourier(USAccDeaths, K=5, h=36))
autoplot(deaths.fcast) + xlab("Year")

# Using Fourier series for a "msts" object
taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
taylor.fcast <- forecast(taylor.lm,
    data.frame(fourier(taylor, K = c(3, 3), h = 270)))
autoplot(taylor.fcast)
}
\keyword{ts}

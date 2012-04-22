\name{croston}
\alias{croston}
\title{Forecasts for intermittent demand using Croston's method}
\usage{croston(x, h=10, alpha=0.1)
}

\arguments{
\item{x}{a numeric vector or time series}
\item{h}{Number of periods for forecasting.}
\item{alpha}{Value of alpha. Default value is 0.1.}
}

\description{Returns forecasts and other information for Croston's
forecasts applied to x. }

\details{Based on Croston's (1972) method for intermittent demand
forecasting, also described in Shenstone and Hyndman (2005).
Croston's method involves using simple exponential smoothing (SES) on
the non-zero elements of the time series and a separate application
of SES to the times between non-zero elements of the time series. The
smoothing parameters of the two applications of SES are assumed to be
equal and are denoted by \code{alpha}.

Note that prediction intervals are not computed as Croston's method
has no underlying stochastic model.}

\value{
An object of class \code{"forecast"} is a list containing at least the following elements:
\item{model}{A list containing information about the fitted model. The first element gives the SES model used for non-zero demands.
 The second element gives the SES model used for times between non-zero demands. Both models are of class \code{forecast}.}
\item{method}{The name of the forecasting method as a character string}
\item{mean}{Point forecasts as a time series}
\item{x}{The original time series (either \code{object} itself or the time series used to create the model stored as \code{object}).}
\item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
\item{fitted}{Fitted values (one-step forecasts)}

The function \code{summary} is used to obtain and print a summary of
the results, while the function \code{plot} produces a plot of the
forecasts.

The generic accessor functions \code{fitted.values} and
\code{residuals} extract useful features of the value returned by
\code{croston} and associated functions.
}

\references{Croston, J. (1972) "Forecasting and stock control for intermittent demands",
\emph{Operational Research Quarterly}, \bold{23}(3), 289-303.

Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying Croston's method for intermittent
demand forecasting". \emph{Journal of Forecasting}, \bold{24}, 389-402.
}



\seealso{\code{\link{ses}}.}

\author{Rob J Hyndman}
\examples{x <- rpois(20,lambda=.3)
fcast <- croston(x)
plot(fcast)
}
\keyword{ts}

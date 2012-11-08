\name{forecast.StructTS}
\alias{forecast.StructTS}
\title{Forecasting using Structural Time Series models}
\usage{\method{forecast}{StructTS}(object, 
    h=ifelse(object$coef["epsilon"] > 1e-10, 2*object$xtsp[3],10),
    level=c(80,95), fan=FALSE, lambda=NULL, ...)
}

\arguments{
\item{object}{An object of class "\code{StructTS}". Usually the result of a call to \code{\link[stats]{StructTS}}.}
\item{h}{Number of periods for forecasting}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{...}{Other arguments.}
}

\description{Returns forecasts and other information for univariate structural time series models.}

\details{This function calls \code{predict.StructTS} and constructs an object of class "\code{forecast}" from the results.
}


\value{An object of class "\code{forecast}".

The function \code{summary} is used to obtain and print a summary of the
results, while the function \code{plot} produces a plot of the forecasts and prediction intervals.

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{forecast.StructTS}.

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

\seealso{\code{\link[stats]{StructTS}}.}

\author{Rob J Hyndman}

\examples{fit <- StructTS(WWWusage,"level")
plot(forecast(fit))
}
\keyword{ts}

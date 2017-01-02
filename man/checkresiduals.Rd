\name{checkresiduals}
\alias{checkresiduals}
\title{Check that residuals from a time series model look like white noise}
\usage{checkresiduals(object, lag, df=NULL, ...)
}
\arguments{
\item{object}{Either a time series model, a forecast object, or a time series (assumed to be residuals).}
\item{lag}{Number of lags to use in the Ljung-Box test. If missing, it is set to \code{max(10,df+3)} for non-seasonal data, and \code{max(2m, df+3)} for seasonal data, where \code{df} is the degrees of freedom of the model, and \code{m} is the seasonal period of the data.}
\item{df}{Number of degrees of freedom for fitted model, required for the Ljung-Box test. Ignored if the degrees of freedom can be extracted from \code{object}.}
\item{...}{Other arguments are passed to \code{\link{ggtsdisplay}}.}
}

\description{Produces a plot of the residuals, the corresponding ACF and histogram. If the degrees of freedom for the model can be determined, the output from a Ljung-Box test is returned.
}

\value{None}

\author{Rob J Hyndman}
\seealso{\code{\link{ggtsdisplay}}, \code{\link[stats]{Box.test}}}
\examples{
fit <- ets(WWWusage)
checkresiduals(fit)
}


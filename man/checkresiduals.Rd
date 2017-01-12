\name{checkresiduals}
\alias{checkresiduals}
\title{Check that residuals from a time series model look like white noise}
\usage{checkresiduals(object, lag, df=NULL, test, ...)
}
\arguments{
\item{object}{Either a time series model, a forecast object, or a time series (assumed to be residuals).}
\item{lag}{Number of lags to use in the Ljung-Box or Breusch-Godfrey test. If missing, it is set to \code{max(10,df+3)} for non-seasonal data, and \code{max(2m, df+3)} for seasonal data, where \code{df} is the degrees of freedom of the model, and \code{m} is the seasonal period of the data.}
\item{df}{Number of degrees of freedom for fitted model, required for the Ljung-Box or Breusch-Godfrey test. Ignored if the degrees of freedom can be extracted from \code{object}.}
\item{test}{Test to use for serial correlation. By default, if \code{object} is of class \code{lm}, then \code{test="BG"}. Otherwise, \code{test="LB"}.}
\item{...}{Other arguments are passed to \code{\link{ggtsdisplay}}.}
}

\description{Produces a time plot of the residuals, the corresponding ACF, and a histogram. If the degrees of freedom for the model can be determined, the output from either a Breusch-Godfrey test or a Ljung-Box test is printed.
}

\value{None}

\author{Rob J Hyndman}
\seealso{\code{\link{ggtsdisplay}}, \code{\link[stats]{Box.test}}, \code{\link[lmtest]{bgtest}}}
\examples{
fit <- ets(WWWusage)
checkresiduals(fit)
}


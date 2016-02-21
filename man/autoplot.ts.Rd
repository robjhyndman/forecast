\name{autoplot.ts}
\alias{autoplot.ts}
\alias{fortify.ts}
\alias{autoplot.mts}
\title{Automatically create a ggplot for time series objects}
\usage{\method{autoplot}{ts}(object, main=NULL, xlab="Time", ylab=substitute(x), ...)
\method{autoplot}{mts}(object, main=NULL, xlab="Time", ylab=substitute(x), ...)
\method{fortify}{ts}(model,data)
}

\arguments{
\item{object}{Object of class \dQuote{ts} or \dQuote{mts}.}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{model}{Object of class \dQuote{ts} to be converted to \dQuote{data.frame}.}
\item{data}{Not used (required for \link{fortify} method)}
}

\description{\code{autoplot} takes an object of type \code{ts} or \code{mts} and creates a ggplot object suitable for usage with \code{stat_forecast}.

\code{fortify.ts} takes a \code{ts} object and converts it into a data frame (for usage with ggplot2).}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{plot.ts}}, \code{\link[ggplot2]{fortify}}}

\author{Mitchell O'Hara-Wild}
\examples{
library(ggplot2)
autoplot(USAccDeaths)

lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths)
}

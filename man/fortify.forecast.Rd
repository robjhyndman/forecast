\name{fortify.forecast}
\alias{fortify.forecast}
\title{Fortify a forecast object to data.frame for ggplot}
\usage{\method{fortify}{forecast}(model, data=as.data.frame(model), PI=TRUE, ...)
}

\arguments{
\item{model}{Object of class \dQuote{ts} to be converted to \dQuote{data.frame}.}
\item{data}{Not used (required for \link{fortify} method)}
\item{PI}{If TRUE, confidence intervals are included.}
\item{...}{Other arguments, currently ignored.}
}

\description{\code{fortify.forecast} takes a \code{forecast} object and converts it into a data frame (for usage with ggplot2).}

\value{A \code{data.frame} containing elements from a \code{forecast} object necessary for plotting in ggplot.}

\seealso{\code{\link{forecast}}, \code{\link[ggplot2]{fortify}}}

\author{Mitchell O'Hara-Wild}

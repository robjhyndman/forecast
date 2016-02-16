\name{autoplot.decomposed.ts}
\alias{autoplot.decomposed.ts}
\title{ggplot of a decomposed time series object}
\usage{\method{autoplot}{decomposed.ts}(x, main=NULL, xlab=NULL, ylab=NULL, ...)
}

\arguments{
\item{x}{Object of class \dQuote{decomposed.ts} to be plotted.}
\item{main}{Main title. Default is the series name.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of decomposed components of a time series object}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{decompose}}, \link[stats]{stl}}}

\author{Mitchell O'Hara-Wild}
\examples{
m <- decompose(co2)
autoplot(m)
}

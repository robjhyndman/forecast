\name{autoplot.seas}
\alias{autoplot.seas}
\title{X-13-ARIMA decomposition ggplot}
\usage{\method{autoplot}{seas}(object, labels = NULL, ...)
}

\arguments{
\item{object}{Object of class \dQuote{acf}.}
\item{labels}{Labels to replace "seasonal", "trend", and "remainder".}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of seasonally decomposed time series for objects of class "stl"}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[seasonal]{seas}}}

\author{Mitchell O'Hara-Wild}
\examples{
library(seasonal)
library(ggplot2)
m <- seas(USAccDeaths)
autoplot(m)
}

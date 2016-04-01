\name{autoplot.stl}
\alias{autoplot.stl}
\title{ggplot STL object}
\usage{\method{autoplot}{stl}(object, labels = NULL, ...)
}

\arguments{
\item{object}{Object of class \dQuote{acf}.}
\item{labels}{Labels to replace "seasonal", "trend", and "remainder".}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of seasonally decomposed time series for objects of class "stl"}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{stl}}, \link[stats]{plot.stl}}

\author{Mitchell O'Hara-Wild}
\examples{plot(stl(nottem, "periodic"))

library(ggplot2)
autoplot(stl(nottem, "periodic"))
}

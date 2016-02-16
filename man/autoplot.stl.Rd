\name{autoplot.acf}
\alias{autoplot.acf}
\title{ggplot STL object}
\usage{\method{autoplot}{stl}(x=NULL, labels = NULL, main=NULL, xlab="Date", ylab="", ...)
}

\arguments{
\item{x}{Object of class \dQuote{acf}.}
\item{labels}{Labels to replace "seasonal", "trend", and "remainder".}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of seasonally decomposed time series for objects of class "stl"}

\value{None. Function produces a ggplot graph.}

\seealso{\code{\link[stats]{stl}}, \link[stats]{plot.stl}}}

\author{Mitchell O'Hara-Wild}
\examples{plot(stl(nottem, "per"))
autoplot(stl(nottem, "per"))
}

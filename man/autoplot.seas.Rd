\name{autoplot.seas}
\alias{autoplot.seas}
\alias{autoplot.stl}
\alias{autoplot.decomposed.ts}
\alias{autoplot.StructTS}
\title{Plot time series decomposition components using ggplot}
\usage{\method{autoplot}{seas}(object, labels = NULL, range.bars = NULL, ...)
\method{autoplot}{stl}(object, labels = NULL, range.bars = TRUE, ...)
\method{autoplot}{decomposed.ts}(object, labels = NULL, range.bars = NULL, ...)
\method{autoplot}{StructTS}(object, labels = NULL, range.bars = TRUE, ...)
}

\arguments{
\item{object}{Object of class \dQuote{seas}, \dQuote{stl}, or \dQuote{decomposed.ts}.}
\item{labels}{Labels to replace "seasonal", "trend", and "remainder".}
\item{range.bars}{Logical indicating if each plot should have a bar at its right side representing relative size. If NULL, automatic selection takes place.}
\item{...}{Other plotting parameters to affect the plot.}
}

\description{Produces a ggplot object of seasonally decomposed time series for objects of class "stl" (created with \code{\link[stats]{stl}}), class "seas" (created with \code{\link[seasonal]{seas}}), or  class "decomposed.ts" (created with \code{\link[stats]{decompose}}).
}

\value{Returns an object of class \code{ggplot}.
}

\seealso{\code{\link[seasonal]{seas}}, \code{\link[stats]{stl}}, \code{\link[stats]{decompose}}, \code{\link[stats]{StructTS}}, \code{\link[stats]{plot.stl}}.
}

\author{Mitchell O'Hara-Wild}
\examples{
library(ggplot2)
co2 \%>\% decompose \%>\% autoplot
nottem \%>\% stl(s.window='periodic') \%>\% autoplot

\dontrun{
library(seasonal)
m <- seas(USAccDeaths)
autoplot(m)
}
}

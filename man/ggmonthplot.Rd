\name{ggmonthplot}
\alias{ggmonthplot}
\title{Create a seasonal subseries ggplot}
\usage{ggmonthplot(x, labels = NULL, times = time(x), phase = cycle(x),
    ylab = deparse(substitute(x)), ...)
}
\arguments{
\item{x}{a time series object (type \code{ts}).}
\item{labels}{A vector of labels to use for each 'season'}
\item{times}{A vector of times for each observation}
\item{phase}{A vector of seasonal components}
\item{ylab}{Y-axis label.}
\item{\dots}{Not used (for consistency with monthplot)}
}
\description{Plots a month plot using ggplot.}

\value{None.
}

\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[stats]{monthplot}}}
\examples{
library(fpp)
ggmonthplot(AirPassengers)
ggmonthplot(bricksq)
}
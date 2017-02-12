\name{ggmonthplot}
\alias{ggmonthplot}
\alias{ggsubseriesplot}
\title{Create a seasonal subseries ggplot}
\usage{ggsubseriesplot(x, labels = NULL, times = time(x), phase = cycle(x), ...)
	ggmonthplot(x, labels = NULL, times = time(x), phase = cycle(x), ...)
}
\arguments{
\item{x}{a time series object (type \code{ts}).}
\item{labels}{A vector of labels to use for each 'season'}
\item{times}{A vector of times for each observation}
\item{phase}{A vector of seasonal components}
\item{\dots}{Not used (for consistency with monthplot)}
}
\description{Plots a subseries plot using ggplot. Each season is plotted as a separate mini time series. The blue lines represent the mean of the observations within each season.

The \code{ggmonthplot} function is simply a wrapper for \code{ggsubseriesplot} as a convenience for users familiar with \code{\link[stats]{monthplot}}.
}

\value{Returns an object of class \code{ggplot}.
}

\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[stats]{monthplot}}}
\examples{
ggsubseriesplot(AirPassengers)
ggsubseriesplot(woolyrnq)
}

\name{gglagplot}
\alias{gglagplot}
\title{Time series lag ggplots}
\usage{gglagplot(x, lags = 1, set.lags = 1:lags, diag=TRUE,
  diag.col="gray", do.lines = TRUE, colourlines = TRUE, labels = !do.lines,
  seasonal = TRUE, ...)
}
\arguments{
\item{x}{a time series object (type \code{ts}).}
\item{lags}{number of lag plots desired, see arg set.lags.}
\item{set.lags}{vector of positive integers specifying which lags to use.}
\item{diag}{logical indicating if the x=y diagonal should be drawn.}
\item{diag.col}{color to be used for the diagonal if(diag).}
\item{do.lines}{logical indicating if lines should be drawn.}
\item{colourlines}{logical indicating if lines should be coloured.}
\item{labels}{logical indicating if labels should be used.}
\item{seasonal}{Should the line colour be based on seasonal characteristics (TRUE), or sequential (FALSE).}
\item{\dots}{Not used (for consistency with lag.plot)}
}
\description{Plots a lag plot using ggplot.}

\value{None.
}

\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[stats]{lag.plot}}}
\examples{
gglagplot(AirPassengers)
gglagplot(AirPassengers,seasonal=FALSE)

lungDeaths <- cbind(mdeaths, fdeaths)
gglagplot(lungDeaths, lags=2)
}
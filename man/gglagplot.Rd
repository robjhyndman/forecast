\name{gglagplot}
\alias{gglagplot}
\alias{gglagchull}
\title{Time series lag ggplots}
\usage{gglagplot(x, lags = 1, set.lags = 1:lags, diag=TRUE,
  diag.col="gray", do.lines = TRUE, colour = TRUE, continuous = TRUE,
  labels = FALSE, seasonal = TRUE, ...)
gglagchull(x, lags = 1, set.lags = 1:lags, diag=TRUE,
  diag.col="gray", ...)
}
\arguments{
\item{x}{a time series object (type \code{ts}).}
\item{lags}{number of lag plots desired, see arg set.lags.}
\item{set.lags}{vector of positive integers specifying which lags to use.}
\item{diag}{logical indicating if the x=y diagonal should be drawn.}
\item{diag.col}{color to be used for the diagonal if(diag).}
\item{do.lines}{if TRUE, lines will be drawn, otherwise points will be drawn.}
\item{colour}{logical indicating if lines should be coloured.}
\item{continuous}{Should the colour scheme for years be continuous or discrete?}
\item{labels}{logical indicating if labels should be used.}
\item{seasonal}{Should the line colour be based on seasonal characteristics (TRUE), or sequential (FALSE).}
\item{\dots}{Not used (for consistency with lag.plot)}
}
\description{Plots a lag plot using ggplot.

\dQuote{gglagplot} will plot time series against lagged versions of themselves. Helps visualising 'auto-dependence' even when auto-correlations vanish.

\dQuote{gglagchull} will layer convex hulls of the lags, layered on a single plot. This helps visualise the change in 'auto-dependence' as lags increase.
}

\value{None.
}

\author{Mitchell O'Hara-Wild}
\seealso{\code{\link[stats]{lag.plot}}}
\examples{
gglagplot(AirPassengers)
gglagchull(AirPassengers)
gglagplot(AirPassengers,seasonal=FALSE)

lungDeaths <- cbind(mdeaths, fdeaths)
gglagplot(lungDeaths, lags=2)
gglagchull(lungDeaths, lags=6)
}

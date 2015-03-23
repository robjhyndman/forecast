\name{seasonplot}
\alias{seasonplot}
\title{Seasonal plot}
\usage{seasonplot(x, s, season.labels=NULL, year.labels=FALSE,
    year.labels.left=FALSE, type="o", main, xlab=NULL, ylab="",
    col=1, labelgap=0.1, ...)
}
\arguments{
\item{x}{a numeric vector or time series.}
\item{s}{seasonal frequency of x}
\item{season.labels}{Labels for each season in the "year"}
\item{year.labels}{Logical flag indicating whether labels for each year of data should be plotted on the right.}
\item{year.labels.left}{Logical flag indicating whether labels for each year of data should be plotted on the left.}
\item{type}{plot type (as for \code{\link[graphics]{plot}})}
\item{main}{Main title.}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{col}{Colour}
\item{labelgap}{Distance between year labels and plotted lines}
\item{\dots}{additional arguments to \code{\link[graphics]{plot}}.} }
\description{Plots a seasonal plot as described in Hyndman and Athanasopoulos (2014, chapter 2).
}

\value{None.
}
\references{Hyndman and Athanasopoulos (2014) \emph{Forecasting: principles and practice}, OTexts: Melbourne, Australia. \url{http://www.otexts.org/fpp/}
}

\author{Rob J Hyndman}
\seealso{\code{\link[stats]{monthplot}}}
\examples{seasonplot(AirPassengers,col=rainbow(12),year.labels=TRUE)
}
\keyword{ts}

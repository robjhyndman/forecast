\name{na.interp}
\alias{na.interp}
\title{Interpolate missing values in a time series}
\usage{na.interp(x)}

\arguments{  
\item{x}{time series}
}
\description{Uses linear interpolation to replace missing values.}

\details{A more general and flexible approach is available using \code{na.approx} in the \code{zoo} package.}

\value{Time series}

\author{Rob J Hyndman}
\examples{
data(gold)
plot(na.interp(gold))
}
\keyword{ts}

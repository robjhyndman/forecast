\name{arimaorder}
\alias{arimaorder}
\title{Return the order of an ARIMA or ARFIMA model}
\usage{arimaorder(object)
}

\arguments{
\item{object}{An object of class "\code{Arima}", "\code{ar}" or "\code{fracdiff}". Usually the result of a call to 
\code{\link[stats]{arima}}, \code{\link{Arima}}, \code{\link{auto.arima}}, \code{\link[stats]{ar}}, \code{\link{arfima}} or \code{\link[fracdiff]{fracdiff}}.}
}

\description{Returns the order of a univariate ARIMA or ARFIMA model.}

\value{A numerical vector giving the values \eqn{p}, \eqn{d} and \eqn{q} of the ARIMA or ARFIMA model. For a seasonal ARIMA model, the returned vector contains the values \eqn{p}, \eqn{d}, \eqn{q}, \eqn{P}, \eqn{D}, \eqn{Q} and \eqn{m}, where \eqn{m} is the period of seasonality.}

\seealso{\code{\link[stats]{ar}}, \code{\link{auto.arima}}, \code{\link{Arima}}, 
\code{\link[stats]{arima}}, \code{\link{arfima}}.}

\author{Rob J Hyndman}

\examples{arimaorder(auto.arima(WWWusage))
}
\keyword{ts}

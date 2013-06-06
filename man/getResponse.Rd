\name{getResponse}
\alias{getResponse}
\title{Get response variable from time series model.}


\description{
\code{getResponse} is a generic function for extracting the historical data from a time series model (including \code{Arima}, \code{ets}, \code{ar}, \code{fracdiff}), a linear model of class \code{lm}, or a forecast object. The function invokes particular \emph{methods} which depend on the class of the first argument.}

\usage{getResponse(object,...)
}

\arguments{
\item{object}{a time series model or forecast object.}
\item{...}{Additional arguments that are ignored.}
}
\value{A numerical vector or a time series object of class \code{ts}.}

\author{Rob J Hyndman}
\keyword{ts}

\name{plot.Arima}
\alias{plot.Arima}
\alias{autoplot.Arima}
\alias{plot.ar}
\alias{autoplot.ar}
\title{Plot characteristic roots from ARIMA model}
\usage{\method{plot}{Arima}(x, type=c("both","ar","ma"),
  main, xlab="Real", ylab="Imaginary", ...)
\method{autoplot}{Arima}(object, type=c("both","ar","ma"), ...)
\method{plot}{ar}(x, main, xlab="Real", ylab="Imaginary", ...)
\method{autoplot}{ar}(object, ...)
}

\arguments{
\item{x}{Object of class \dQuote{Arima} or \dQuote{ar}.}
\item{object}{Object of class \dQuote{Arima} or \dQuote{ar}. Used for ggplot graphics (S3 method consistency).}
\item{type}{Determines if both AR and MA roots are plotted, of if just one set is plotted.}
\item{main}{Main title. Default is "Inverse AR roots" or "Inverse MA roots".}
\item{xlab}{X-axis label.}
\item{ylab}{Y-axis label.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}

\description{Produces a plot of the inverse AR and MA roots of an ARIMA model. Inverse roots outside the unit circle are shown in red.

\code{autoplot} will produce an equivelant plot as a ggplot object.}

\value{None. Function produces a plot}

\seealso{\code{\link{Arima}}, \code{\link[stats]{ar}}}

\author{Rob J Hyndman & Mitchell O'Hara-Wild}
\examples{
library(ggplot2)

fit <- Arima(WWWusage, order=c(3,1,0))
plot(fit)
autoplot(fit)

fit <- Arima(woolyrnq,order=c(2,0,0),seasonal=c(2,1,1))
plot(fit)
autoplot(fit)

plot(ar.ols(gold[1:61]))
autoplot(ar.ols(gold[1:61]))

}
\keyword{hplot}

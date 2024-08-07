% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast2.R
\name{BoxCox}
\alias{BoxCox}
\alias{InvBoxCox}
\title{Box Cox Transformation}
\usage{
BoxCox(x, lambda)

InvBoxCox(x, lambda, biasadj = FALSE, fvar = NULL)
}
\arguments{
\item{x}{a numeric vector or time series of class \code{ts}.}

\item{lambda}{transformation parameter. If \code{lambda = "auto"}, then
the transformation parameter lambda is chosen using BoxCox.lambda (with a lower bound of -0.9)}

\item{biasadj}{Use adjusted back-transformed mean for Box-Cox
transformations. If transformed data is used to produce forecasts and fitted values,
a regular back transformation will result in median forecasts. If biasadj is TRUE,
an adjustment will be made to produce mean forecasts and fitted values.}

\item{fvar}{Optional parameter required if biasadj=TRUE. Can either be the
forecast variance, or a list containing the interval \code{level}, and the
corresponding \code{upper} and \code{lower} intervals.}
}
\value{
a numeric vector of the same length as x.
}
\description{
BoxCox() returns a transformation of the input variable using a Box-Cox
transformation. InvBoxCox() reverses the transformation.
}
\details{
The Box-Cox transformation (as given by Bickel & Doksum 1981) is given by

\deqn{f_\lambda(x) =(sign(x)|x|^\lambda - 1)/\lambda}{f(x;lambda)=(sign(x)|x|^lambda - 1)/lambda}

if \eqn{\lambda\ne0}{lambda is not equal to 0}. For \eqn{\lambda=0}{lambda=0},

\deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
}
\examples{

lambda <- BoxCox.lambda(lynx)
lynx.fit <- ar(BoxCox(lynx,lambda))
plot(forecast(lynx.fit,h=20,lambda=lambda))

}
\references{
Box, G. E. P. and Cox, D. R. (1964) An analysis of
transformations. \emph{JRSS B} \bold{26} 211--246.
Bickel, P. J. and Doksum K. A. (1981) An Analysis of Transformations Revisited. \emph{JASA} \bold{76} 296-311.
}
\seealso{
\code{\link{BoxCox.lambda}}
}
\author{
Rob J Hyndman & Mitchell O'Hara-Wild
}
\keyword{ts}

\name{BoxCox}
\alias{BoxCox}
\alias{InvBoxCox}
\title{Box Cox Transformation}
\usage{BoxCox(x, lambda)
InvBoxCox(x,lambda)
}
\arguments{
\item{x}{a numeric vector or time series}
\item{lambda}{transformation parameter}
}
\description{BoxCox() returns a transformation of the input variable using a Box-Cox transformation.
InvBoxCox() reverses the transformation.
}
\details{The Box-Cox transformation is given by
\deqn{f_\lambda(x) =\frac{x^\lambda - 1}{\lambda}}{f(x;lambda)=(x^lambda - 1)/lambda}
if \eqn{\lambda\ne0}{lambda is not equal to 0}. For \eqn{\lambda=0}{lambda=0},
\deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
}
\value{a numeric vector of the same length as x.
}
\references{Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26} 211--246.
}
\seealso{\code{\link{BoxCox.lambda}}}
\author{Rob J Hyndman}
\examples{
lambda <- BoxCox.lambda(lynx)
lynx.fit <- ar(BoxCox(lynx,lambda))
plot(forecast(lynx.fit,h=20,lambda=lambda))
}
\keyword{ts}

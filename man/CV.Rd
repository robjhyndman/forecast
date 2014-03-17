\name{CV}
\alias{CV}
\title{Cross-validation statistic}
\usage{
CV(obj)
}


\arguments{
\item{obj}{output from \code{\link[stats]{lm}} or \code{\link{tslm}}}
}

\description{Computes the leave-one-out cross-validation statistic (also known as PRESS -- prediction residual sum of squares), AIC, corrected AIC, BIC and adjusted R^2 values for a linear model.}

\value{Numerical vector containing CV, AIC, AICc, BIC and AdjR2 values.}

\seealso{\code{\link[stats]{AIC}}}

\author{Rob J Hyndman}
\examples{
y <- ts(rnorm(120,0,3) + 20*sin(2*pi*(1:120)/12), frequency=12)
fit1 <- tslm(y ~ trend + season)
fit2 <- tslm(y ~ season)
CV(fit1)
CV(fit2)
}

\keyword{models}

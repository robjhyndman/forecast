\name{CVar}
\alias{CVar}
\title{k-fold Cross-Validation applied to an autoregressive model}
\usage{
CVar(y, k=10, FUN=nnetar, cvtrace=FALSE, ...)
}

\arguments{
\item{y}{Univariate time series}
\item{k}{Number of folds to use for cross-validation.}
\item{FUN}{Function to fit an autoregressive model. Currently, it only works with the \code{\link{nnetar}} function.}
\item{cvtrace}{Provide progress information.}
\item{...}{Other arguments are passed to \code{FUN}.}
}

\description{\code{CVar} computes the errors obtained by applying an autoregressive modelling function to subsets of the time series \code{y} using k-fold cross-validation as described in Bergmeir, Hyndman and Koo (2015).}

\value{A list containing information about the model and accuracy for each fold, plus other summary information computed across folds.}

\references{Bergmeir, C., Hyndman, R.J., Koo, B. (2015) A note on the validity of cross-validation for evaluating time series prediction. Monash working paper 10/15. \url{http://robjhyndman.com/working-papers/cv-time-series/}.}

\seealso{\link{CV}, \link{tsCV}.}

\author{Gabriel Caceres and Rob J Hyndman}
\examples{
modelcv <- CVar(lynx, k=5, lambda=0.15)
print(modelcv)
print(modelcv$fold1)}
\keyword{ts}
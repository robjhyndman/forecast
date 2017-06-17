# Time series cross-validation
# y is a time series
# forecastfunction must return an object of class forecast
# h is number of steps ahead to forecast
# ... are passed to forecastfunction




#' Time series cross-validation
#'
#' \code{tsCV} computes the forecast errors obtained by applying
#' \code{forecastfunction} to subsets of the time series \code{y} using a
#' rolling forecast origin.
#'
#' Let \code{y} contain the time series \eqn{y_1,\dots,y_T}{y[1:T]}. Then
#' \code{forecastfunction} is applied successively to the time series
#' \eqn{y_1,\dots,y_t}{y[1:t]}, for \eqn{t=1,\dots,T-h}, making predictions
#' \eqn{\hat{y}_{t+h|t}}{f[t+h]}. The errors are given by \eqn{e_{t+h} =
#' y_{t+h}-\hat{y}_{t+h|t}}{e[t+h] = y[t+h]-f[t+h]}. These are returned as a
#' vector, \eqn{e_1,\dots,e_T}{e[1:T]}. The first few errors may be missing as
#' it may not be possible to apply \code{forecastfunction} to very short time
#' series.
#'
#' @param y Univariate time series
#' @param forecastfunction Function to return an object of class
#' \code{forecast}. Its first argument must be a univariate time series, and it
#' must have an argument \code{h} for the forecast horizon.
#' @param h Forecast horizon
#' @param ... Other arguments are passed to \code{forecastfunction}.
#' @return Numerical time series object containing the forecast errors.
#' @author Rob J Hyndman
#' @seealso \link{CV}, \link{CVar}, \link{residuals.Arima}.
#' @keywords ts
#' @examples
#'
#' #Fit an AR(2) model to each subset
#' far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
#' e <- tsCV(lynx, far2, h=1)
#'
#' @export
tsCV <- function(y, forecastfunction, h=1, ...)
{
  y <- as.ts(y)
  n <- length(y)
  e <- y*NA
  for(i in seq_len(n-h))
  {
    fc <- try(suppressWarnings(forecastfunction(subset(y, end=i), h=h, ...)), silent=TRUE)
    if(!is.element("try-error", class(fc)))
      e[i+h] <- y[i+h] - fc$mean[h]
  }
  return(e)
}

## Cross-validation for AR models
# By Gabriel Caceres
## Note arguments to pass must be named


#' k-fold Cross-Validation applied to an autoregressive model
#'
#' \code{CVar} computes the errors obtained by applying an autoregressive
#' modelling function to subsets of the time series \code{y} using k-fold
#' cross-validation as described in Bergmeir, Hyndman and Koo (2015).
#'
#' @aliases print.CVar
#'
#' @param y Univariate time series
#' @param k Number of folds to use for cross-validation.
#' @param FUN Function to fit an autoregressive model. Currently, it only works
#' with the \code{\link{nnetar}} function.
#' @param cvtrace Provide progress information.
#' @param ... Other arguments are passed to \code{FUN}.
#' @return A list containing information about the model and accuracy for each
#' fold, plus other summary information computed across folds.
#' @author Gabriel Caceres and Rob J Hyndman
#' @seealso \link{CV}, \link{tsCV}.
#' @references Bergmeir, C., Hyndman, R.J., Koo, B. (2015) A note on the
#' validity of cross-validation for evaluating time series prediction. Monash
#' working paper 10/15.
#' \url{https://robjhyndman.com/publications/cv-time-series/}.
#' @keywords ts
#' @examples
#'
#' modelcv <- CVar(lynx, k=5, lambda=0.15)
#' print(modelcv)
#' print(modelcv$fold1)
#'
#' @export
CVar <- function(y, k=10, FUN=nnetar, cvtrace=FALSE, ...){
  nx <- length(y)
  ## n-folds at most equal number of points
  k <- min(as.integer(k), nx)
  if(k <= 1L)
    stop("k must be at least 2")
  # Set up folds
  ind <- seq_len(nx)
  fold <- sample(rep(1:k, length.out=nx))

  cvacc <- matrix(NA_real_, nrow=k, ncol=7)
  out <- list()
  for (i in 1:k)
  {
    out[[paste0("fold", i)]] <- list()
    testset <- ind[fold==i]
    trainset <- ind[fold!=i]
    trainmodel <- FUN(y, subset=trainset, ...)
    testmodel <- FUN(y, model=trainmodel, xreg=trainmodel$xreg)
    testfit <- fitted(testmodel)
    acc <- accuracy(y, testfit, test=testset)
    cvacc[i, ] <- acc
    out[[paste0("fold", i)]]$model <- trainmodel
    out[[paste0("fold", i)]]$accuracy <- acc
    if (isTRUE(cvtrace)){
      cat("Fold", i, "\n")
      print(acc)
      cat("\n")
    }
  }
  out$k <- k
  ## calculate mean acuracy accross all folds
  CVmean <- matrix(apply(cvacc, 2, FUN=mean, na.rm=TRUE), dimnames=list(colnames(acc), "Mean"))
  ## calculate accuracy sd accross all folds --- include?
  CVsd <- matrix(apply(cvacc, 2, FUN=sd, na.rm=TRUE), dimnames=list(colnames(acc), "SD"))
  out$CVsummary <- cbind(CVmean,CVsd)
  out$series <- deparse(substitute(y))
  out$call <- match.call()
  return(structure(out, class=c("CVar", class(trainmodel))))
}

#' @export
print.CVar <- function(x, ...)
{
  cat("Series:", x$series, "\n")
  cat("Call:   ")
  print(x$call)
  ## Add info about series, function, and parameters
  ## Add note about any NA/NaN in folds?
  ##
  ## Print number of folds
  cat("\n", x$k, "-fold cross-validation\n", sep="")
  ## Print mean & sd accuracy() results
  print(x$CVsummary)
  invisible(x)
}

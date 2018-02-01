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
#' y_{t+h}-\hat{y}_{t+h|t}}{e[t+h] = y[t+h]-f[t+h]}. If h=1, these are returned as a
#' vector, \eqn{e_1,\dots,e_T}{e[1:T]}. For h>1, they are returned as a matrix with
#' the hth column containing errors for forecast horizon h.
#'  The first few errors may be missing as
#' it may not be possible to apply \code{forecastfunction} to very short time
#' series.
#'
#' @param y Univariate time series
#' @param forecastfunction Function to return an object of class
#' \code{forecast}. Its first argument must be a univariate time series, and it
#' must have an argument \code{h} for the forecast horizon.
#' @param h Forecast horizon
#' @param window Length of the rolling window, if NULL, a rolling window will not be used.
#' @param ... Other arguments are passed to \code{forecastfunction}.
#' @return Numerical time series object containing the forecast errors as a vector (if h=1)
#' and a matrix otherwise.
#' @author Rob J Hyndman
#' @seealso \link{CV}, \link{CVar}, \link{residuals.Arima}, \url{https://robjhyndman.com/hyndsight/tscv/}.
#'
#' @keywords ts
#' @examples
#'
#' #Fit an AR(2) model to each rolling origin subset
#' far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
#' e <- tsCV(lynx, far2, h=1)
#'
#' #Fit the same model with a rolling window of length 30
#' e <- tsCV(lynx, far2, h=1, window=30)
#'
#' @export
tsCV <- function(y, forecastfunction, h=1, window=NULL, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- matrix(NA_real_, nrow = n, ncol = h)
  for (i in seq_len(n - h))
  {
    fc <- try(suppressWarnings(
      forecastfunction(subset(
        y,
        start = ifelse(is.null(window), 1L,
          ifelse(i - window >= 0L, i - window + 1L, stop("small window"))
        ),
        end = i
      ), h = h, ...)
    ), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  } else {
    return(e)
  }
}

## Cross-validation for AR models
# By Gabriel Caceres
## Note arguments to pass must be named


#' k-fold Cross-Validation applied to an autoregressive model
#'
#' \code{CVar} computes the errors obtained by applying an autoregressive
#' modelling function to subsets of the time series \code{y} using k-fold
#' cross-validation as described in Bergmeir, Hyndman and Koo (2015). It also
#' applies a Ljung-Box test to the residuals. If this test is significant
#' (see returned pvalue), there is serial correlation in the residuals and the
#' model can be considered to be underfitting the data. In this case, the
#' cross-validated errors can underestimate the generalization error and should
#' not be used.
#'
#' @aliases print.CVar
#'
#' @param y Univariate time series
#' @param k Number of folds to use for cross-validation.
#' @param FUN Function to fit an autoregressive model. Currently, it only works
#' with the \code{\link{nnetar}} function.
#' @param cvtrace Provide progress information.
#' @param blocked choose folds randomly or as blocks?
#' @param LBlags lags for the Ljung-Box test, defaults to 24, for yearly series can be set to 20
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
#' plot(lynx)
#' lines(modelcv$testfit, col="green")
#' lines(modelcv$residuals, col="red")
#' Acf(modelcv$residuals)
#'
#' @export
CVar <- function(y, k=10, FUN=nnetar, cvtrace=FALSE, blocked=FALSE, LBlags=24, ...) {
  nx <- length(y)
  ## n-folds at most equal number of points
  k <- min(as.integer(k), nx)
  if (k <= 1L) {
    stop("k must be at least 2")
  }
  # Set up folds
  ind <- seq_len(nx)
  fold <- if (blocked) {
    sort(rep(1:k, length.out = nx))
  } else {
    sample(rep(1:k, length.out = nx))
  }

  cvacc <- matrix(NA_real_, nrow = k, ncol = 7)
  out <- list()
  alltestfit <- rep(NA, length.out = nx)
  for (i in 1:k)
  {
    out[[paste0("fold", i)]] <- list()
    testset <- ind[fold == i]
    trainset <- ind[fold != i]
    trainmodel <- FUN(y, subset = trainset, ...)
    testmodel <- FUN(y, model = trainmodel, xreg = trainmodel$xreg)
    testfit <- fitted(testmodel)
    acc <- accuracy(y, testfit, test = testset)
    cvacc[i, ] <- acc
    out[[paste0("fold", i)]]$model <- trainmodel
    out[[paste0("fold", i)]]$accuracy <- acc

    out[[paste0("fold", i)]]$testfit <- testfit
    out[[paste0("fold", i)]]$testset <- testset

    alltestfit[testset] <- testfit[testset]

    if (isTRUE(cvtrace)) {
      cat("Fold", i, "\n")
      print(acc)
      cat("\n")
    }
  }

  out$testfit <- ts(alltestfit)
  tsp(out$testfit) <- tsp(y)

  out$residuals <- out$testfit - y
  out$LBpvalue <- Box.test(out$residuals, type = "Ljung", lag = LBlags)$p.value

  out$k <- k
  ## calculate mean accuracy accross all folds
  CVmean <- matrix(apply(cvacc, 2, FUN = mean, na.rm = TRUE), dimnames = list(colnames(acc), "Mean"))
  ## calculate accuracy sd accross all folds --- include?
  CVsd <- matrix(apply(cvacc, 2, FUN = sd, na.rm = TRUE), dimnames = list(colnames(acc), "SD"))
  out$CVsummary <- cbind(CVmean, CVsd)
  out$series <- deparse(substitute(y))
  out$call <- match.call()
  return(structure(out, class = c("CVar", class(trainmodel))))
}

#' @export
print.CVar <- function(x, ...) {
  cat("Series:", x$series, "\n")
  cat("Call:   ")
  print(x$call)
  ## Add info about series, function, and parameters
  ## Add note about any NA/NaN in folds?
  ##
  ## Print number of folds
  cat("\n", x$k, "-fold cross-validation\n", sep = "")
  ## Print mean & sd accuracy() results
  print(x$CVsummary)

  cat("\n")
  cat("p-value of Ljung-Box test of residuals is ", x$LBpvalue, "\n")
  cat("if this value is significant (<0.05),\n")
  cat("the result of the cross-validation should not be used\n")
  cat("as the model is underfitting the data.\n")
  invisible(x)
}

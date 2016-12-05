# Time series cross-validation
# y is a time series
# forecastfunction must return an object of class forecast
# h is number of steps ahead to forecast
# ... are passed to forecastfunction


tsCV <- function(y, forecastfunction, h, ...)
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
CVar <- function(y, k=10, FUN=nnetar, cvtrace=FALSE, ...){
  nx <- length(y)
  ## n-folds at most equal number of points
  k <- min(as.integer(k), nx)
  if(k <= 1L)
    stop("k must be at least 2")
  ## capture arguments to pass to FUN
  funargs <- list(...)
  funargs$x <- y
  ind <- seq_len(nx)
  fold <- sample(rep(1:k, length.out=nx))

  ## xfit <- x  # out-of-sample for full series?
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
    ##
    out[[paste0("fold", i)]]$model <- trainmodel
    out[[paste0("fold", i)]]$accuracy <- acc
    ## include full series?
    ## xfit[testset] <- testfit[testset]
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
  ## what class to set? e.g.
  return(structure(out, class=c("CVar", class(trainmodel))))
}

print.CVar <- function(x, ...){
  ## Add info about series, function, and parameters
  ## Add note about any NA/NaN in folds?
  ##
  ## Print number of folds
  cat(x$k, "-fold cross-validation\n", sep="")
  ## Print mean & sd accuracy() results
  print(x$CVsummary)
  invisible(x)
}

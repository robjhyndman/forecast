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
  k <- min(k, nx)
  ## capture arguments to pass to FUN
  funargs <- list(...)
  funargs$x <- y
  ind <- seq_len(nx)
  xshuff <- sample(nx)
  ## create folds
  fold <- suppressWarnings(split(xshuff, 1:k))
  ## xfit <- x  # out-of-sample for full series?
  cvacc <- matrix(NA_real_, nrow=k, ncol=7)
  out <- list()
  for (i in 1:k)
  {
    out[[paste0("fold", i)]] <- list()
    testset <- fold[[i]]
    trainset <- ind[-testset]
    funargs$subset <- trainset
    ## trainmodel <- do.call(FUN, funargs)
    ## testmodel <- do.call(FUN, list(x=x, model=trainmodel, xreg=trainmodel$xreg))
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
  out$CVmean <- matrix(apply(cvacc, 2, FUN=mean, na.rm=TRUE), dimnames=list(colnames(acc), "Mean"))
  ## calculate accuracy sd accross all folds --- include?
  out$CVsd <- matrix(apply(cvacc, 2, FUN=sd, na.rm=TRUE), dimnames=list(colnames(acc), "SD"))
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
  print(x$CVmean)
  print(x$CVsd)
  invisible(x)
}

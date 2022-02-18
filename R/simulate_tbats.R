#' @rdname simulate.ets
#' @export
simulate.tbats <- function(object, nsim=length(object$y),
                           seed = NULL, future=TRUE,
                           bootstrap=FALSE, innov = NULL, ...) {
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- .Random.seed
    }
    else {
      R.seed <- .Random.seed
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  }
  else {
    nsim <- length(innov)
  }
  if (bootstrap) {
    res <- residuals(object)
    res <- na.omit(res - mean(res, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  }
  else if (is.null(innov)) {
    e <- rnorm(nsim, 0, sqrt(object$variance))
  } else {
    e <- innov
  }
  x <- getResponse(object)

  y <- numeric(nsim)
  if(future) {
    dataplusy <- x
  } else {
    # Start somewhere in the original series
    dataplusy <- ts(sample(x, 1), start=-1/frequency(x),
                    frequency = frequency(x))
  }
  fitplus <- object
  for(i in seq_along(y)) {
    y[i] <- forecast(fitplus, h=1)$mean + e[i]
    dataplusy <- ts(c(dataplusy, y[i]),
                    start=start(dataplusy), frequency=frequency(dataplusy))
    fitplus <- tbats(dataplusy, model=fitplus)
  }
  return(tail(dataplusy, nsim))
}

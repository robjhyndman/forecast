# Time series cross-validation
# y is a time series
# forecastfunction must return an object of class forecast
# h is number of steps ahead to forecast
# ... are passed to forecastfunction


tscv <- function(y, forecastfunction, h, ...)
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


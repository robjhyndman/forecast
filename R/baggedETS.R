##

forecast.baggedETS <- function(x, h=ifelse(frequency(x)>1, 2*frequency(x), 10), 
    block_size = if(frequency(x)>1) 2*frequency(x) else 8,
    bootstrapped_series=bld.mbb.bootstrap(x, 100, block_size), ...) {
  
  forecasts_boot <- lapply(bootstrapped_series, function(x) { 
        mod <- ets(x, ...) 
        forecast(mod, PI=FALSE, h=h)$mean
      })
  
  forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
  colnames(forecasts_boot) <- NULL
  
  tsp.x <- tsp(x)
  if(!is.null(tsp.x))
    start.f <- tsp(x)[2] + 1/frequency(x)
  else
    start.f <- length(x)+1

  res <- list()
  res[["forecasts_boot"]] <- forecasts_boot
  
  res[["mean"]] <- ts(apply(forecasts_boot, 1, mean),frequency=frequency(x),start=start.f)
  res[["median"]] <- ts(apply(forecasts_boot, 1, median))
  res[["lower"]] <- ts(apply(forecasts_boot, 1, min))
  res[["upper"]] <- ts(apply(forecasts_boot, 1, max))

  tsp(res[["median"]]) <- tsp(res[["lower"]]) <- tsp(res[["upper"]]) <- tsp(res[["mean"]])
  
  res[["x"]] <- x
  class(res) <- "forecast"
  res
  
}


##

forecast.baggedETS <- function(x, h=ifelse(frequency(x)>1, 2*frequency(x), 10), 
    bootstrapped_series=bld.mbb.bootstrap(x, 100), ...) {
  
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


#library(forecast)
#library(fpp)
#
#res <- forecast:::forecast.baggedETS(ausbeer)
#res
#
#plot(res)
#
#print.default(res)

#
###ausbeer
##bootstrapped_series <-  forecast:::bld.mbb.bootstrap(ausbeer, 100)
##
###frequency(ausbeer)
###bootstrapped_series <-  bld.mbb.bootstrap(ausbeer, 4, 100)
###bootstrapped_series[[100]]
##
##res <- forecast:::baggedETS(ausbeer, bootstrapped_series, 12)
##res
#
#
#
#
#
#
#plot(res$mean_forecast, type="l")
#
#plot(bootstrapped_series[1,], type="l")
#apply(bootstrapped_series, 1, lines)
#
#forc <- forecast(ets(ausbeer))
#
#print.default(forc)
#
#plot()
#
#horizon = 12
#forecasts_boot <- lapply(bootstrapped_series, function(x) { 
#      mod <- ets(x) 
#      forecast(mod, PI=FALSE, h=horizon)$mean
#    })
#
#print.default(forecasts_boot[[100]])
#
#forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
#colnames(forecasts_boot) <- NULL
#
#x <- bootstrapped_series[2,]
#
#plot(x, type="l")
#
#plot(forc)
#
#plot(forecast(ets(x)))
#
#apply(bootstrapped_series, 1, ts, frequency=12)
#
#?apply
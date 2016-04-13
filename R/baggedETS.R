##

#git checkout -b baggedETS
#git push -u origin baggedETS

baggedETS <- function(bootstrapped_series, horizon, ...) {
  fits_boot <- apply(bootstrapped_series, 1, function(x) { ets(x, ...) })
  forecasts_boot <- lapply(fits_boot, function(x) { forecast(x, PI=FALSE, h=horizon)$mean })
  forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
  colnames(forecasts_boot) <- NULL
  
  res <- list()
  res[["all_forecasts"]] <- forecasts_boot
  
  res[["mean_forecast"]] <- apply(forecasts_boot, 1, mean)
  res[["median_forecast"]] <- apply(forecasts_boot, 1, median)
  res  
}


#library(forecast)
#library(fpp)
#
#ausbeer
#bootstrapped_series <-  forecast:::bootstrap_series(ausbeer, 4, 100)
#
#res <- forecast:::baggedETS(bootstrapped_series, 12)
#res
#
#plot(bootstrapped_series[1,], type="l")
#apply(bootstrapped_series, 1, lines)
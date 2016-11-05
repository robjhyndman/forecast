msts <- function(data, seasonal.periods, ts.frequency=floor(max(seasonal.periods)), ...)
{
	#if(!is.element(ts.frequency, round(seasonal.periods-0.5+1e-12)))
  #  stop("ts.frequency should be one of the seasonal periods")

	if(is.element("ts",class(data)) & frequency(data) == ts.frequency)
		object <- data
  else
	  object <- ts(data=data, frequency=ts.frequency, ...)
	if(length(seasonal.periods) > 1L)
	{
		class(object) <- c("msts", "ts")
		attr(object, "msts") <- seasonal.periods
	}
	return(object)
}

print.msts <- function(x, ...) {
	cat("Multi-Seasonal Time Series:\n")
	cat("Start: ")
	cat(start(x))
	#cat("\nEnd: ")
	#cat(x$end)
	cat("\nSeasonal Periods: ")
	cat(attr(x,"msts"))
	cat("\nData:\n")
  xx <- unclass(x) # handles both univariate and multivariate ts
  attr(xx, "tsp") <- attr(xx, "msts") <- NULL
	print(xx)
	#print(matrix(x, ncol=length(x)), nrow=1)
	cat("\n")
}

window.msts <- function(x, ...) {
	seasonal.periods <- attr(x,"msts")
	class(x) <- c("ts")
	x <- window(x, ...)
	class(x) <- c("msts", "ts")
	attr(x, "msts") <- seasonal.periods
	return(x)
}


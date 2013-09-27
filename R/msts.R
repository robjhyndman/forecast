# TODO: Add comment

msts <- function(data, seasonal.periods, ts.frequency=floor(max(seasonal.periods)), ...) {
	tsp.data <- tsp(data)	
	object <- ts(data=data, frequency=ts.frequency, ...)
	if(!is.null(tsp.data)) {
		tsp(object) <- tsp.data	
	}
	class(object) <- c("msts", "ts")
	attr(object, "msts") <- seasonal.periods
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
	print(as.numeric(x))
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


# forecast function for varest, just a wrapper for predict.varest
#' @export
forecast.varest <- function(object, h=10, level=c(80,95), fan=FALSE, ...)
{
	out <- list(model=object,forecast=vector("list", object$K))
	# Get residuals and fitted values and fix the times
	
	tspx <- tsp(object$y)
	vres <- residuals(object)
	vfits <- fitted(object)
	method <- paste0("VAR(",object$p,")")
	# Add forecasts with prediction intervals
	#out$mean <- out$lower <- out$upper <- vector("list",object$K)
	for(i in seq_along(level))
	{
		pr <- predict(object, n.ahead=h, ci=level[i]/100, ...)
		for(j in 1:object$K)
		{
			out$forecast[[j]]$lower <- cbind(out$forecast[[j]]$lower, pr$fcst[[j]][,"lower"])
			out$forecast[[j]]$upper <- cbind(out$forecast[[j]]$upper, pr$fcst[[j]][,"upper"])
		}
	}
	j <- 1
	for(fcast in out$forecast){
	  fcast$mean <- ts(pr$fcst[[j]][,"fcst"], frequency=tspx[3], start=tspx[2]+1/tspx[3])
	  fcast$lower <- ts(fcast$lower, frequency=tspx[3], start=tspx[2]+1/tspx[3])
	  fcast$upper <- ts(fcast$upper, frequency=tspx[3], start=tspx[2]+1/tspx[3])
	  colnames(fcast$lower) <- colnames(fcast$upper) <- paste0(level, "%")
	  fcast$residuals <- fcast$fitted <- ts(rep(NA, nrow(object$y)))
	  fcast$residuals[((1-nrow(vres)):0) + length(fcast$residuals)] <- vres[,j]
	  fcast$fitted[((1-nrow(vfits)):0) + length(fcast$fitted)] <- vfits[,j]
	  fcast$method <- method
	  fcast$level <- level
	  fcast$x <- object$y[,j]
	  fcast$series <- colnames(object$y)[j]
	  tsp(fcast$residuals) <- tsp(fcast$fitted) <- tspx
	  fcast <- structure(fcast, class="forecast")
	  out$forecast[[j]] <- fcast
	  j <- j + 1
	}
	names(out$forecast) <- names(pr$fcst)
	out$method <- rep(method, object$K)
	names(out$forecast) <- names(out$method) <- names(pr$fcst)
	return(structure(out,class="mforecast"))
}


# forecast function for varest, just a wrapper for predict.varest
forecast.varest <- function(object, h=10, level=c(80,95), fan=FALSE, ...)
{
	out <- list(model=object,level=level,x=object$y)
	# Get residuals and fitted values and fix the times
	out$res <- out$fitted <- ts(matrix(NA,nrow=nrow(out$x),ncol=ncol(out$x)))
	tsp(out$res) <- tsp(out$fitted) <- tsp(out$x)
	vres <- residuals(object)
	vfits <- fitted(object)
	out$res[ (nrow(out$res)-nrow(vres)+1):nrow(out$res), ] <- vres
	out$fitted[ (nrow(out$fitted)-nrow(vfits)+1):nrow(out$fitted), ] <- vfits
	# Add forecasts with prediction intervals
	out$mean <- out$lower <- out$upper <- vector("list",object$K)
	for(i in 1:(length(level)))
	{
		pr <- predict(object, n.ahead=h, ci=level[i]/100)
		for(j in 1:object$K)
		{
			if(i==1)
				out$mean[[j]] <- pr$fcst[[j]][,"fcst"]
			out$lower[[j]] <- cbind(out$lower[[j]],pr$fcst[[j]][,"lower"])
			out$upper[[j]] <- cbind(out$upper[[j]],pr$fcst[[j]][,"upper"])
		}
	}
	names(out$mean) <- names(out$lower) <- names(out$upper) <- names(pr$fcst)
	tspx <- tsp(object$y)
	for(j in 1:object$K)
		out$mean[[j]] <- ts(out$mean[[j]], frequency=tspx[3], start=tspx[2]+1/tspx[3])
	out$method <- paste("VAR(",object$p,")",sep="")
	return(structure(out,class="mforecast"))
}

print.mforecast <- function(x, ...)
{
	for(i in 1:length(x$mean))
	{
		cat(names(x$mean)[i],"\n")
		fcst <- x
		fcst$mean <- x$mean[[i]]
		fcst$lower <- x$lower[[i]]
		fcst$upper <- x$upper[[i]]
		class(fcst) <- "forecast"
		print(fcst)
		if(i < length(x$mean))
			cat("\n")
	}
}

plot.mforecast <- function(x, main=paste("Forecasts from",x$method),xlab="time",...)
{
	K <- length(x$mean)
	oldpar <- par(mfrow=c(K,1),mar=c(0,5.1,0,2.1),oma=c(6,0,5,0))
	on.exit(par(oldpar))
	for(i in 1:K)
	{
		fcst <- x
		fcst$mean <- x$mean[[i]]
		fcst$lower <- x$lower[[i]]
		fcst$upper <- x$upper[[i]]
		fcst$x <- x$x[,i]
		class(fcst) <- "forecast"
		plot(fcst,main="",xaxt="n",ylab=names(x$mean)[i],...)
	}
	axis(1)
	mtext(xlab,outer=TRUE,side=1,line=3)
	title(main=main,outer=TRUE)
}
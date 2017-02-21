forecast.tbats <- function(object, h, level=c(80,95), fan=FALSE, biasadj=NULL, ...)
{
  # Check if forecast.tbats called incorrectly
  if(identical(class(object),"bats"))
    return(forecast.bats(object, h, level, fan, biasadj, ...))

  #Set up the variables
  if(any(class(object$y) == "ts"))
		ts.frequency <- frequency(object$y)
	else
		ts.frequency <- ifelse(!is.null(object$seasonal.periods), max(object$seasonal.periods), 1)

  if(missing(h))
  {
  	if(is.null(object$seasonal.periods))
      h <- ifelse(ts.frequency == 1, 10, 2*ts.frequency)
    else
      h <- 2 * max(object$seasonal.periods)
  }
	else if(h <= 0)
		stop("Forecast horizon out of bounds")

	if(fan)
		level <- seq(51,99,by=3)
	else
	{
		if(min(level) > 0 & max(level) < 1)
			level <- 100*level
		else if(min(level) < 0 | max(level) > 99.99)
			stop("Confidence limit out of range")
	}

	if(!is.null(object$k.vector)) {
		tau <- 2*sum(object$k.vector)
	} else {
		tau <- 0
	}
	x <- matrix(0,nrow=nrow(object$x), ncol=h)
	y.forecast <- numeric(h)
	if(!is.null(object$beta)) {
		adj.beta <- 1
	} else {
		adj.beta <- 0
	}

	#Set up the matrices
	w <- .Call("makeTBATSWMatrix", smallPhi_s = object$damping.parameter, kVector_s=as.integer(object$k.vector), arCoefs_s = object$ar.coefficients, maCoefs_s = object$ma.coefficients, tau_s=as.integer(tau), PACKAGE = "forecast")

	if(!is.null(object$seasonal.periods)) {
		gamma.bold <- matrix(0,nrow=1,ncol=tau)
		.Call("updateTBATSGammaBold", gammaBold_s=gamma.bold, kVector_s=as.integer(object$k.vector), gammaOne_s=object$gamma.one.v, gammaTwo_s=object$gamma.two.v, PACKAGE = "forecast")
	} else {
		gamma.bold <- NULL
	}
	g <- matrix(0, nrow=(tau+1+adj.beta+object$p+object$q), ncol=1)
	if(object$p != 0) {
		g[(1+adj.beta+tau+1),1] <- 1
	}
	if(object$q != 0) {
		g[(1+adj.beta+tau+object$p+1),1] <- 1
	}
	.Call("updateTBATSGMatrix", g_s=g, gammaBold_s=gamma.bold, alpha_s=object$alpha, beta_s=object$beta.v, PACKAGE = "forecast")

	#print(g)

	F <- makeTBATSFMatrix(alpha=object$alpha, beta=object$beta, small.phi=object$damping.parameter, seasonal.periods=object$seasonal.periods, k.vector=as.integer(object$k.vector), gamma.bold.matrix=gamma.bold, ar.coefs=object$ar.coefficients, ma.coefs=object$ma.coefficients)

	#Do the forecast
	y.forecast[1] <- w$w.transpose %*% object$x[,ncol(object$x)]
	x[,1] <-  F %*% object$x[,ncol(object$x)] #+ g %*% object$errors[length(object$errors)]

	if(h > 1) {
		for(t in 2:h) {
			x[,t] <- F %*% x[,(t-1)]
			y.forecast[t] <- w$w.transpose %*% x[,(t-1)]
		}
	}
	##Make prediction intervals here
	lower.bounds  <-  upper.bounds  <-  matrix(NA,ncol=length(level),nrow=h)
	variance.multiplier <- numeric(h)
	variance.multiplier[1] <- 1
	if(h > 1) {
		for(j in 1:(h-1)) {
			if(j == 1) {
				f.running <- diag(ncol(F))
			} else {
				f.running <- f.running %*% F
			}
			c.j <- w$w.transpose %*% f.running %*% g
			variance.multiplier[(j+1)] <- variance.multiplier[j]+ c.j^2
		}
	}

	variance <- object$variance * variance.multiplier
	#print(variance)
	st.dev <- sqrt(variance)
	for(i in 1:length(level)) {
		marg.error  <-  st.dev * abs(qnorm((100-level[i])/200))
		lower.bounds[,i]  <-  y.forecast - marg.error
		upper.bounds[,i]  <-  y.forecast + marg.error

	}
	#Inv Box Cox transform if required
	if(!is.null(object$lambda))
	{
	  y.forecast <- InvBoxCox(y.forecast, object$lambda, biasadj, list(level = level, upper = upper.bounds, lower = lower.bounds))
		lower.bounds  <-  InvBoxCox(lower.bounds,object$lambda)
		if(object$lambda < 1) {
			lower.bounds<-pmax(lower.bounds, 0)
		}
		upper.bounds  <-  InvBoxCox(upper.bounds,object$lambda)
	}
	##Calc a start time for the forecast
	#y <- object$y
	start.time <- start(object$y)
	y <- ts(c(object$y,0), start=start.time, frequency=ts.frequency)
	#y[(length(y)+1)] <- 0
	#y <- ts(y, start=object$start.time, frequency=ts.frequency)
	fcast.start.time <- end(y)
	#Make msts object for x and mean
	x <- msts(object$y, seasonal.periods=(if(!is.null(object$seasonal.periods)) { object$seasonal.periods} else { ts.frequency}), ts.frequency=ts.frequency, start=start.time)
	fitted.values <- msts(object$fitted.values, seasonal.periods=(if(!is.null(object$seasonal.periods)) { object$seasonal.periods} else { ts.frequency}), ts.frequency=ts.frequency, start=start.time)
	y.forecast <- msts(y.forecast, seasonal.periods=(if(!is.null(object$seasonal.periods)) { object$seasonal.periods} else { ts.frequency}), ts.frequency=ts.frequency, start=fcast.start.time)
  upper.bounds <- msts(upper.bounds, seasonal.periods=(if(!is.null(object$seasonal.periods)) { object$seasonal.periods} else { ts.frequency}), ts.frequency=ts.frequency, start=fcast.start.time)
  lower.bounds <- msts(lower.bounds, seasonal.periods=(if(!is.null(object$seasonal.periods)) { object$seasonal.periods} else { ts.frequency}), ts.frequency=ts.frequency, start=fcast.start.time)
  colnames(upper.bounds) <- colnames(lower.bounds) <- paste0(level, "%")
  
  forecast.object <- list(model=object, mean=y.forecast, level=level, x=x, series=object$series,
	                        upper=upper.bounds, lower=lower.bounds, fitted=fitted.values,
	                        method=makeTextTBATS(object), residuals=object$errors)
	if(is.null(object$series)){
	  forecast.object$series <- deparse(object$call$y)
	}
	
	class(forecast.object) <- "forecast"
	return(forecast.object)
}


makeTextTBATS <- function(object) {
	name <- "TBATS("
	if(!is.null(object$lambda)) {
		name <- paste(name, round(object$lambda, digits=3), sep="")
	} else {
		name <- paste(name, "1", sep="")
	}
	name <- paste(name, ", {", sep="")
	if(!is.null(object$ar.coefficients)) {
		name <- paste(name, length(object$ar.coefficients), sep="")
	} else {
		name <- paste(name, "0", sep="")
	}
	name <- paste(name, ",", sep="")
	if(!is.null(object$ma.coefficients)) {
		name <- paste(name, length(object$ma.coefficients), sep="")
	} else {
		name <- paste(name, "0", sep="")
	}
	name <- paste(name, "}, ", sep="")
	if(!is.null(object$damping.parameter)) {
		name <- paste(name, round(object$damping.parameter, digits=3), ",",sep="")
	} else {
		name <- paste(name, "-,", sep="")
	}

	if(!is.null(object$seasonal.periods)) {
		name <- paste(name, " {", sep="")
    M <- length(object$seasonal.periods)
		for(i in 1:M) {
			name <- paste(name, "<", object$seasonal.periods[i], ",", object$k.vector[i], ">", sep="")
			if(i < M) {
				name <- paste(name, ", ", sep="")
			} else {
				name <- paste(name, "})", sep="")
			}
		}
	} else {
		name <- paste(name, "{-})", sep="")
	}
	return(name)
}


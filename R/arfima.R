# Remove missing values from end points
na.ends <- function(x)
{
  tspx <- tsp(x)
  # Strip initial and final missing values
  nonmiss <- (1:length(x))[!is.na(x)]
	if(length(nonmiss)==0)
		stop("No non-missing data")
	j <- nonmiss[1]
	k <- nonmiss[length(nonmiss)]
	x <- x[j:k]
	if(!is.null(tspx))
		x <- ts(x,start=tspx[1]+(j-1)/tspx[3],frequency=tspx[3])
	return(x)
}

# Add back missing values at ends
# x is original series. y is the series with NAs removed at ends.
# returns y with the nas put back at beginning but not end.
undo.na.ends <- function(x,y)
{
	n <- length(x)
	nonmiss <- (1:length(x))[!is.na(x)]
	j <- nonmiss[1]
	k <- nonmiss[length(nonmiss)]
	if(j>1)
		y <- c(rep(NA,j-1),y)
	if(k<n)
		y <- c(y,rep(NA,n-k))
	tspx <- tsp(x)
	if(!is.null(tspx))
		tsp(y) <- tsp(x)
	return(y)
}

## Undifference
unfracdiff <- function(x,y,n,h,d)
{
	bin.c <- (-1)^(0:(n+h)) * choose(d, (0:(n+h)))
	b <- numeric(n)
	xnew <- LHS <- numeric(h)
	RHS <- cumsum(y)
	bs <- cumsum(bin.c[1:h])
	b <- bin.c[(1:n) + 1]
	xnew[1] <- RHS[1] <- y[1] - sum(b * rev(x))
	if(h>1)
	{
		for (k in 2:h) 
		{
			b <- b + bin.c[(1:n) + k]
			RHS[k] <- RHS[k] - sum(b * rev(x))
			LHS[k] <- sum(rev(xnew[1:(k-1)]) * bs[2:k])
			xnew[k] <- RHS[k] - LHS[k]
		}
	}
	tspx <- tsp(x)
	if(is.null(tspx))
		tspx <- c(1,length(x),1)
	return(ts(xnew,frequency=tspx[3],start=tspx[2]+1/tspx[3]))    
}

## Automatic ARFIMA modelling
## Will return Arima object if d < 0.01 to prevent estimation problems
arfima <- function(x, drange = c(0, 0.5), estim = c("mle","ls"), lambda = NULL, biasadj = FALSE, ...)
{
	estim <- match.arg(estim)
#	require(fracdiff)
    
	orig.x <- x
	if (!is.null(lambda)){
		x <- BoxCox(x, lambda)
	}
	
	# Strip initial and final missing values
	xx <- na.ends(x)
	
	# Remove mean
	meanx <- mean(xx)
	xx <- xx - meanx
	
	# Choose differencing parameter with AR(2) proxy to handle correlations
	suppressWarnings(fit <- fracdiff::fracdiff(xx,nar=2,drange=drange))
 
	# Choose p and q
	d <- fit$d
	y <- fracdiff::diffseries(xx, d=d)
	fit <- auto.arima(y, max.P=0, max.Q=0, stationary=TRUE, ...)
	
	# Refit model using fracdiff
	suppressWarnings(fit <- fracdiff::fracdiff(xx, nar=fit$arma[1], nma=fit$arma[2],drange=drange))
	
	# Refine parameters with MLE
	if(estim=="mle")
	{
		y <- fracdiff::diffseries(xx, d=fit$d)
		p <- length(fit$ar)
		q <- length(fit$ma)
		fit2 <- try(Arima(y,order=c(p,0,q),include.mean=FALSE))
		if(is.element("try-error",class(fit2)))
      fit2 <- try(Arima(y,order=c(p,0,q),include.mean=FALSE,method="ML"))
		if(!is.element("try-error",class(fit2)))
		{
			if(p>0)
				fit$ar <- fit2$coef[1:p]
			if(q>0)
				fit$ma <- -fit2$coef[p+(1:q)]
			fit$residuals <- fit2$residuals
		}
		else
			warning("MLE estimation failed. Returning LS estimates")
	}
	
	# Add things to model that will be needed by forecast.fracdiff
	fit$x <- orig.x
	fit$residuals <- undo.na.ends(x,residuals(fit))
	fit$fitted <- x - fit$residuals
	if(!is.null(lambda))
	  fit$fitted <- InvBoxCox(fit$fitted,lambda)
	  if(biasadj){
	    fit$fitted <- InvBoxCoxf(fit$fitted, fvar = var(fit$residuals), lambda=lambda)
	  }
	fit$lambda <- lambda
	fit$call$data <- data.frame(x=x)
	return(fit)
}

# Forecast the output of fracdiff() or arfima()

forecast.fracdiff <- function(object, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=FALSE, ...) 
{
	# Extract data
	x <- object$x <- getResponse(object)
	
	if(!is.null(lambda)){
		x <- BoxCox(x,lambda)
	}
	
	xx <- na.ends(x)
	n <- length(xx)
	
	meanx <- mean(xx)
	xx <- xx - meanx
	
	# Construct ARMA part of model and forecast with it
	y <- fracdiff::diffseries(xx, d=object$d)
	fit <- Arima(y, order=c(length(object$ar),0,length(object$ma)), include.mean=FALSE, fixed=c(object$ar,-object$ma))
	fcast.y <- forecast(fit, h=h, level=level)

	# Undifference
	fcast.x <- unfracdiff(xx,fcast.y$mean,n,h,object$d)
	
	# Binomial coefficient for expansion of d
	bin.c <- (-1)^(0:(n+h)) * choose(object$d,(0:(n+h)))

	#Cumulative forecasts of y and forecast of y
	# b <- numeric(n)
	# fcast.x <- LHS <- numeric(h)
	# RHS <- cumsum(fcast.y$mean)
	# bs <- cumsum(bin.c[1:h])
	# b <- bin.c[(1:n)+1]
	# fcast.x[1] <- RHS[1] <- fcast.y$mean[1] - sum(b*rev(xx))
	# if(h>1)
	# {
			# for (k in 2:h)
			# {
					# b <- b + bin.c[(1:n)+k]
					# RHS[k] <- RHS[k] - sum(b*rev(xx))
					# LHS[k] <- sum(rev(fcast.x[1:(k-1)]) * bs[2:k])
					# fcast.x[k] <- RHS[k] - LHS[k]
			# }
	# }
	
	# Extract stuff from ARMA model
	p <- fit$arma[1]
	q <- fit$arma[2]
	phi <- theta <- numeric(h)
	if(p > 0)
			phi[1:p] <- fit$coef[1:p]
	if(q > 0)
			theta[1:q] <- fit$coef[p+(1:q)]

	# Calculate psi weights
	new.phi <- psi <- numeric(h)
	psi[1] <- new.phi[1] <- 1
	if(h>1)
	{
		new.phi[2:h] <- -bin.c[2:h]
		for (i in 2:h) 
		{
			if(p>0)
				new.phi[i] <- sum(phi[1:(i-1)] * bin.c[(i-1):1]) - bin.c[i]
			psi[i] <- sum(new.phi[2:i] * rev(psi[1:(i-1)])) + theta[i-1]
		}
	}
	
	# Compute forecast variances
	fse <- sqrt(cumsum(psi^2) * fit$sigma2)
	
	# Compute prediction intervals
	if (fan) 
			level <- seq(51, 99, by = 3)
	else 
	{
		if (min(level) > 0 & max(level) < 1) 
			level <- 100 * level
		else if (min(level) < 0 | max(level) > 99.99) 
			stop("Confidence limit out of range")
	}
	nint <- length(level)
	upper <- lower <- matrix(NA, ncol = nint, nrow=h)
	for (i in 1:nint) 
	{
		qq <- qnorm(0.5 * (1 + level[i]/100))
		lower[, i] <- fcast.x - qq * fse
		upper[, i] <- fcast.x + qq * fse
	}
	colnames(lower) = colnames(upper) = paste(level, "%", sep = "")

	res <- undo.na.ends(x,residuals(fit))
	fits <- x-res
	data.tsp <- tsp(x)
	if(is.null(data.tsp))
		data.tsp <- c(1,length(x),1)
	mean.fcast <- ts(fcast.x+meanx, frequency=data.tsp[3], start=data.tsp[2] + 1/data.tsp[3])
	lower <- ts(lower+meanx, frequency=data.tsp[3], start=data.tsp[2] + 1/data.tsp[3])
	upper <- ts(upper+meanx, frequency=data.tsp[3], start=data.tsp[2] + 1/data.tsp[3])
	method <- paste("ARFIMA(",p,",",round(object$d,2),",",q,")",sep="")
	
	if(!is.null(lambda))
	{
		x <- InvBoxCox(x,lambda)
		fits <- InvBoxCox(fits,lambda)
		mean.fcast <- InvBoxCox(mean.fcast,lambda)
		if(biasadj){
		  mean.fcast <- InvBoxCoxf(list(level = level, mean = mean.fcast, upper=upper, lower=lower),lambda=lambda)
		}
		lower <- InvBoxCox(lower,lambda)
		upper <- InvBoxCox(upper,lambda)
	}

    return(structure(list(x=x, mean=mean.fcast, upper=upper, lower=lower, 
        level=level, method=method, xname=deparse(substitute(x)), model=object, 
        residuals=res, fitted=fits), class="forecast"))
}

# Residuals from arfima() or fracdiff()

residuals.fracdiff <- function(object, ...)
{
	#require(fracdiff)

	if(!is.null(object$residuals))   # Object produced by arfima()
		return(object$residuals)
	else                             # Object produced by fracdiff()
	{
		if (is.element("x", names(object))) 
			x <- object$x
		else 
			x <- eval.parent(parse(text=as.character(object$call)[2]))
		if(!is.null(object$lambda))
			x <- BoxCox(x,object$lambda)
		y <- fracdiff::diffseries(x - mean(x), d=object$d)
		fit <- arima(y, order=c(length(object$ar),0,length(object$ma)), include.mean=FALSE, fixed=c(object$ar,object$ma))
		return(residuals(fit))
	}
}

# Fitted values from arfima() or fracdiff()

fitted.fracdiff <- function(object, ...)
{
	if(!is.null(object$fitted))      # Object produced by arfima()
		return(object$fitted)
	else
	{
		x <- getResponse(object)
		return(x-residuals(object))
	}
}

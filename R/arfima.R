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


#' Fit a fractionally differenced ARFIMA model
#' 
#' An ARFIMA(p,d,q) model is selected and estimated automatically using the
#' Hyndman-Khandakar (2008) algorithm to select p and q and the Haslett and
#' Raftery (1989) algorithm to estimate the parameters including d.
#' 
#' This function combines \code{\link[fracdiff]{fracdiff}} and
#' \code{\link{auto.arima}} to automatically select and estimate an ARFIMA
#' model.  The fractional differencing parameter is chosen first assuming an
#' ARFIMA(2,d,0) model. Then the data are fractionally differenced using the
#' estimated d and an ARMA model is selected for the resulting time series
#' using \code{\link{auto.arima}}. Finally, the full ARFIMA(p,d,q) model is
#' re-estimated using \code{\link[fracdiff]{fracdiff}}. If \code{estim=="mle"},
#' the ARMA coefficients are refined using \code{\link[stats]{arima}}.
#' 
#' @param y a univariate time series (numeric vector).
#' @param drange Allowable values of d to be considered. Default of
#' \code{c(0,0.5)} ensures a stationary model is returned.
#' @param estim If \code{estim=="ls"}, then the ARMA parameters are calculated
#' using the Haslett-Raftery algorithm. If \code{estim=="mle"}, then the ARMA
#' parameters are calculated using full MLE via the \code{\link[stats]{arima}}
#' function.
#' @param model Output from a previous call to \code{arfima}. If model is
#' passed, this same model is fitted to y without re-estimating any parameters.
#' @param lambda Box-Cox transformation parameter. Ignored if \code{NULL}.
#' Otherwise, data transformed before model is estimated.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If \code{TRUE}, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param x Deprecated. Included for backwards compatibility.
#' @param \dots Other arguments passed to \code{\link{auto.arima}} when
#' selecting p and q.
#' @return A list object of S3 class \code{"fracdiff"}, which is described in
#' the \code{\link[fracdiff]{fracdiff}} documentation. A few additional objects
#' are added to the list including \code{x} (the original time series), and the
#' \code{residuals} and \code{fitted} values.
#' 
#' @export
#' 
#' @author Rob J Hyndman and Farah Yasmeen
#' @seealso \code{\link[fracdiff]{fracdiff}}, \code{\link{auto.arima}},
#' \code{\link{forecast.fracdiff}}.
#' @references J. Haslett and A. E. Raftery (1989) Space-time Modelling with
#' Long-memory Dependence: Assessing Ireland's Wind Power Resource (with
#' discussion); \emph{Applied Statistics} \bold{38}, 1-50.
#' 
#' Hyndman, R.J. and Khandakar, Y. (2008) "Automatic time series forecasting:
#' The forecast package for R", \emph{Journal of Statistical Software},
#' \bold{26}(3).
#' @keywords ts
#' @examples
#' 
#' library(fracdiff)
#' x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
#' fit <- arfima(x)
#' tsdisplay(residuals(fit))
#' 
arfima <- function(y, drange = c(0, 0.5), estim = c("mle","ls"), model = NULL, lambda = NULL, biasadj = FALSE, x=y, ...)
{
	estim <- match.arg(estim)
#	require(fracdiff)
	seriesname <- deparse(substitute(y))
	
	orig.x <- x
	if (!is.null(lambda)){
		x <- BoxCox(x, lambda)
	}
	
	# Re-fit arfima model
	if(!is.null(model)){
	  fit <- model
	  fit$residuals <- fit$fitted <- fit$lambda <- NULL
	  if(!is.null(lambda)){
	    fit$lambda <- lambda # Required for residuals.fracdiff()
	  }
	}
	# Estimate model
	else{
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
	}

	# Add things to model that will be needed by forecast.fracdiff
	fit$x <- orig.x
	fit$residuals <- undo.na.ends(x,residuals(fit))
	fit$fitted <- x - fit$residuals
	if(!is.null(lambda)){
	  fit$fitted <- InvBoxCox(fit$fitted, lambda, biasadj, var(fit$residuals))
	  attr(lambda, "biasadj") <- biasadj
	}
	fit$lambda <- lambda
	fit$call <- match.call()
	fit$series <- seriesname
	#fit$call$data <- data.frame(x=x) #Consider replacing fit$call with match.call for consistency and tidyness
	return(fit)
}

# Forecast the output of fracdiff() or arfima()

#' @rdname forecast.Arima
#' @export
forecast.fracdiff <- function(object, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=NULL, ...) 
{
	# Extract data
	x <- object$x <- getResponse(object)
	if(is.null(x))
	  stop("Unable to find original time series")

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
		mean.fcast <- InvBoxCox(mean.fcast, lambda, biasadj, list(level = level, upper=upper, lower=lower))
		lower <- InvBoxCox(lower,lambda)
		upper <- InvBoxCox(upper,lambda)
	}
	
	seriesname <- if(!is.null(object$series)){
	  object$series
	}
	else {
	  deparse(object$call$x)
	}

  return(structure(list(x=x, mean=mean.fcast, upper=upper, lower=lower,
      level=level, method=method, model=object, series=seriesname,
      residuals=res, fitted=fits), class="forecast"))
}

# Fitted values from arfima() or fracdiff()

#' @rdname fitted.Arima
#' @export
fitted.fracdiff <- function(object, h = 1, ...)
{
	if(!is.null(object$fitted)){      # Object produced by arfima()
	  if(h==1){
	    return(object$fitted)
	  }
    else{
      return(hfitted(object=object, h=h, FUN="arfima", ...))
    }
	}
	else {
	  if(h!=1){
	    warning("h-step fits are not supported for models produced by fracdiff(), returning one-step fits (h=1)")
	  }
		x <- getResponse(object)
		return(x-residuals(object))
	}
}

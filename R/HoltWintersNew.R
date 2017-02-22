# Modelled on the HoltWinters() function but with more conventional
# initialization.
# Written by Zhenyu Zhou. 21 October 2012

HoltWintersZZ  <- function (x,
				# smoothing parameters
				alpha    = NULL, # level
				beta     = NULL, # trend
				gamma    = NULL, # seasonal component
				seasonal = c("additive", "multiplicative"),
				exponential = FALSE, # exponential
				phi = NULL, # damp
				lambda = NULL, # box-cox
				biasadj = FALSE # adjusted back-transformed mean for box-cox
)
{
	x <- as.ts(x)
	seasonal <- match.arg(seasonal)
	m <- frequency(x)
	lenx <- length(x)

	if(!is.null(lambda)){
	  x <- BoxCox(x,lambda)
	}

	if(is.null(phi) || !is.numeric(phi))
		phi <- 1
	if(!is.null(alpha) && !is.numeric(alpha))
		stop ("cannot fit models without level ('alpha' must not be 0 or FALSE).")
	if(!all(is.null(c(alpha, beta, gamma))) &&
			any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
		stop ("'alpha', 'beta' and 'gamma' must be within the unit interval.")
	if((is.null(gamma) || gamma > 0)) {
		if (seasonal == "multiplicative" && any(x <= 0))
			stop ("data must be positive for multiplicative Holt-Winters.")
	}

	if(m<=1)
		gamma <- FALSE

	## initialise l0, b0, s0
	if(!is.null(gamma) && is.logical(gamma) && !gamma) {
		seasonal <- "none"
		l.start <- x[1L]
		s.start <- 0
		if(is.null(beta) || !is.logical(beta) || beta){
			if(!exponential)
				b.start <- x[2L] - x[1L]
			else
				b.start <- x[2L]/x[1L]
		}
	} else {
		## seasonal Holt-Winters
		l.start <- mean(x[1:m])
		b.start <- (mean(x[m+(1:m)]) - l.start)/m
		if(seasonal=="additive")
			s.start <- x[1:m]-l.start
		else
			s.start <- x[1:m]/l.start
	}

	#initialise smoothing parameters
	#lower=c(rep(0.0001,3), 0.8)
	#upper=c(rep(0.9999,3),0.98)
	lower <- c(0,0,0,0)
	upper <- c(1,1,1,1)

	if(!is.null(beta) && is.logical(beta) && !beta)
		trendtype <- "N"
	else if(exponential)
		trendtype <- "M"
	else
		trendtype <- "A"

	if(seasonal=="none")
		seasontype <- "N"
	else if(seasonal=="multiplicative")
		seasontype <- "M"
	else
		seasontype <- "A"

	## initialise smoothing parameter
	optim.start <- initparam(alpha = alpha, beta = beta, gamma=gamma, phi=1,
			trendtype=trendtype, seasontype=seasontype, damped=FALSE, lower=lower, upper=upper, m=m)

	# if(!is.na(optim.start["alpha"]))
	# 	alpha2 <- optim.start["alpha"]
	# else
	# 	alpha2 <- alpha
	# if(!is.na(optim.start["beta"]))
	# 	beta2 <- optim.start["beta"]
	# else
	# 	beta2 <- beta
	# if(!is.na(optim.start["gamma"]))
	# 	gamma2 <- optim.start["gamma"]
	# else
	# 	gamma2 <- gamma

#	if(!check.param(alpha = alpha2,beta = beta2, gamma = gamma2,phi=1,lower,upper,bounds="haha",m=m))
#	{
#		print(paste("alpha=", alpha2, "beta=",beta2, "gamma=",gamma2))
#		stop("Parameters out of range")
#	}

	###################################################################################
	#optimisation: alpha, beta, gamma, if any of them is null, then optimise them
	error <- function (p, select)
	{
		if(select[1]>0)
			alpha <- p[1L]
		if(select[2]>0)
			beta <- p[1L+select[1]]
		if(select[3]>0)
			gamma <- p[1L+select[1]+select[2]]

		zzhw(x,lenx=lenx, alpha = alpha, beta=beta, gamma=gamma, seasonal=seasonal, m=m,
			dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma),
			exponential=exponential, phi=phi, l.start=l.start, b.start=b.start, s.start=s.start)$SSE
	}
	select <- as.numeric(c(is.null(alpha),is.null(beta),is.null(gamma)))

	if(sum(select)>0) # There are parameters to optimize
	{
		sol <- optim(optim.start, error, method = "L-BFGS-B", lower = lower[select], upper = upper[select], select=select)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s", sol$message), domain = NA)
			} else stop("optimization failure")
		}
		if(select[1]>0)
			alpha <- sol$p[1L]
		if(select[2]>0)
			beta <- sol$p[1L+select[1]]
		if(select[3]>0)
			gamma <- sol$p[1L+select[1]+select[2]]
	}

	final.fit <- zzhw(x, lenx=lenx, alpha = alpha, beta=beta, gamma=gamma, seasonal=seasonal, m=m,
			dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma),
			exponential=exponential, phi=phi, l.start=l.start, b.start=b.start, s.start=s.start)

	tspx <- tsp(x)
	fitted <- ts(final.fit$fitted,frequency=m,start=tspx[1])
	if(!is.null(lambda))
	{
	  fitted <- InvBoxCox(fitted, lambda, biasadj, var(final.fit$residuals))
	  attr(lambda, "biasadj") <- biasadj
	}
	states <- matrix(final.fit$level,ncol=1)
	colnames(states) <- "l"
	if(trendtype!="N")
		states <- cbind(states,b=final.fit$trend)
	if(seasontype!="N")
  {
    nr <- nrow(states)
    nc <- ncol(states)
    for(i in 1:m)
  		states <- cbind(states,final.fit$season[(m-i)+(1:nr)])
    colnames(states)[nc+(1:m)] <- paste("s",1:m,sep="")
  }
	states <- ts(states,frequency=m,start=tspx[1]-1/m)

	# Package output as HoltWinters class
	# structure(list(fitted    = fitted,
	# 				x        = x,
	# 				alpha    = alpha,
	# 				beta     = beta,
	# 				gamma    = gamma,
	# 				coefficients = c(a = final.fit$level[lenx],
	# 						b = if (!is.logical(beta) || beta) final.fit$trend[lenx],
	# 						s = if (!is.logical(gamma) || gamma) final.fit$season[lenx - m + 1L:m]),
	# 				seasonal  = seasonal,
	# 				exponential = exponential,
	# 				SSE       = final.fit$SSE,
	# 				call      = match.call(),
	# 				level = final.fit$level,
	# 				trend = final.fit$trend,
	# 				season = final.fit$season,
	# 				phi = phi
	# 		),
	# 		class = "HoltWinters"
	# )
	# Package output as ets class
	damped <- (phi<1.0)
	if(seasonal=="additive") # This should not happen
		components <- c("A",trendtype,seasontype,damped)
	else if(seasonal=="multiplicative")
  	components <- c("M",trendtype,seasontype, damped)
  else if(seasonal=="none" & exponential)
		components <- c("M",trendtype,seasontype,damped)
  else# if(seasonal=="none" & !exponential)
  	components <- c("A",trendtype,seasontype, damped)

  initstate <- states[1,]
  param <- alpha
  names(param) <- "alpha"
  if(trendtype!="N")
  {
  	param <- c(param,beta=beta)
    names(param)[length(param)] <- "beta"
  }
  if(seasontype!="N")
  {
  	param <- c(param,gamma=gamma)
    names(param)[length(param)] <- "gamma"
  }
  if(damped)
  {
  	param <- c(param,phi=phi)
    names(param)[length(param)] <- "phi"
  }

  if(components[1]=="A")
  	sigma2 <- mean(final.fit$residuals^2)
  else
  	sigma2 <- mean((final.fit$residuals/fitted)^2)
	structure(list(fitted    = fitted,
			residuals=final.fit$residuals,
			components=components,
					x        = x,
					par=c(param,initstate),
					initstate=initstate,
					states=states,
					SSE       = final.fit$SSE,
					sigma2 = sigma2,
					call      = match.call(),
					m = m
			),
			class = "ets"
	)
}

###################################################################################
#filter function
zzhw <- function(x, lenx, alpha=NULL, beta=NULL, gamma=NULL, seasonal="additive", m,
	dotrend=FALSE, doseasonal=FALSE, l.start=NULL, exponential = NULL, phi=NULL,
	b.start=NULL, s.start=NULL)
{

	if(exponential!=TRUE || is.null(exponential))
		exponential <- FALSE

	if(is.null(phi) || !is.numeric(phi))
		phi <- 1

	#initialise array of l, b, s
	level <- trend <- season <- xfit <- residuals <- numeric(lenx)
	SSE <- 0

	if(!dotrend){
		beta <- 0
		b.start <- 0
	}
	if(!doseasonal){
		gamma <- 0
		s.start[1:length(s.start)] <- ifelse(seasonal=="additive",0,1)
	}
	lastlevel <- level0 <- l.start
	lasttrend <- trend0 <- b.start
	season0 <- s.start

	for(i in 1:lenx){
		# definel l(t-1)
		if(i>1)
			lastlevel <- level[i-1]
		#define b(t-1)
		if(i>1)
			lasttrend <- trend[i-1]
		#define s(t-m)
		if(i>m)
			lastseason <- season[i-m]
		else
			lastseason <- season0[i]
		if(is.na(lastseason))
			lastseason <- ifelse(seasonal=="additive",0,1)

		#stop((lastlevel + phi*lasttrend)*lastseason)

		#forecast for this period i
		if(seasonal=="additive"){
			if(!exponential)
				xhat <- lastlevel + phi*lasttrend + lastseason
			else
				xhat <- lastlevel * lasttrend^phi + lastseason
		}else {
			if(!exponential)
				xhat <- (lastlevel + phi*lasttrend)*lastseason
			else
				xhat <- lastlevel * lasttrend^phi * lastseason
		}

		xfit[i] <- xhat
		res <- x[i] - xhat
		residuals[i] <- res
		SSE <- SSE + res*res

		#calculate level[i]
		if(seasonal=="additive"){
			if(!exponential)
				level[i] <- alpha * (x[i]-lastseason) + (1 - alpha)*(lastlevel + phi*lasttrend)
			else
				level[i] <- alpha * (x[i]-lastseason) + (1 - alpha)*(lastlevel*lasttrend^phi)
		}
		else {
			if(!exponential)
				level[i] <- alpha * (x[i]/lastseason) + (1 - alpha)*(lastlevel + phi*lasttrend)
			else
				level[i] <- alpha * (x[i]/lastseason) + (1 - alpha)*(lastlevel * lasttrend^phi)
		}

		#calculate trend[i]
		if(!exponential)
			trend[i] <- beta*(level[i] - lastlevel) + (1 - beta)* phi* lasttrend
		else
			trend[i] <- beta*(level[i]/lastlevel) + (1 - beta)* lasttrend^phi

		#calculate season[i]
		if(seasonal=="additive"){
			if(!exponential)
				season[i] <- gamma*(x[i] - lastlevel- phi*lasttrend) + (1 - gamma) * lastseason
			else
				season[i] <- gamma*(x[i] - lastlevel*lasttrend^phi) + (1 - gamma) * lastseason
		}else{
			if(!exponential)
				season[i] <- gamma*(x[i]/(lastlevel+phi*lasttrend)) + (1 - gamma) * lastseason
			else
				season[i] <- gamma*(x[i]/(lastlevel*lasttrend^phi)) + (1 - gamma) * lastseason
		}
	}

	list(SSE=SSE,
			fitted= xfit,
			residuals = residuals,
			level = c(level0,level),
			trend=c(trend0,trend),
			season=c(season0,season),
			phi = phi
			)
}

ses <- function (y, h = 10, level = c(80, 95), fan = FALSE, initial=c("optimal","simple"),
  alpha=NULL, lambda=NULL, biasadj=FALSE, x=y, ...)
{
  initial <- match.arg(initial)

  if(initial=="optimal")
    fcast <- forecast(ets(x, "ANN", alpha=alpha, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level, fan = fan, ...)
  else
    fcast <- forecast(HoltWintersZZ(x, alpha=alpha, beta=FALSE, gamma=FALSE, lambda=lambda, biasadj=biasadj), h, level = level, fan = fan, ...)

  fcast$method <- fcast$model$method <- "Simple exponential smoothing"
  fcast$model$call <- match.call()
  fcast$series <- deparse(substitute(y))

  return(fcast)
}

holt <- function (y, h = 10, damped = FALSE, level = c(80, 95), fan = FALSE,
          initial=c("optimal","simple"), exponential=FALSE, alpha=NULL, beta=NULL,
          phi=NULL, lambda=NULL, biasadj=FALSE, x=y, ...)
{
  initial <- match.arg(initial)
  if(initial=="optimal" | damped)
  {
  	if(exponential)
	    fcast <- forecast(ets(x, "MMN", alpha=alpha, beta=beta, phi=phi, damped = damped, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level, fan = fan, ...)
	  else
	    fcast <- forecast(ets(x, "AAN", alpha=alpha, beta=beta, phi=phi, damped = damped, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level, fan = fan, ...)
	}
  else
    fcast <- forecast(HoltWintersZZ(x, alpha=alpha, beta=beta, gamma=FALSE, phi=phi, exponential=exponential, lambda=lambda, biasadj=biasadj),
    	h, level = level, fan = fan, ...)
  if (damped)
  {
	  fcast$method <- "Damped Holt's method"
    if(initial=="simple")
      warning("Damped Holt's method requires optimal initialization")
  }
	else
	  fcast$method <- "Holt's method"
  if(exponential)
	  fcast$method <- paste(fcast$method,"with exponential trend")
  fcast$model$method <- fcast$method
  fcast$model$call <- match.call()
  fcast$series <- deparse(substitute(y))

  return(fcast)
}

hw <- function(y, h = 2 * frequency(x), seasonal = c("additive","multiplicative"), damped = FALSE,
    level = c(80, 95), fan = FALSE, initial=c("optimal","simple"), exponential=FALSE,
    alpha=NULL, beta=NULL, gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE, x=y, ...)
{
  initial <- match.arg(initial)
  seasonal <- match.arg(seasonal)
  if(initial=="optimal" | damped)
  {
    if (seasonal == "additive" & exponential)
    	stop("Forbidden model combination")
    else if(seasonal=="additive" & !exponential)
      fcast <- forecast(ets(x, "AAA", alpha=alpha, beta=beta, gamma=gamma, phi=phi, damped = damped, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level,  fan = fan, ...)
    else if(seasonal!="additive" & exponential)
      fcast <- forecast(ets(x, "MMM", alpha=alpha, beta=beta, gamma=gamma, phi=phi, damped = damped, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level,  fan = fan, ...)
    else #if(seasonal!="additive" & !exponential)
      fcast <- forecast(ets(x, "MAM", alpha=alpha, beta=beta, gamma=gamma, phi=phi, damped = damped, opt.crit="mse", lambda=lambda, biasadj=biasadj), h, level = level, fan = fan, ...)
  }
  else
    fcast <- forecast(HoltWintersZZ(x, alpha=alpha, beta=beta, gamma=gamma, phi=phi, seasonal=seasonal,exponential=exponential, lambda=lambda, biasadj=biasadj),
    	h, level = level, fan = fan, ...)
  if (seasonal == "additive")
    fcast$method <- "Holt-Winters' additive method"
  else
    fcast$method <- "Holt-Winters' multiplicative method"
  if(exponential)
  	fcast$method <- paste(fcast$method,"with exponential trend")
  if (damped)
  {
    fcast$method <- paste("Damped",fcast$method)
    if(initial=="simple")
      warning("Damped methods require optimal initialization")
  }
  fcast$model$method <- fcast$method
  fcast$model$call <- match.call()
  fcast$series <- deparse(substitute(y))

  return(fcast)
}

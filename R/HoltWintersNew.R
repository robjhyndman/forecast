#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

HoltWintersNew <-
		function (x,
				
				# smoothing parameters
				alpha    = NULL, # level
				beta     = NULL, # trend
				gamma    = NULL, # seasonal component
				seasonal = c("additive", "multiplicative"),
				start.periods = 2,
				
				# starting values
				l.start  = NULL, # level
				b.start  = NULL, # trend
				s.start  = NULL, # seasonal components vector of length `period'
				
				# starting values for optim
				optim.start = NULL,
				#optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
				optim.control = list()
)
{
	x <- as.ts(x)
	seasonal <- match.arg(seasonal)
	f <- frequency(x) 
	lenx=length(x)
	
	if(!is.null(alpha) && (alpha == 0))
		stop ("cannot fit models without level ('alpha' must not be 0 or FALSE).")
	if(!all(is.null(c(alpha, beta, gamma))) &&
			any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
		stop ("'alpha', 'beta' and 'gamma' must be within the unit interval.")
	if((is.null(gamma) || gamma > 0)) {
		if (seasonal == "multiplicative" && any(x == 0))
			stop ("data must be non-zero for multiplicative Holt-Winters.")
		if (start.periods < 2)
			stop ("need at least 2 periods to compute seasonal start values.")
	}
	
	
	
	## initialise l0, b0, s0
	if(!is.null(gamma) && is.logical(gamma) && !gamma) {
		if(is.null(l.start))
			l.start <- x[1L]
		if(is.null(b.start))
			if(is.null(beta) || !is.logical(beta) || beta){
				if(seasonal!="multiplicative")
					b.start <- x[2L] - x[1L]
				else
					b.start <- x[2L]/x[1L]	
			}
		start.time <- 1
		s.start    <- 0
	} else {
		## seasonal Holt-Winters
		if (is.null(l.start)) {
			temp<-0
			for(i in 1:f)
				temp<-temp+x[i]
			l.start = temp/f
		}
		if (is.null(b.start)) {
			temp<-0
			for(i in 1:f)
				temp<-temp+(x[i+f]-x[i])/f
			b.start = temp/f
		}
		if (is.null(s.start)) {
			for(i in 1:f){
				if(seasonal!="multiplicative")
					s.start[i] <- x[i]-l.start
				else
					s.start[i] <- x[i]/l.start
			}
		}
		start.time <- 1	
	}
	
	
	
	
	#initialise smoothing parameters
	lower=c(rep(0.0001,3), 0.8)
	upper=c(rep(0.9999,3),0.98)
	if(!is.null(beta) && is.logical(beta) && !beta)
		trendtype="N"
	else
		trendtype="A"
	if(!is.null(gamma) && is.logical(gamma) && !gamma)
		seasontype="N"
	else
		seasontype="A"
	
		
	initparam <- function(alpha,beta,gamma,phi,trendtype,seasontype,damped,lower,upper,m)
	{
		# Set up initial parameters
		par <- numeric(0)
		if(is.null(alpha))
		{
			if(m > 12)
				alpha <- 0.0002
			if(is.null(beta) & is.null(gamma))
				alpha <- lower[1] + .5*(upper[1]-lower[1])
			else if(is.null(gamma))
				alpha <- beta+0.001
			else if(is.null(beta))
				alpha <- 0.999-gamma
			else 
				alpha <- 0.5*(beta - gamma + 1)
			if(alpha < lower[1] | alpha > upper[1])
				stop("Inconsistent parameter limits")
			par <- alpha
			names(par) <- "alpha"
		}
		if(is.null(beta))
		{
			if(trendtype !="N")
			{
				if(m > 12)
					beta <- 0.00015
				else
					beta <- lower[2] + .1*(upper[2]-lower[2])
				if(beta > alpha)
					beta <- min(alpha - 0.0001,0.0001)
				if(beta < lower[2] | beta > upper[2])
					stop("Can't find consistent starting parameters")
				par <- c(par,beta)
				names(par)[length(par)] <- "beta"
			}
		}
		if(is.null(gamma))
		{
			if(seasontype !="N")
			{
				if(m > 12)
					gamma <- 0.0002
				else
					gamma <- lower[3] + .01*(upper[3]-lower[3])
				if(gamma > 1-alpha)
					gamma <- min(0.999-alpha,0.001)
				if(gamma < lower[3] | gamma > upper[3])
					stop("Can't find consistent starting parameters")
				par <- c(par,gamma)
				names(par)[length(par)] <- "gamma"
			}
		}
		if(is.null(phi))
		{
			if(damped)
			{
				phi <- lower[4] + .99*(upper[4]-lower[4])
				par <- c(par,phi)
				names(par)[length(par)] <- "phi"
			}
		}
		
		return(par)
	}
		
	check.param <- function(alpha,beta,gamma,phi,lower,upper,bounds,m)
	{	
		if(bounds != "admissible")
		{
			if(!is.null(alpha))
			{
				if(alpha < lower[1] | alpha > upper[1])
					return(0)
			}
			if(!is.null(beta))
			{
				if(beta < lower[2] | beta > alpha | beta > upper[2])
					return(0)
			}
			if(!is.null(phi))
			{
				if(phi < lower[4] | phi > upper[4])
					return(0)
			}
			if(!is.null(gamma))
			{
				if(gamma < lower[3] | gamma > 1-alpha | gamma > upper[3])
					return(0)
			}
		}
		if(bounds != "usual")
		{
			if(!admissible(alpha,beta,gamma,phi,m))
				return(0)
		}
		return(1)
		
	}
		
	###################################################################################
	#filter function	
	hw <- function(x, lenx, alpha=NULL, beta=NULL, gamma=NULL, start.time=1, seasonal="additive", f, dotrend=FALSE, doseasonal=FALSE, l.start=NULL, b.start=NULL, s.start=NULL){
		
				
		#initialise array of l, b, s
		level = array(0, dim=c(lenx))
		trend = array(0, dim=c(lenx))
		season = array(0, dim=c(lenx))
		xfit = array(0, dim=c(lenx))
		
		# periodicity
		m=f
		
		if(seasonal=="multiplicative")
			additive <- FALSE
		else
			additive <- TRUE
		
		SSE = 0
		
		if(dotrend==FALSE){
			beta<-0
			b.start<-0
		}
		
		if(doseasonal==FALSE){
			gamma<-0
			for(i in 1:length(s.start))
				if(additive)
					s.start[i]<-0
				else
					s.start[i]<-1
		}
		
		level0 <- l.start
		trend0 <- b.start
		season0 <- s.start
		
		
		for(i in start.time:lenx){
			# definel l(t-1)
			if(i>1)
				lastlevel <- level[i-1]
			else
				lastlevel <- level0
			#define b(t-1)
			if(i>1)
				lasttrend <- trend[i-1]
			else
				lasttrend <- trend0
			#define s(t-m)
			if(i>m)
				lastseason <- season[i-m]
			else
				lastseason <- season0[i]	
			if(is.na(lastseason)){
				if(addtive)
					lastseason<-0
				else
					lastseason<-1
			}
			
			#forecast for this period i
			if(additive)
				xhat <- lastlevel + lasttrend + lastseason
			else
				xhat <- (lastlevel + lasttrend)*lastseason
			
						
			xfit[i]=xhat
			
			res <- xhat - x[i]			
			SSE <- SSE + res*res			
					
			#calculate level[i]
			if(additive)
				level[i] <- alpha * (x[i]-lastseason) + (1 - alpha)*(lastlevel + lasttrend)
			else
				level[i] <- alpha * (x[i]/lastseason) + (1 - alpha)*(lastlevel + lasttrend)
			
						
			#calculate trend[i]
			trend[i] <- beta*(level[i] - lastlevel) + (1 - beta)* lasttrend
			
						
			#calculate season[i]
			if(additive)
				season[i] <- gamma*(x[i] - lastlevel-lasttrend) + (1 - gamma) * lastseason
			else
				season[i] <- gamma*(x[i]/(lastlevel+lasttrend)) + (1 - gamma) * lastseason
					
		}
		
		xfit <- ts(xfit, start = start.time, frequency=4)
		list(SSE=SSE,
			 xfit=xfit)
	}
	###################################################################################
	
	
	
	########################
	## initialise smoothing parameter
	if(is.null(optim.start))
		optim.start <- initparam(alpha = alpha, beta = beta, gamma=gamma,phi=1,trendtype=trendtype,seasontype=seasontype,damped=FALSE,lower=lower,upper=upper,m=f)
	
		
	if(!is.na(optim.start["alpha"]))
		alpha2 <- optim.start["alpha"]
	else
		alpha2 <- alpha
	if(!is.na(optim.start["beta"]))
		beta2 <- optim.start["beta"]
	else
		beta2 <- beta
	if(!is.na(optim.start["gamma"]))
		gamma2 <- optim.start["gamma"]
	else
		gamma2 <- gamma
	
	
	
	
#	if(!check.param(alpha = alpha2,beta = beta2, gamma = gamma2,phi=1,lower,upper,bounds="haha",m=f))
#	{
#		print(paste("alpha=", alpha2, "beta=",beta2, "gamma=",gamma2))
#		stop("Parameters out of range")
#	}	
	###################################################################################
	#optimisation: alpha, beta, gamma, if any of them is null, then optimise them
	optimiseStr <- "none"

	
	
	#all null
	if((is.null(alpha))&&(is.null(beta))&&(is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = p[1L], beta=p[2L], gamma = p[3L], start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0, 0, 0), upper = c(1, 1, 1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		alpha <- sol$par[1L]
		beta  <- sol$par[2L]
		gamma <- sol$par[3L]
		
		optimiseStr <- "111"
	}
	else
	#alpha, gamma are null
	if((is.null(alpha))&&(!is.null(beta))&&(is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = p[1L], beta=beta, gamma = p[2L], start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0, 0), upper = c(1, 1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		alpha <- sol$par[1L]
		#beta  <- sol$par[2L]
		gamma <- sol$par[2L]
		
		optimiseStr <- "101"
	}	
	else
	#beta, gamma are null
	if((!is.null(alpha))&&(is.null(beta))&&(is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = alpha, beta=p[1L], gamma = p[2L], start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0, 0), upper = c(1, 1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		#alpha <- sol$par[1L]
		beta  <- sol$par[1L]
		gamma <- sol$par[2L]
		
		optimiseStr <- "011"
	}	
	else
	#alpha, beta are null
	if((is.null(alpha))&&(is.null(beta))&&(!is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = p[1L], beta=p[2L], gamma = gamma, start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0, 0), upper = c(1, 1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		alpha <- sol$par[1L]
		beta  <- sol$par[2L]
		#gamma <- sol$par[3L]
	
		optimiseStr <- "110"
	}
	else
	#alpha is null
	if((is.null(alpha))&&(!is.null(beta))&&(!is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = p[1L], beta=beta, gamma = gamma, start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0), upper = c(1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		alpha <- sol$par[1L]
		#beta  <- sol$par[2L]
		#gamma <- sol$par[3L]
	
		optimiseStr <- "100"
	}
	else
	#beta is null
	
	if((!is.null(alpha))&&(is.null(beta))&&(!is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = alpha, beta=p[1L], gamma = gamma, start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0), upper = c(1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		#alpha <- sol$par[1L]
		beta  <- sol$par[1L]
		#gamma <- sol$par[3L]
		optimiseStr<-"010"
		
	}
	else
	#gamma is null
	if((!is.null(alpha))&&(!is.null(beta))&&(is.null(gamma))){
		error <- function (p) hw(x,lenx=lenx, alpha = alpha, beta=beta, gamma = p[1L], start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)$SSE
		sol   <- optim(optim.start, error, method = "L-BFGS-B",
				lower = c(0), upper = c(1),
				control = optim.control)
		if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
			if (sol$convergence > 50) {
				warning(gettextf("optimization difficulties: %s",
								sol$message), domain = NA)
			} else stop("optimization failure")
		}
		#alpha <- sol$par[1L]
		#beta  <- sol$par[2L]
		gamma <- sol$par[1L]
		
		optimiseStr <- "001"
	}
		
	
	final.fit<-hw(x,lenx=lenx, alpha = alpha, beta=beta, gamma = gamma, start.time=start.time, seasonal=seasonal, f=f, dotrend=(!is.logical(beta) || beta), doseasonal=(!is.logical(gamma) || gamma), l.start=l.start,b.start=b.start,s.start=s.start)
	
	structure(list(fitted    = final.fit$xfit,
					x         = x,
					alpha     = alpha,
					beta      = beta,
					gamma     = gamma,
					l.start = l.start,
					b.start = b.start,
					s.start = s.start,
					seasonal  = seasonal,
					SSE       = final.fit$SSE
									
			),
			class = "HoltWintersNew"
	)
	

	
}

admissible <- function(alpha,beta,gamma,phi,m)
{
	if(is.null(phi))
		phi <- 1
	if(phi < 0 | phi > 1+1e-8)
		return(0)
	if(is.null(gamma))
	{
		if(alpha < 1-1/phi | alpha > 1+1/phi)
			return(0)
		if(!is.null(beta))
		{
			if(beta < alpha * (phi-1) | beta > (1+phi)*(2-alpha))
				return(0)
		}
	}
	else if(m > 1) # Seasonal model
	{
		if(is.null(beta))
			beta <- 0
		if(gamma < max(1-1/phi-alpha,0) | gamma > 1+1/phi-alpha)
			return(0)
		if(alpha < 1-1/phi-gamma*(1-m+phi+phi*m)/(2*phi*m))
			return(0)
		if(beta < -(1-phi)*(gamma/m+alpha))
			return(0)
		
		# End of easy tests. Now use characteristic equation
		P <- c(phi*(1-alpha-gamma),alpha+beta-alpha*phi+gamma-1,rep(alpha+beta-alpha*phi,m-2),(alpha+beta-phi),1)
		roots <- polyroot(P)
		if(max(abs(roots)) > 1+1e-10)
			return(0)
	}
	# Passed all tests
	return(1)
}
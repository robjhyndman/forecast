# TODO: 
# 
# Author: srazbash
###############################################################################
fitSpecificBATS <- function(y, use.box.cox, use.beta, use.damping, seasonal.periods=NULL, starting.params=NULL, x.nought=NULL, ar.coefs=NULL, ma.coefs=NULL, init.box.cox=NULL) {
	if(!is.null(seasonal.periods)) {
		seasonal.periods <- as.integer(sort(seasonal.periods))
	}
	##Meaning/purpose of the first if() statement: If this is the first pass, then use default starting values. Else if it is the second pass, then use the values form the first pass as starting values.
	if(is.null(starting.params)) {
		##Check for the existence of ARMA() coefficients
		if(!is.null(ar.coefs)) {
			p <- length(ar.coefs)
		} else {
			p <- 0
		}
		if(!is.null(ma.coefs)) {
			q <- length(ma.coefs)
		} else {
			q <- 0
		}
		#Calculate starting values:
		if(sum(seasonal.periods) > 16) {
			alpha <- (1e-6)
		} else {
			alpha <- .02
		}
		if(use.beta) {
			if(sum(seasonal.periods) > 16) {
				beta.v <- (5e-7)
			} else {
				beta.v <- .01
			}
			b <- 0
			if(use.damping) {
				#if(sum(seasonal.periods) > 16) {
					small.phi <- .999
				#} else {
				#	small.phi <- .97
				#}
			} else {
				small.phi <- 1
			}
		} else {
			beta.v <- NULL
			b <- NULL
			small.phi <- NULL
			use.damping=FALSE
		}
		if(!is.null(seasonal.periods)) {
			gamma.v <- rep(.001, length(seasonal.periods))
			s.vector <- numeric(sum(seasonal.periods))
			#for(s in seasonal.periods) {
			#	s.vector <- cbind(s.vector, numeric(s))
			#}
		} else {
			gamma.v <- NULL
			s.vector <- NULL
		}
		if(use.box.cox) {
			if(!is.null(init.box.cox)) {
				lambda<-init.box.cox
			} else {
				lambda <- BoxCox.lambda(y, lower=0, upper=1.5)
			}
			y.transformed <- BoxCox(y, lambda=lambda)
			#print(lambda)
		} else { #the "else" is not needed at the moment
			lambda <- NULL
		}
	} else {

		paramz <- unParameterise(starting.params$vect, starting.params$control)
		lambda <- paramz$lambda
		alpha <- paramz$alpha
		beta.v <- paramz$beta
		b <- 0
		small.phi <- paramz$small.phi
		gamma.v <- paramz$gamma.v
		if(!is.null(seasonal.periods)) {
			s.vector <- numeric(sum(seasonal.periods))
		} else {
			s.vector <- NULL
		}
		#ar.coefs <- paramz$ar.coefs
		#ma.coefs <- paramz$ma.coefs
		##Check for the existence of ARMA() coefficients
		if(!is.null(ar.coefs)) {
			p <- length(ar.coefs)
		} else {
			p <- 0
		}
		if(!is.null(ma.coefs)) {
			q <- length(ma.coefs)
		} else {
			q <- 0
		}
	}
	if(is.null(x.nought)) {
		#Start with the seed states equal to zero
		if(!is.null(ar.coefs)) {
			d.vector <- numeric(length(ar.coefs))
		} else {
			d.vector <- NULL
		}
		if(!is.null(ma.coefs)) {
			epsilon.vector <- numeric(length(ma.coefs))
		} else {
			epsilon.vector <- NULL
		}
		x.nought <- makeXMatrix(l=0,b=b, s.vector=s.vector, d.vector=d.vector, epsilon.vector=epsilon.vector)$x
	}
	##Optimise the starting values:
	#Make the parameter vector  parameterise
	param.vector <- parameterise(alpha=alpha, beta.v=beta.v, small.phi=small.phi, gamma.v=gamma.v, lambda=lambda, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	#if(use.box.cox) {
		#Un-transform the seed states
	#	x.nought.untransformed <- InvBoxCox(x.nought, lambda=lambda)
		#Optimise
	#	optim.like <- optim(par=param.vector$vect, fn=calcLikelihood, method="Nelder-Mead", y=y, x.nought=x.nought.untransformed, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q, control=list(maxit=1000))	
	#} else {
		#Optimise
	#	if(length(param.vector$vect) > 1) {
	#		optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformed, method="Nelder-Mead", y=y, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q)
	#	} else {
	#		optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformed, method="BFGS", y=y, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q)
	#	}
	#}
	#print("first optim return code:")
	#print(optim.like$convergence)
	#paramz <- unParameterise(optim.like$par, param.vector$control)
	#param.vector$vect <- optim.like$par
	#lambda <- paramz$lambda
	#alpha <- paramz$alpha
	#beta.v <- paramz$beta
	#small.phi <- paramz$small.phi
	#gamma.v <- paramz$gamma.v
	#ar.coefs <- paramz$ar.coefs
	#ma.coefs <- paramz$ma.coefs
	#if(use.box.cox) {
		#Transform the seed states
	#	x.nought <- BoxCox(x.nought.untransformed, lambda=lambda)	
	#}
	
	
	#w <- makeWMatrix(small.phi=small.phi, seasonal.periods=seasonal.periods, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	w <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = seasonal.periods, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
	#g <- makeGMatrix(alpha=alpha, beta=beta.v, gamma.vector=gamma, seasonal.periods=seasonal.periods, p=p, q=q)
	g <- .Call("makeBATSGMatrix", as.numeric(alpha), beta.v, gamma.v, seasonal.periods, as.integer(p), as.integer(q), PACKAGE="forecast")
	F <- makeFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, gamma.bold.matrix=g$gamma.bold.matrix, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	D <-  F - g$g %*% w$w.transpose

	
	##Set up matrices to find the seed states
	if(use.box.cox) {
		y.transformed <- BoxCox(y, lambda=lambda)
		#x.nought <- BoxCox(x.nought, lambda=lambda)
		y.tilda <- calcModel(y.transformed, x.nought, F, g$g, w)$e
	} else {
		y.tilda <- calcModel(y, x.nought, F, g$g, w)$e
	}
	w.tilda.transpose <- matrix(0, nrow=length(y), ncol=ncol(w$w.transpose))
	w.tilda.transpose[1,] <- w$w.transpose
	#for(i in 2:length(y)) {
	#	w.tilda.transpose[i,] <- w.tilda.transpose[(i-1),] %*% D
	#}
	w.tilda.transpose=.Call("calcWTilda", wTildaTransposes=w.tilda.transpose, Ds=D, PACKAGE = "forecast")
	##If there is a seasonal component in the model, then the follow adjustment need to be made so that the seed states can be found
	if(!is.null(seasonal.periods)) {
		#drop the lines from w.tilda.transpose that correspond to the last seasonal value of each seasonal period
		list.cut.w <- cutW(use.beta=use.beta, w.tilda.transpose=w.tilda.transpose, seasonal.periods=seasonal.periods, p=p, q=q)
		w.tilda.transpose <- list.cut.w$matrix
		mask.vector <- list.cut.w$mask.vector
		##Run the regression to find the SEED STATES
		coefs <- lm(t(y.tilda) ~ w.tilda.transpose - 1)$coefficients
		#print(coefs)
		##Find the ACTUAL SEASONAL seed states
		x.nought <- calcSeasonalSeeds(use.beta=use.beta, coefs=coefs, seasonal.periods=seasonal.periods, mask.vector=mask.vector, p=p, q=q)
	} else {
		#Remove the AR() and MA() bits if they exist
		if((p != 0) | (q != 0)) {
			end.cut <- ncol(w.tilda.transpose)
			start.cut <- end.cut-(p+q)+1
			w.tilda.transpose <- w.tilda.transpose[,-c(start.cut:end.cut)]	
			
		}
		#print(w.tilda.transpose)
		x.nought <- lm(t(y.tilda) ~ w.tilda.transpose - 1)$coefficients
		x.nought <- matrix(x.nought, nrow=length(x.nought), ncol=1)
		##Replace the AR() and MA() bits if they exist
		if((p != 0) | (q != 0)) {
			arma.seed.states <- numeric((p+q))
			arma.seed.states <- matrix(arma.seed.states, nrow=length(arma.seed.states), ncol=1)
			x.nought <- rbind(x.nought, arma.seed.states)
		}
	}
	####
	#Set up environment
	opt.env <- new.env()
	assign("F", F, envir=opt.env)
	assign("w.transpose", w$w.transpose, envir=opt.env)
	assign("g", g$g, envir=opt.env)
	assign("gamma.bold.matrix", g$gamma.bold.matrix, envir=opt.env)
	assign("y", matrix(y, nrow=1, ncol=length(y)), envir=opt.env)
	assign("y.hat", matrix(0, nrow=1, ncol=length(y)), envir=opt.env)
	assign("e", matrix(0, nrow=1, ncol=length(y)), envir=opt.env)
	assign("x", matrix(0, nrow=length(x.nought), ncol=length(y)),  envir=opt.env)
	if(!is.null(seasonal.periods)) {
		tau <- sum(seasonal.periods)
	} else {
		tau <- 0
	}
	
	##Second pass of optimisation
	if(use.box.cox) {
		#Un-transform the seed states
		#x.nought.untransformed <- InvBoxCox(x.nought, lambda=lambda)
		assign("x.nought.untransformed", InvBoxCox(x.nought, lambda=lambda), envir=opt.env)
		#Optimise the likelihood function
		optim.like <- optim(par=param.vector$vect, fn=calcLikelihood, method="Nelder-Mead", opt.env=opt.env, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q, tau=tau, control=list(maxit=(100*length(param.vector$vect)^2)))
		#Get the parameters out of the param.vector
		paramz <- unParameterise(optim.like$par, param.vector$control)
		lambda <- paramz$lambda
		alpha <- paramz$alpha
		beta.v <- paramz$beta
		small.phi <- paramz$small.phi
		gamma.v <- paramz$gamma.v
		ar.coefs <- paramz$ar.coefs
		ma.coefs <- paramz$ma.coefs
		#Transform the seed states
		x.nought <- BoxCox(opt.env$x.nought.untransformed, lambda=lambda)
		
		
		
		##Calculate the variance:
		#1. Re-set up the matrices
		#w <- makeWMatrix(small.phi=small.phi, seasonal.periods=seasonal.periods, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
		w <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = seasonal.periods, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
		#g <- makeGMatrix(alpha=alpha, beta=beta.v, gamma.vector=gamma, seasonal.periods=seasonal.periods, p=p, q=q)
		g <- .Call("makeBATSGMatrix", as.numeric(alpha), beta.v, gamma.v, seasonal.periods, as.integer(p), as.integer(q), PACKAGE="forecast")
		F <- makeFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, gamma.bold.matrix=g$gamma.bold.matrix, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
		#print("here!")
		#print(w)
		#2. Calculate!
		y.transformed <- BoxCox(y, lambda=lambda)
		fitted.values.and.errors <- calcModel(y.transformed, x.nought, F, g$g, w)
		e <- fitted.values.and.errors$e
		fitted.values <- fitted.values.and.errors$y.hat
		fitted.values <- InvBoxCox(fitted.values, lambda=lambda)
		variance <- sum((e*e))/length(y)
		#e <- InvBoxCox(e, lambda=lambda)
		#ee <- y-fitted.values
		
	} else { #else if we are not using the Box-Cox transformation
		#Optimise the likelihood function
		if(length(param.vector$vect) > 1) {
			optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformed, method="Nelder-Mead", opt.env=opt.env, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q, tau=tau, control=list(maxit=(100*length(param.vector$vect)^2)))
		} else {
			optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformed, method="BFGS", opt.env=opt.env, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, p=p, q=q, tau=tau)
		}
		#Get the parameters out of the param.vector
		paramz <- unParameterise(optim.like$par, param.vector$control)
		lambda <- paramz$lambda
		alpha <- paramz$alpha
		beta.v <- paramz$beta
		small.phi <- paramz$small.phi
		gamma.v <- paramz$gamma.v
		ar.coefs <- paramz$ar.coefs
		ma.coefs <- paramz$ma.coefs
		
		##Calculate the variance:
		#1. Re-set up the matrices
		#w <- makeWMatrix(small.phi=small.phi, seasonal.periods=seasonal.periods, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
		w <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = seasonal.periods, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
		#g <- makeGMatrix(alpha=alpha, beta=beta.v, gamma.vector=gamma, seasonal.periods=seasonal.periods, p=p, q=q)
		g <- .Call("makeBATSGMatrix", as.numeric(alpha), beta.v, gamma.v, seasonal.periods, as.integer(p), as.integer(q), PACKAGE="forecast")
		F <- makeFMatrix(alpha=alpha, beta=beta.v, small.phi <- small.phi, seasonal.periods=seasonal.periods, gamma.bold.matrix=g$gamma.bold.matrix, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
		#2. Calculate!
		fitted.values.and.errors <- calcModel(y, x.nought, F, g$g, w)
		e <- fitted.values.and.errors$e
		fitted.values <- fitted.values.and.errors$y.hat
		variance <- sum((e*e))/length(y)
	}
	#Get the likelihood
	likelihood <- optim.like$value
	#Calculate the AIC
	aic <- likelihood+2*(length(param.vector$vect)+nrow(x.nought))
	
	
	#Make a list object
	model.for.output <- list(lambda=lambda, alpha=alpha, beta=beta.v, damping.parameter=small.phi, gamma.values=gamma.v, ar.coefficients=ar.coefs, ma.coefficients=ma.coefs, likelihood=likelihood, optim.return.code=optim.like$convergence, variance=variance, AIC=aic, parameters=list(vect=optim.like$par, control=param.vector$control), seed.states=x.nought, fitted.values=c(fitted.values), errors=c(e), x=fitted.values.and.errors$x, seasonal.periods=seasonal.periods, y=y)
	class(model.for.output) <- "bats"
	####
	return(model.for.output)
}

calcModel <- function(y, x.nought, F, g, w) { #w is passed as a list
	length.ts <- length(y)
	x <- matrix(0, nrow=length(x.nought), ncol=length.ts)
	y.hat <- matrix(0,nrow=1, ncol=length.ts)
	e <- matrix(0, nrow=1, ncol=length.ts)
	y.hat[,1] <- w$w.transpose %*% x.nought
	e[,1] <- y[1]-y.hat[,1]
	x[,1] <- F %*% x.nought + g %*% e[,1]
	y <- matrix(y, nrow=1, ncol=length.ts)
	
	loop <- .Call( "calcBATS", ys=y, yHats=y.hat, wTransposes = w$w.transpose, Fs=F, xs=x, gs=g, es=e, PACKAGE = "forecast" )
			
	return(list(y.hat=loop$y.hat, e=loop$e, x=loop$x))
}

calcLikelihood <- function(param.vector, opt.env, use.beta, use.small.phi, seasonal.periods, p=0, q=0, tau=0) {
	#param vector should be as follows: Box-Cox.parameter, alpha, beta, small.phi, gamma.vector, ar.coefs, ma.coefs 
	#Put the components of the param.vector into meaningful individual variables
	box.cox.parameter <- param.vector[1]
	alpha <- param.vector[2]
	if(use.beta) {
		if(use.small.phi) {
			small.phi <- param.vector[3]
			beta.v <- param.vector[4]
			gamma.start <- 5
		} else {
			small.phi <- 1
			beta.v <- param.vector[3]
			gamma.start <- 4
		}
	} else {
		small.phi <- NULL
		beta.v <- NULL
		gamma.start <- 3
	}
	if(!is.null(seasonal.periods)) {
		gamma.vector <- param.vector[gamma.start:(gamma.start+length(seasonal.periods)-1)]
		final.gamma.pos <- gamma.start+length(gamma.vector)-1
	} else {
		gamma.vector=NULL
		final.gamma.pos <- gamma.start-1
	}
	if(p != 0) {
		ar.coefs <- matrix(param.vector[(final.gamma.pos+1):(final.gamma.pos+p)], nrow=1, ncol=p)
	} else {
		ar.coefs <- NULL
	}
	if(q != 0) {
		ma.coefs <- matrix(param.vector[(final.gamma.pos+p+1):length(param.vector)], nrow=1, ncol=q)
	} else {
		ma.coefs <- NULL
	}
	x.nought <- BoxCox(opt.env$x.nought.untransformed, lambda=box.cox.parameter)	
	#w <- makeWMatrix(small.phi=small.phi, seasonal.periods=seasonal.periods, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	#w <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = seasonal.periods, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
	.Call("updateWtransposeMatrix", wTranspose_s=opt.env$w.transpose, smallPhi_s=small.phi, tau_s=as.integer(tau), arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, p_s=as.integer(p), q_s=as.integer(q), PACKAGE = "forecast")

	#g <- makeGMatrix(alpha=alpha, beta=beta, gamma.vector=gamma.vector, seasonal.periods=seasonal.periods, p=p, q=q)
	#g <- .Call("makeBATSGMatrix", as.numeric(alpha), beta.v, gamma.vector, seasonal.periods, as.integer(p), as.integer(q), PACKAGE="forecast")
	.Call("updateGMatrix", g_s=opt.env$g, gammaBold_s=opt.env$gamma.bold.matrix, alpha_s=alpha, beta_s=beta.v, gammaVector_s=gamma.vector, seasonalPeriods_s=seasonal.periods, PACKAGE="forecast")
	
	
	#F <- makeFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, gamma.bold.matrix=g$gamma.bold.matrix, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	.Call("updateFMatrix", opt.env$F, small.phi, alpha, beta.v, opt.env$gamma.bold.matrix, ar.coefs, ma.coefs, tau, PACKAGE="forecast")	
	
	mat.transformed.y <- BoxCox(opt.env$y, box.cox.parameter)
	n <- ncol(opt.env$y)
	######################################################################
	#e <- calcModel(y=transformed.y, x.nought=x.nought, F=F, g=g$g, w=w)$e
	######################
	#### calcModel() code:
	##
	
	#x <- matrix(0, nrow=length(x.nought), ncol=n)
	#y.hat <- matrix(0,nrow=1, ncol=n)
	#e <- matrix(0, nrow=1, ncol=n)
	#opt.env$y.hat[,1] <- w$w.transpose %*% x.nought
	#opt.env$e[,1] <- mat.transformed.y[,1]-opt.env$y.hat[,1]
	#opt.env$x[,1] <- opt.env$F %*% x.nought + g$g %*% opt.env$e[,1]
	#mat.transformed.y <- matrix(transformed.y, nrow=1, ncol=n)
	
	.Call( "calcBATSFaster", ys=mat.transformed.y, yHats=opt.env$y.hat, wTransposes = opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s = x.nought, sPeriods_s = seasonal.periods, betaV = beta.v, tau_s = as.integer(tau), p_s = as.integer(p), q_s = as.integer(q), PACKAGE = "forecast" )
	
	
	##
	####
	####################################################################
	

	log.likelihood <- n*log(sum(opt.env$e^2))-2*(box.cox.parameter-1)*sum(log(opt.env$y))

	
	#print("two 2 - before")
	#D <- opt.env$F - g$g %*% w$w.transpose
	assign("D", (opt.env$F - opt.env$g %*% opt.env$w.transpose), envir=opt.env)
	#print("two 2 - AFTER")
	#print(param.vector)
	if(checkAdmissibility(opt.env, box.cox=box.cox.parameter, small.phi=small.phi, ar.coefs=ar.coefs, ma.coefs=ma.coefs, tau=tau)) {
		return(log.likelihood)
	} else {
		return(10^20)
	}
}

calcLikelihoodNOTransformed <- function(param.vector, opt.env, x.nought, use.beta, use.small.phi, seasonal.periods, p=0, q=0, tau=0) {
	#The likelihood function without the Box-Cox Transformation
	#param vector should be as follows: alpha, beta, small.phi, gamma.vector, ar.coefs, ma.coefs 
	#Put the components of the param.vector into meaningful individual variables
	alpha <- param.vector[1]
	if(use.beta) {
		if(use.small.phi) {
			small.phi <- param.vector[2]
			beta.v <- param.vector[3]
			gamma.start <- 4
		} else {
			small.phi <- 1
			beta.v <- param.vector[2]
			gamma.start <- 3
		}
	} else {
		small.phi <- NULL
		beta.v <- NULL
		gamma.start <- 2
	}
	if(!is.null(seasonal.periods)) {
		gamma.vector <- param.vector[gamma.start:(gamma.start+length(seasonal.periods)-1)]
		final.gamma.pos <- gamma.start+length(gamma.vector)-1
	} else {
		gamma.vector=NULL
		final.gamma.pos <- gamma.start-1
	}
	if(p != 0) {
		ar.coefs <- matrix(param.vector[(final.gamma.pos+1):(final.gamma.pos+p)], nrow=1, ncol=p)
	} else {
		ar.coefs <- NULL
	}
	if(q != 0) {
		ma.coefs <- matrix(param.vector[(final.gamma.pos+p+1):length(param.vector)], nrow=1, ncol=q)
	} else {
		ma.coefs <- NULL
	}
	
	
	#w <- makeWMatrix(small.phi=small.phi, seasonal.periods=seasonal.periods, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	#w <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = seasonal.periods, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
	.Call("updateWtransposeMatrix", wTranspose_s=opt.env$w.transpose, smallPhi_s=small.phi, tau_s=as.integer(tau), arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, p_s=as.integer(p), q_s=as.integer(q), PACKAGE = "forecast")
	#g <- makeGMatrix(alpha=alpha, beta=beta, gamma.vector=gamma.vector, seasonal.periods=seasonal.periods, p=p, q=q)
	#g <- .Call("makeBATSGMatrix", alpha, beta.v, gamma.vector, seasonal.periods, as.integer(p), as.integer(q), PACKAGE="forecast")
	.Call("updateGMatrix", g_s=opt.env$g, gammaBold_s=opt.env$gamma.bold.matrix, alpha_s=alpha, beta_s=beta.v, gammaVector_s=gamma.vector, seasonalPeriods_s=seasonal.periods, PACKAGE="forecast")

	#F <- makeFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, gamma.bold.matrix=g$gamma.bold.matrix, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
	.Call("updateFMatrix", opt.env$F, small.phi, alpha, beta.v, opt.env$gamma.bold.matrix, ar.coefs, ma.coefs, tau, PACKAGE="forecast")
	n <- ncol(opt.env$y)
	
	#########################################################################################
	#e <- calcModel(y=y, x.nought=x.nought, F=F, g=g$g, w=w)$e
	######################
	#### calcModel() code:
	##
	#x <- matrix(0, nrow=length(x.nought), ncol=n)
	#y.hat <- matrix(0,nrow=1, ncol=n)
	#e <- matrix(0, nrow=1, ncol=n)
	#opt.env$y.hat[,1] <- w$w.transpose %*% x.nought
	#opt.env$e[,1] <- opt.env$y[,1]-opt.env$y.hat[,1]
	#opt.env$x[,1] <- opt.env$F %*% x.nought + g$g %*% opt.env$e[,1]
	#mat.y <- matrix(opt.env$y, nrow=1, ncol=n)
	
	.Call( "calcBATSFaster", ys=opt.env$y, yHats=opt.env$y.hat, wTransposes = opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s = x.nought, sPeriods_s = seasonal.periods, betaV = beta.v, tau_s = as.integer(tau), p_s = as.integer(p), q_s = as.integer(q) , PACKAGE = "forecast" )
	##
	####
	####################################################################
	
	
	log.likelihood <- n*log(sum(opt.env$e*opt.env$e))
	#print("three 3 - before")
	#D <- opt.env$F - g$g %*% w$w.transpose
	assign("D", (opt.env$F - opt.env$g %*% opt.env$w.transpose), envir=opt.env)
	#print("three 3  - AFTER")
	
	if(checkAdmissibility(opt.env=opt.env, box.cox=NULL, small.phi=small.phi, ar.coefs=ar.coefs, ma.coefs=ma.coefs, tau=tau)) {
		return(log.likelihood)
	} else {
		return(10^20)
	}
}


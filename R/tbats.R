# Author: srazbash
###############################################################################



#' TBATS model (Exponential smoothing state space model with Box-Cox
#' transformation, ARMA errors, Trend and Seasonal components)
#' 
#' Fits a TBATS model applied to \code{y}, as described in De Livera, Hyndman &
#' Snyder (2011). Parallel processing is used by default to speed up the
#' computations.
#' 
#' @aliases as.character.tbats print.tbats
#' 
#' @param y The time series to be forecast. Can be \code{numeric}, \code{msts}
#' or \code{ts}. Only univariate time series are supported.
#' @param use.box.cox \code{TRUE/FALSE} indicates whether to use the Box-Cox
#' transformation or not. If \code{NULL} then both are tried and the best fit
#' is selected by AIC.
#' @param use.trend \code{TRUE/FALSE} indicates whether to include a trend or
#' not. If \code{NULL} then both are tried and the best fit is selected by AIC.
#' @param use.damped.trend \code{TRUE/FALSE} indicates whether to include a
#' damping parameter in the trend or not. If \code{NULL} then both are tried
#' and the best fit is selected by AIC.
#' @param seasonal.periods If \code{y} is \code{numeric} then seasonal periods
#' can be specified with this parameter.
#' @param use.arma.errors \code{TRUE/FALSE} indicates whether to include ARMA
#' errors or not. If \code{TRUE} the best fit is selected by AIC. If
#' \code{FALSE} then the selection algorithm does not consider ARMA errors.
#' @param use.parallel \code{TRUE/FALSE} indicates whether or not to use
#' parallel processing.
#' @param num.cores The number of parallel processes to be used if using
#' parallel processing. If \code{NULL} then the number of logical cores is
#' detected and all available cores are used.
#' @param bc.lower The lower limit (inclusive) for the Box-Cox transformation.
#' @param bc.upper The upper limit (inclusive) for the Box-Cox transformation.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param model Output from a previous call to \code{tbats}. If model is
#' passed, this same model is fitted to \code{y} without re-estimating any
#' parameters.
#' @param ... Additional arguments to be passed to \code{auto.arima} when
#' choose an ARMA(p, q) model for the errors. (Note that xreg will be ignored,
#' as will any arguments concerning seasonality and differencing, but arguments
#' controlling the values of p and q will be used.)
#' @return An object with class \code{c("tbats", "bats")}. The generic accessor
#' functions \code{fitted.values} and \code{residuals} extract useful features
#' of the value returned by \code{bats} and associated functions. The fitted
#' model is designated TBATS(omega, p,q, phi, <m1,k1>,...,<mJ,kJ>) where omega
#' is the Box-Cox parameter and phi is the damping parameter; the error is
#' modelled as an ARMA(p,q) process and m1,...,mJ list the seasonal periods
#' used in the model and k1,...,kJ are the corresponding number of Fourier
#' terms used for each seasonality.
#' @author Slava Razbash and Rob J Hyndman
#' @seealso \code{\link{tbats.components}}.
#' @references De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011),
#' Forecasting time series with complex seasonal patterns using exponential
#' smoothing, \emph{Journal of the American Statistical Association},
#' \bold{106}(496), 1513-1527.
#' @keywords ts
#' @examples
#' 
#' \dontrun{
#' fit <- tbats(USAccDeaths)
#' plot(forecast(fit))
#' 
#' taylor.fit <- tbats(taylor)
#' plot(forecast(taylor.fit))}
#' 
#' @export
tbats <- function(y, use.box.cox=NULL, use.trend=NULL, use.damped.trend=NULL,
	seasonal.periods=NULL, use.arma.errors=TRUE, use.parallel=length(y)>1000, num.cores=2,
	bc.lower=0, bc.upper=1, biasadj=FALSE, model=NULL, ...)
{
  if (any(class(y) %in% c("data.frame", "list", "matrix", "mts")))
    stop("y should be a univariate time series")

  seriesname <- deparse(substitute(y))

  origy <- y

  # Get seasonal periods
  if(is.null(seasonal.periods))
  {
    if(any(class(y) == "msts"))
      seasonal.periods <- attr(y,"msts")
    else if(class(y) == "ts")
      seasonal.periods <- frequency(y)
    else
    {
      y <- as.ts(y)
      seasonal.periods <- 1
    }
  }
  else
  {
    # Add ts attributes
    if(!any(class(y) == "ts"))
      y <- msts(y, seasonal.periods)
  }
  seasonal.periods <- unique(pmax(seasonal.periods,1))
  if(all(seasonal.periods == 1))
    seasonal.periods <- NULL

  ny <- length(y)
  y <- na.contiguous(y)
  if (ny != length(y))
    warning("Missing values encountered. Using longest contiguous portion of time series")

  # Refit model if available
  if(!is.null(model))
  {
    if (is.element("tbats", class(model)))
      refitModel <- try(fitPreviousTBATSModel(y, model=model), silent=TRUE)
    else if(is.element("bats", class(model)))
      refitModel <- bats(origy, model=model)
    return (refitModel)
  }

  # Return constant model if required
  if(is.constant(y))
  {
    fit <- list(y=y,x=matrix(y,nrow=1,ncol=ny),errors=y*0,fitted.values=y,seed.states=matrix(y[1]),
      AIC=-Inf,likelihood=-Inf,variance=0,alpha=0.9999, method="TBATS", call=match.call())
    return(structure(fit,class='bats'))
  }

  # Check for observations are positive
	if(any((y <= 0)))
		use.box.cox <- FALSE

	# Fit non-seasonal model as a benchmark
	non.seasonal.model <- bats(as.numeric(y), use.box.cox=use.box.cox, use.trend=use.trend,
	                           use.damped.trend=use.damped.trend, use.arma.errors=use.arma.errors,
	                           use.parallel=use.parallel, num.cores=num.cores,
	                           bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj, ...)


  # If non-seasonal data, return the non-seasonal model
  if(is.null(seasonal.periods))
  {
		non.seasonal.model$call <- match.call()
		attributes(non.seasonal.model$fitted.values) <- attributes(non.seasonal.model$errors) <- attributes(origy)
		non.seasonal.model$y <- origy
		return(non.seasonal.model)
	}
	else
  {
		seasonal.mask <- (seasonal.periods == 1)
		seasonal.periods <- seasonal.periods[!seasonal.mask]
	}

	if(is.null(use.box.cox)) {
		use.box.cox <- c(FALSE, TRUE)
	}
	if(any(use.box.cox)) {
		init.box.cox <- BoxCox.lambda(y, lower=bc.lower, upper=bc.upper)
	} else {
		init.box.cox <- NULL
	}
	if(is.null(use.trend)) {
		use.trend <- c(FALSE, TRUE)
	} else if(use.trend == FALSE) {
		use.damped.trend <- FALSE
	}
	if(is.null(use.damped.trend)) {
		use.damped.trend <- c(FALSE, TRUE)
	}
	#Set a vector of model params for later comparison
	model.params <- logical(length=3)
	model.params[1] <- any(use.box.cox)
	model.params[2] <- any(use.trend)
	model.params[3] <- any(use.damped.trend)

	y <- as.numeric(y)
	n <- length(y)
	k.vector <- rep(1, length(seasonal.periods))

	if(use.parallel) {
		if(is.null(num.cores)) {
			num.cores <- detectCores(all.tests = FALSE, logical = TRUE)
		}
		clus <- makeCluster(num.cores)
	}

	best.model <- try(fitSpecificTBATS(y, use.box.cox = model.params[1], use.beta = model.params[2],
	                      use.damping = model.params[3], seasonal.periods = seasonal.periods,
	                      k.vector = k.vector, init.box.cox=init.box.cox,
	                      bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj), silent=TRUE)
  if(is.element("try-error",class(best.model)))
	  best.model <- list(AIC=Inf)

	for(i in 1:length(seasonal.periods)) {
		if(seasonal.periods[i] == 2) {
			next
		}
		max.k <- floor(((seasonal.periods[i]-1)/2))
		if(i != 1) {
			current.k <- 2
			while(current.k <= max.k) {
				if(seasonal.periods[i]%%current.k != 0) {
					current.k <- current.k+1
					next
				}
				latter <- seasonal.periods[i]/current.k

				if(any(((seasonal.periods[1:(i-1)]%%latter) == 0))) {
					max.k <- current.k-1
					break
				} else {
					current.k <- current.k+1
				}

			}
		}

		if(max.k == 1) {
				next
			}
			if(max.k <= 6) {
				k.vector[i] <- max.k
				best.model$AIC <- Inf
				repeat {
					#old.k <- k.vector[i]
					#k.vector[i] <- k.vector[i]-1
					new.model <- try(fitSpecificTBATS(y, use.box.cox = model.params[1], use.beta = model.params[2],
					                                  use.damping = model.params[3], seasonal.periods = seasonal.periods,
					                                  k.vector = k.vector, init.box.cox=init.box.cox,
					                                  bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
						silent=TRUE)
					if(is.element("try-error",class(new.model)))
						new.model <- list(AIC=Inf)

					if(new.model$AIC > best.model$AIC) {
						k.vector[i] <- k.vector[i]+1
						break
					} else {
						if(k.vector[i] == 1) {
							break
						}
						k.vector[i] <- k.vector[i]-1
						best.model <- new.model
					}

				}
				next
			} else {
				#Three different k vectors
				step.up.k <- k.vector
				step.down.k <- k.vector
				step.up.k[i] <- 7
				step.down.k[i] <- 5
				k.vector[i] <- 6
				#Fit three different models

				###if(use.parallel) then do parallel
				if(use.parallel) {
					k.control.array <- rbind(step.up.k, step.down.k, k.vector)
					models.list <- clusterApplyLB(clus, c(1:3), parFitSpecificTBATS, y=y,
					                              box.cox=model.params[1], trend = model.params[2],
					                              damping = model.params[3], seasonal.periods = seasonal.periods,
					                              k.control.matrix=k.control.array, init.box.cox=init.box.cox,
					                              bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj)
					up.model <- models.list[[1]]
					level.model <- models.list[[3]]
					down.model <- models.list[[2]]
				} else {
					up.model <- try(fitSpecificTBATS(y, use.box.cox = model.params[1],
					                                 use.beta = model.params[2], use.damping = model.params[3],
					                                 seasonal.periods = seasonal.periods, k.vector = step.up.k,
					                                 init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
						silent=TRUE)
  				if(is.element("try-error",class(up.model)))
						up.model <- list(AIC=Inf)
					level.model <- try(fitSpecificTBATS(y, use.box.cox = model.params[1], use.beta = model.params[2],
					                                    use.damping = model.params[3], seasonal.periods = seasonal.periods,
					                                    k.vector = k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
						silent=TRUE)
  				if(is.element("try-error",class(level.model)))
						level.model <- list(AIC=Inf)
					down.model <- try(fitSpecificTBATS(y, use.box.cox = model.params[1], use.beta = model.params[2],
					                                   use.damping = model.params[3], seasonal.periods = seasonal.periods,
					                                   k.vector = step.down.k, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
						silent=TRUE)
  				if(is.element("try-error",class(down.model)))
						down.model <- list(AIC=Inf)
				}
				#Decide the best model of the three and then follow that direction to find the optimal k
				aic.vector <- c(up.model$AIC, level.model$AIC, down.model$AIC)
				##If shifting down
				if(min(aic.vector) == down.model$AIC) {
					best.model <- down.model
					k.vector[i] <- 5
					repeat{
						k.vector[i] <- k.vector[i]-1
						down.model <- try(fitSpecificTBATS(y=y, use.box.cox=model.params[1], use.beta=model.params[2],
						                                   use.damping=model.params[3], seasonal.periods=seasonal.periods,
						                                   k.vector=k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
  						silent=TRUE)
	  				if(is.element("try-error",class(down.model)))
							down.model <- list(AIC=Inf)
						if(down.model$AIC > best.model$AIC) {
							k.vector[i] <- k.vector[i]+1
							break
						} else {
							best.model <- down.model
						}
						if(k.vector[i] == 1) {
							break
						}
					}

				##If staying level
				} else if(min(aic.vector) == level.model$AIC) {
					best.model <- level.model
					next
				##If shifting up
				} else {
					best.model <- up.model
					k.vector[i] <- 7
					repeat {
						k.vector[i] <- k.vector[i]+1
						up.model <- try(fitSpecificTBATS(y, model.params[1], model.params[2], model.params[3], seasonal.periods, k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
  						silent=TRUE)
    				if(is.element("try-error",class(up.model)))
	  					up.model <- list(AIC=Inf)
						if(up.model$AIC > best.model$AIC) {
							k.vector[i] <- k.vector[i]-1
							break
						} else {
							best.model <- up.model
						}
						if(k.vector[i] == max.k) {
							break
						}
					}
				}
			}
	}
	aux.model <- best.model

	if(non.seasonal.model$AIC < best.model$AIC) {
		best.model <- non.seasonal.model
	}

	if((length(use.box.cox) == 1) & (use.trend[1] == TRUE) & (length(use.trend) == 1) & (length(use.damped.trend) == 1) & (use.parallel)) {
		#In the this case, there is only one alternative.
		use.parallel <- FALSE
		stopCluster(clus)
	} else if((length(use.box.cox) == 1) & (use.trend[1] == FALSE) & (length(use.trend) == 1) & (use.parallel)) {
		#As above, in the this case, there is only one alternative.
		use.parallel <- FALSE
		stopCluster(clus)
	}

	if(use.parallel) {
		#Set up the control array
		control.array <- NULL
		for(box.cox in use.box.cox) {
			for(trend in use.trend) {
				for(damping in use.damped.trend) {
					if((trend == FALSE) & (damping == TRUE)) {
						next
					}
					control.line <- c(box.cox, trend, damping)
					if(!is.null(control.array)) {
						control.array <- rbind(control.array, control.line)
					} else {
						control.array <- control.line
					}
				}
			}
		}
		models.list <- clusterApplyLB(clus, c(1:nrow(control.array)), parFilterTBATSSpecifics, y=y, control.array=control.array, model.params=model.params, seasonal.periods=seasonal.periods, k.vector=k.vector, use.arma.errors=use.arma.errors, aux.model=aux.model, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj, ...)
		stopCluster(clus)
		##Choose the best model
		####Get the AICs
		aics <- numeric(nrow(control.array))
		for(i in 1:nrow(control.array)) {
			aics[i] <- models.list[[i]]$AIC
		}
		best.number <- which.min(aics)
		best.seasonal.model <- models.list[[best.number]]
		if(best.seasonal.model$AIC < best.model$AIC) {
			best.model <- best.seasonal.model
		}

	} else {
		for(box.cox in use.box.cox) {
			for(trend in use.trend) {
				for(damping in use.damped.trend) {
					if(all((model.params == c(box.cox, trend, damping)))) {
						new.model <- filterTBATSSpecifics(y, box.cox, trend, damping, seasonal.periods, k.vector, use.arma.errors, aux.model=aux.model, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj, ...)
					} else if(!((trend == FALSE) & (damping == TRUE))) {
						new.model <- filterTBATSSpecifics(y, box.cox, trend, damping, seasonal.periods, k.vector, use.arma.errors, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj, ...)
					}
					if(new.model$AIC < best.model$AIC) {
						best.model <- new.model
					}

				}
			}
		}
	}

	best.model$call <- match.call()
	attributes(best.model$fitted.values) <- attributes(best.model$errors) <- attributes(origy)
	best.model$y <- origy
	best.model$series <- seriesname
	best.model$method <- "TBATS"
	return(best.model)
}

######################################################################################################################################
parFilterTBATSSpecifics <- function(control.number, y, control.array, model.params, seasonal.periods, k.vector, use.arma.errors, aux.model=NULL, init.box.cox=NULL, bc.lower=0, bc.upper=1, biasadj=FALSE, ...) {
	box.cox <- control.array[control.number, 1]
	trend <- control.array[control.number, 2]
	damping <- control.array[control.number, 3]
	if(!all((model.params == c(box.cox, trend, damping)))) {
		first.model <- try(fitSpecificTBATS(y, use.box.cox=box.cox, use.beta=trend, use.damping=damping, seasonal.periods=seasonal.periods, k.vector=k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
			silent=TRUE)
	} else {
		first.model <- aux.model
	}

  if(is.element("try-error", class(first.model)))
  	first.model <- list(AIC=Inf)

	if(use.arma.errors) {
		suppressWarnings(arma <- try(auto.arima(as.numeric(first.model$errors), d=0, ...), silent=TRUE))
		if(!is.element("try-error",class(arma))) {
			p <- arma$arma[1]
			q <- arma$arma[2]
			if((p != 0) | (q != 0)) { #Did auto.arima() find any AR() or MA() coefficients?
				if(p != 0) {
					ar.coefs <- numeric(p)
				} else {
					ar.coefs <- NULL
				}
				if(q != 0) {
					ma.coefs <- numeric(q)
				} else {
					ma.coefs <- NULL
				}
				starting.params <- first.model$parameters

				second.model <- try(fitSpecificTBATS(y, use.box.cox=box.cox, use.beta=trend, use.damping=damping, seasonal.periods=seasonal.periods, k.vector=k.vector, ar.coefs=ar.coefs, ma.coefs=ma.coefs, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
					silent=TRUE)
				if(is.element("try-error", class(second.model)))
  				second.model <- list(AIC=Inf)
				if(second.model$AIC < first.model$AIC) {
					return(second.model)
				} else {
					return(first.model)
				}
			} else { #Else auto.arima() did not find any AR() or MA()coefficients
				return(first.model)
			}
		} else {
			return(first.model)
		}
	} else {
		return(first.model)
	}
}

#################################################################################################
parFitSpecificTBATS <- function(control.number, y, box.cox, trend, damping, seasonal.periods, k.control.matrix, init.box.cox=NULL, bc.lower=0, bc.upper=1, biasadj=FALSE) {
	k.vector <- k.control.matrix[control.number,]

	model <- try(fitSpecificTBATS(y, use.box.cox = box.cox, use.beta = trend, use.damping = damping, seasonal.periods = seasonal.periods, k.vector = k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
		silent=TRUE)
	if(is.element("try-error", class(model)))
		model <- list(AIC=Inf)
	return(model)
}

filterTBATSSpecifics <- function(y, box.cox, trend, damping, seasonal.periods, k.vector, use.arma.errors, aux.model=NULL, init.box.cox=NULL, bc.lower=0, bc.upper=1, biasadj=FALSE, ...) {
	if(is.null(aux.model)) {
		first.model <- try(fitSpecificTBATS(y, use.box.cox=box.cox, use.beta=trend, use.damping=damping, seasonal.periods=seasonal.periods, k.vector=k.vector, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
			silent=TRUE)
	} else {
		first.model <- aux.model
	}
	if(is.element("try-error",class(first.model)))
		first.model <- list(AIC=Inf)

	if(use.arma.errors) {
		suppressWarnings(arma <- try(auto.arima(as.numeric(first.model$errors), d=0, ...), silent=TRUE))
		if(!is.element("try-error",class(arma))) {
			p <- arma$arma[1]
			q <- arma$arma[2]
			if((p != 0) | (q != 0)) { #Did auto.arima() find any AR() or MA() coefficients?
				if(p != 0) {
					ar.coefs <- numeric(p)
				} else {
					ar.coefs <- NULL
				}
				if(q != 0) {
					ma.coefs <- numeric(q)
				} else {
					ma.coefs <- NULL
				}
				starting.params <- first.model$parameters

				second.model <- try(fitSpecificTBATS(y, use.box.cox=box.cox, use.beta=trend, use.damping=damping, seasonal.periods=seasonal.periods, k.vector=k.vector, ar.coefs=ar.coefs, ma.coefs=ma.coefs, init.box.cox=init.box.cox, bc.lower=bc.lower, bc.upper=bc.upper, biasadj=biasadj),
					silent=TRUE)
				if(is.element("try-error",class(second.model)))
					second.model <- list(AIC=Inf)

				if(second.model$AIC < first.model$AIC) {
					return(second.model)
				} else {
					return(first.model)
				}
			} else { #Else auto.arima() did not find any AR() or MA()coefficients
				return(first.model)
			}
		} else {
			return(first.model)
		}
	} else {
		return(first.model)
	}
}


makeSingleFourier <- function(j, m, T) {
	frier <- matrix(0, nrow=T, ncol=2)
	for(t in 1:T) {
		frier[t,1] <- cos((2*pi*j)/m)
		frier[t,2] <- sin((2*pi*j)/m)
	}
	return(frier)
}

calcFTest <- function(r.sse, ur.sse, num.restrictions, num.u.params, num.observations) {
	f.stat <- ((r.sse - ur.sse)/num.restrictions)/(r.sse/(num.observations - num.u.params))
	p.value <- pf(f.stat, num.restrictions, (num.observations - num.u.params),lower.tail=FALSE )
	return(p.value)
}

#' @rdname fitted.Arima
#' @export
fitted.tbats <- function(object, h=1, ...){
  if(h==1){
    return(object$fitted.values)
  }
  else{
    return(hfitted(object=object, h=h, FUN="tbats", ...))
  }
}

#' @export
print.tbats <- function(x, ...) {
	cat(as.character(x))
	cat("\n")
	cat("\nCall: ")
	print(x$call)
	cat("\nParameters")
  if(!is.null(x$lambda))
  {
    cat("\n  Lambda: ")
    cat(round(x$lambda,6))
  }
	cat("\n  Alpha: ")
	cat(x$alpha)
  if(!is.null(x$beta))
  {
    cat("\n  Beta: ")
    cat(x$beta)
    cat("\n  Damping Parameter: ")
    cat(round(x$damping.parameter,6))
  }
  if(!is.null(x$gamma.one.values))
  {
    cat("\n  Gamma-1 Values: ")
    cat(x$gamma.one.values)
  }
  if(!is.null(x$gamma.two.values))
  {
    cat("\n  Gamma-2 Values: ")
    cat(x$gamma.two.values)
  }
  if(!is.null(x$ar.coefficients))
  {
    cat("\n  AR coefficients: ")
    cat(round(x$ar.coefficients,6))
  }
  if(!is.null(x$ma.coefficients))
  {
    cat("\n  MA coefficients: ")
    cat(round(x$ma.coefficients,6))
  }
	cat("\n")
	cat("\nSeed States:\n")
	print(x$seed.states)
	cat("\nSigma: ")
	cat(sqrt(x$variance))
	cat("\nAIC: ")
	cat(x$AIC)
	cat("\n")
}

#' @rdname plot.bats
#' 
#' @examples
#' 
#' \dontrun{
#' fit <- tbats(USAccDeaths)
#' plot(fit)
#' autoplot(fit, range.bars = TRUE)}
#' 
#' @export
plot.tbats <- function (x, main="Decomposition by TBATS model", ...)
{
	out <- tbats.components(x)
	plot.ts(out, main=main, nc=1, ...)
}




#' Extract components of a TBATS model
#' 
#' Extract the level, slope and seasonal components of a TBATS model.
#' 
#' 
#' @param x A tbats object created by \code{\link{tbats}}.
#' @return A multiple time series (\code{mts}) object.
#' @author Slava Razbash and Rob J Hyndman
#' @seealso \code{\link{tbats}}.
#' @references De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011),
#' Forecasting time series with complex seasonal patterns using exponential
#' smoothing, \emph{Journal of the American Statistical Association},
#' \bold{106}(496), 1513-1527.
#' @keywords ts
#' @examples
#' 
#' \dontrun{
#' fit <- tbats(USAccDeaths, use.parallel=FALSE)
#' components <- tbats.components(fit)
#' plot(components)}
#' 
#' @export
tbats.components <- function(x)
{
  # Get original data, transform if necessary
  if (!is.null(x$lambda))
    y <- BoxCox(x$y, x$lambda)
  else
    y <- x$y
  # Compute matrices
  tau <- ifelse(!is.null(x$k.vector), 2*sum(x$k.vector), 0)
	w <- .Call("makeTBATSWMatrix", smallPhi_s = x$damping.parameter, kVector_s=as.integer(x$k.vector),
		arCoefs_s = x$ar.coefficients, maCoefs_s = x$ma.coefficients, tau_s=as.integer(tau), PACKAGE = "forecast")

  out <- cbind(observed=c(y), level=x$x[1,])
  if(!is.null(x$beta))
    out <- cbind(out, slope=x$x[2,])

  # Add seasonal components if they exist
  if(tau > 0)
  {
	  nonseas <- 2+!is.null(x$beta) # No. non-seasonal columns in out
  	nseas <- length(x$seasonal.periods) # No. seasonal periods
	  seas.states <- cbind(x$seed.states,x$x)[-(1:(1+!is.null(x$beta))),]
  	seas.states <- seas.states[,-ncol(seas.states)]
	  w <- w$w.transpose[,-(1:(1+!is.null(x$beta))),drop=FALSE]
  	w <- w[,1:tau,drop=FALSE]
  	j <- cumsum(c(1,2*x$k.vector))
  	for(i in 1:nseas)
    	out <- cbind(out, season=c(w[,j[i]:(j[i+1]-1),drop=FALSE] %*% seas.states[j[i]:(j[i+1]-1),]))
  	if(nseas > 1)
    	colnames(out)[nonseas + 1:nseas] <- paste("season",1:nseas,sep="")
  }

  # Add time series characteristics
  out <- ts(out)
  tsp(out) <- tsp(y)
  return(out)
}

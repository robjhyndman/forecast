fitPreviousTBATSModel <- function (y, model, biasadj=FALSE) {
  seasonal.periods <- model$seasonal.periods
  if (is.null(seasonal.periods) == FALSE) {
    seasonal.periods <- as.integer(sort(seasonal.periods))
  }
  #Get the parameters out of the param.vector
  paramz <- unParameteriseTBATS(model$parameters$vect, model$parameters$control)
  lambda <- paramz$lambda
  alpha <- paramz$alpha
  beta.v <- paramz$beta
  if(!is.null(beta.v)) {
    adj.beta <- 1
  } else {
    adj.beta <- 0
  }
  small.phi <- paramz$small.phi
  gamma.one.v <- paramz$gamma.one.v
  gamma.two.v <- paramz$gamma.two.v
  if(!is.null(paramz$ar.coefs)) {
    p <- length(paramz$ar.coefs)
    ar.coefs <- matrix(paramz$ar.coefs,nrow=1,ncol=p)
  } else {
    ar.coefs <- NULL
    p <- 0
  }
  if(!is.null(paramz$ma.coefs)) {
    q <- length(paramz$ma.coefs)
    ma.coefs <- matrix(paramz$ma.coefs, nrow=1, ncol=q)
  } else {
    ma.coefs <- NULL
    q <- 0
  }

  if(!is.null(seasonal.periods)) {
    tau <- as.integer(2*sum(model$k.vector))
    gamma.bold <- matrix(0,nrow=1,ncol=(2*sum(model$k.vector)))
  } else {
    tau <- as.integer(0)
    gamma.bold <- NULL
  }

  g <- matrix(0, nrow=((2*sum(model$k.vector))+1+adj.beta+p+q), ncol=1)
  if(p != 0) {
    g[(1+adj.beta+tau+1),1] <- 1
  }
  if(q != 0) {
    g[(1+adj.beta+tau+p+1),1] <- 1
  }

  y.touse <- y
  if (is.null(lambda) == FALSE) {
    y.touse <- BoxCox(y, lambda=lambda)
  }

  ##Calculate the variance:
  #1. Re-set up the matrices
  w <- .Call("makeTBATSWMatrix", smallPhi_s=small.phi, kVector_s=model$k.vector, arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, tau_s=tau, PACKAGE="forecast")
  if(!is.null(gamma.bold)) {
    .Call("updateTBATSGammaBold", gammaBold_s=gamma.bold, kVector_s=model$k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v, PACKAGE = "forecast")
  }
  .Call("updateTBATSGMatrix", g_s=g, gammaBold_s=gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE = "forecast")
  F <- makeTBATSFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, k.vector=model$k.vector, gamma.bold.matrix=gamma.bold, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
  .Call("updateFMatrix", F, small.phi, alpha, beta.v, gamma.bold, ar.coefs, ma.coefs, tau, PACKAGE="forecast")
  #2. Calculate!
  fitted.values.and.errors <- calcModel(y.touse, model$seed.states, F, g, w)
  e <- fitted.values.and.errors$e
  fitted.values <- fitted.values.and.errors$y.hat
  if (is.null(lambda) == FALSE) {
    fitted.values <- InvBoxCox(fitted.values, lambda=lambda)
    if(biasadj){
      fitted.values <- InvBoxCoxf(fitted.values, fvar = var(e), lambda = lambda)
    }
  }
  variance <- sum((e*e))/length(y)

  model.for.output <- model
  model.for.output$variance = variance
  model.for.output$fitted.values = c(fitted.values)
  model.for.output$errors=c(e)
  model.for.output$x=fitted.values.and.errors$x
  model.for.output$y=y
  return(model.for.output)

}


fitSpecificTBATS <- function(y, use.box.cox, use.beta, use.damping, seasonal.periods=NULL, k.vector=NULL, starting.params=NULL, x.nought=NULL, ar.coefs=NULL, ma.coefs=NULL, init.box.cox=NULL, bc.lower=0, bc.upper=1, biasadj=FALSE) {
  if(!is.null(seasonal.periods)) {
    seasonal.periods <- sort(seasonal.periods)
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
    alpha <- .1
    if(use.beta) {
      adj.beta <- 1
      beta.v <- 0.05
      b <- 0.00
      if(use.damping) {
        small.phi <- .999
      } else {
        small.phi <- 1
      }
    } else {
      adj.beta <- 0
      beta.v <- NULL
      b <- NULL
      small.phi <- NULL
      use.damping=FALSE
    }
    if(!is.null(seasonal.periods)) {
      gamma.one.v <- rep(0, length(k.vector))
      gamma.two.v <- rep(0, length(k.vector))
      s.vector <- numeric(2*sum(k.vector))
      k.vector <- as.integer(k.vector)
    } else {
      gamma.one.v <- NULL
      gamma.two.v <- NULL
      s.vector <- NULL
    }
    if(use.box.cox) {
      if(!is.null(init.box.cox)) {
        lambda<-init.box.cox
      } else {
        lambda <- BoxCox.lambda(y, lower=0, upper=1.5)
      }
      y.transformed <- BoxCox(y, lambda=lambda)
    } else { #the "else" is not needed at the moment
      lambda <- NULL
    }
  } else {
    paramz <- unParameteriseTBATS(starting.params$vect, starting.params$control)
    lambda <- paramz$lambda
    alpha <- paramz$alpha
    beta.v <- paramz$beta
    if(!is.null(beta.v)) {
      adj.beta <- 1
    } else {
      adj.beta <- 0
    }
    b <- 0
    small.phi <- paramz$small.phi
    gamma.one.v <- paramz$gamma.one.v
    gamma.two.v <- paramz$gamma.two.v
    if(!is.null(seasonal.periods)) {
      s.vector <- numeric(2*sum(k.vector))
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

  #Make the parameter vector  parameterise
  param.vector <- parameterise(alpha=alpha, beta.v=beta.v, small.phi=small.phi, gamma.v=cbind(gamma.one.v,gamma.two.v), lambda=lambda, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
  par.scale <- makeParscale(param.vector$control)
  if(!is.null(seasonal.periods)) {
    tau <- as.integer(2*sum(k.vector))
  } else {
    tau <- as.integer(0)
  }

  w <- .Call("makeTBATSWMatrix", smallPhi_s = small.phi, kVector_s = k.vector, arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, tau_s = tau, PACKAGE = "forecast")

  if(!is.null(seasonal.periods)) {
    gamma.bold <- matrix(0,nrow=1,ncol=(2*sum(k.vector)))
    .Call("updateTBATSGammaBold", gammaBold_s=gamma.bold, kVector_s=k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v, PACKAGE = "forecast")
  } else {
    gamma.bold <- NULL
  }
  g <- matrix(0, nrow=((2*sum(k.vector))+1+adj.beta+p+q), ncol=1)
  if(p != 0) {
    g[(1+adj.beta+tau+1),1] <- 1
  }
  if(q != 0) {
    g[(1+adj.beta+tau+p+1),1] <- 1
  }
  .Call("updateTBATSGMatrix", g_s=g, gammaBold_s=gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE = "forecast")
  F <- makeTBATSFMatrix(alpha=alpha, beta=beta.v, small.phi=small.phi, seasonal.periods=seasonal.periods, k.vector=k.vector, gamma.bold.matrix=gamma.bold, ar.coefs=ar.coefs, ma.coefs=ma.coefs)
  D <-  F - g %*% w$w.transpose

  ####
  #Set up environment
  opt.env <- new.env()
  assign("F", F, envir=opt.env)
  assign("w.transpose", w$w.transpose, envir=opt.env)
  assign("g", g, envir=opt.env)
  assign("gamma.bold", gamma.bold, envir=opt.env)
  assign("k.vector", k.vector, envir=opt.env)
  assign("y", matrix(y, nrow=1, ncol=length(y)), envir=opt.env)
  assign("y.hat", matrix(0, nrow=1, ncol=length(y)), envir=opt.env)
  assign("e", matrix(0, nrow=1, ncol=length(y)), envir=opt.env)
  assign("x", matrix(0, nrow=length(x.nought), ncol=length(y)),  envir=opt.env)

  ##Set up matrices to find the seed states
  if(use.box.cox) {
    y.transformed <- BoxCox(y, lambda=lambda)
    .Call("calcTBATSFaster",ys=matrix(y.transformed,nrow=1,ncol=length(y.transformed)), yHats=opt.env$y.hat, wTransposes=opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s=x.nought, PACKAGE="forecast")
    y.tilda <- opt.env$e
  } else {
    .Call("calcTBATSFaster",ys=opt.env$y, yHats=opt.env$y.hat, wTransposes=opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s=x.nought, PACKAGE="forecast")
    y.tilda <- opt.env$e
  }
  w.tilda.transpose <- matrix(0, nrow=length(y), ncol=ncol(w$w.transpose))
  w.tilda.transpose[1,] <- w$w.transpose
  w.tilda.transpose=.Call("calcWTilda", wTildaTransposes=w.tilda.transpose, Ds=D, PACKAGE = "forecast")
  #Remove the AR() and MA() bits if they exist
  if((p != 0) | (q != 0)) {
    end.cut <- ncol(w.tilda.transpose)
    start.cut <- end.cut-(p+q)+1
    w.tilda.transpose <- w.tilda.transpose[,-c(start.cut:end.cut)]

  }
  x.nought <- lm(t(y.tilda) ~ w.tilda.transpose - 1)$coefficients
  x.nought <- matrix(x.nought, nrow=length(x.nought), ncol=1)
  ##Replace the AR() and MA() bits if they exist
  if((p != 0) | (q != 0)) {
    arma.seed.states <- numeric((p+q))
    arma.seed.states <- matrix(arma.seed.states, nrow=length(arma.seed.states), ncol=1)
    x.nought <- rbind(x.nought, arma.seed.states)
  }

  ##Optimisation
  if(use.box.cox) {
    #Un-transform the seed states
    assign("x.nought.untransformed", InvBoxCox(x.nought, lambda=lambda), envir=opt.env)
    #Optimise the likelihood function
    optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodTBATS, method="Nelder-Mead",
      opt.env=opt.env, use.beta=use.beta, use.small.phi=use.damping,
      seasonal.periods=seasonal.periods, param.control=param.vector$control,
      p=p, q=q, tau=tau, bc.lower=bc.lower, bc.upper=bc.upper,
      control=list(maxit=(100*length(param.vector$vect)^2), parscale=par.scale))
    #Get the parameters out of the param.vector
    paramz <- unParameteriseTBATS(optim.like$par, param.vector$control)
    lambda <- paramz$lambda
    alpha <- paramz$alpha
    beta.v <- paramz$beta
    small.phi <- paramz$small.phi
    gamma.one.v <- paramz$gamma.one.v
    gamma.two.v <- paramz$gamma.two.v
    if(!is.null(paramz$ar.coefs)) {
      p <- length(paramz$ar.coefs)
      ar.coefs <- matrix(paramz$ar.coefs,nrow=1,ncol=p)
    } else {
      ar.coefs <- NULL
      p <- 0
    }
    if(!is.null(paramz$ma.coefs)) {
      q <- length(paramz$ma.coefs)
      ma.coefs <- matrix(paramz$ma.coefs, nrow=1, ncol=q)
    } else {
      ma.coefs <- NULL
      q <- 0
    }
    #Transform the seed states
    x.nought <- BoxCox(opt.env$x.nought.untransformed, lambda=lambda)



    ##Calculate the variance:
    #1. Re-set up the matrices
    w <- .Call("makeTBATSWMatrix", smallPhi_s=small.phi, kVector_s=k.vector, arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, tau_s=tau, PACKAGE="forecast")
    if(!is.null(gamma.bold)) {
      .Call("updateTBATSGammaBold", gammaBold_s=gamma.bold, kVector_s=k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v, PACKAGE = "forecast")
    }
    .Call("updateTBATSGMatrix", g_s=g, gammaBold_s=gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE = "forecast")
    .Call("updateFMatrix", F, small.phi, alpha, beta.v, gamma.bold, ar.coefs, ma.coefs, tau, PACKAGE="forecast")

    #2. Calculate!
    y.transformed <- BoxCox(y, lambda=lambda)
    fitted.values.and.errors <- calcModel(y.transformed, x.nought, F, g, w)
    e <- fitted.values.and.errors$e
    fitted.values <- InvBoxCox(fitted.values.and.errors$y.hat, lambda=lambda)
    if(biasadj){
      fitted.values <- InvBoxCoxf(fitted.values, fvar = var(e), lambda = lambda)
    }
    variance <- sum((e*e))/length(y)
    #e <- InvBoxCox(e, lambda=lambda)
    ee <- y-fitted.values

  } else { #else if we are not using the Box-Cox transformation
    #Optimise the likelihood function
    if(length(param.vector$vect) > 1) {
      optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformedTBATS, method="Nelder-Mead", opt.env=opt.env, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, param.control=param.vector$control, p=p, q=q, tau=tau, control=list(maxit=(100*length(param.vector$vect)^2), parscale=par.scale))
    } else {
      optim.like <- optim(par=param.vector$vect, fn=calcLikelihoodNOTransformedTBATS, method="BFGS", opt.env=opt.env, x.nought=x.nought, use.beta=use.beta, use.small.phi=use.damping, seasonal.periods=seasonal.periods, param.control=param.vector$control, p=p, q=q, tau=tau, control=list(parscale=par.scale))
    }

    #Get the parameters out of the param.vector
    paramz <- unParameteriseTBATS(optim.like$par, param.vector$control)
    lambda <- paramz$lambda
    alpha <- paramz$alpha
    beta.v <- paramz$beta
    small.phi <- paramz$small.phi
    gamma.one.v <- paramz$gamma.one.v
    gamma.two.v <- paramz$gamma.two.v
    if(!is.null(paramz$ar.coefs)) {
      p <- length(paramz$ar.coefs)
      ar.coefs <- matrix(paramz$ar.coefs,nrow=1,ncol=p)
    } else {
      ar.coefs <- NULL
      p <- 0
    }
    if(!is.null(paramz$ma.coefs)) {
      q <- length(paramz$ma.coefs)
      ma.coefs <- matrix(paramz$ma.coefs, nrow=1, ncol=q)
    } else {
      ma.coefs <- NULL
      q <- 0
    }

    ##Calculate the variance:
    #1. Re-set up the matrices
    w <- .Call("makeTBATSWMatrix", smallPhi_s=small.phi, kVector_s=k.vector, arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, tau_s=tau, PACKAGE="forecast")
    if(!is.null(gamma.bold)) {
      .Call("updateTBATSGammaBold", gammaBold_s=gamma.bold, kVector_s=k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v, PACKAGE = "forecast")
    }
    .Call("updateTBATSGMatrix", g_s=g, gammaBold_s=gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE = "forecast")
    .Call("updateFMatrix", F, small.phi, alpha, beta.v, gamma.bold, ar.coefs, ma.coefs, tau, PACKAGE="forecast")
    #2. Calculate!
    fitted.values.and.errors <- calcModel(y, x.nought, F, g, w)
    e <- fitted.values.and.errors$e
    fitted.values <- fitted.values.and.errors$y.hat
    variance <- sum((e*e))/length(y)
  }
  #Get the likelihood
  likelihood <- optim.like$value
  #Calculate the AIC
  aic <- likelihood+2*(length(param.vector$vect)+nrow(x.nought))


  #Make a list object
  model.for.output <- list(lambda=lambda, alpha=alpha, beta=beta.v, damping.parameter=small.phi, gamma.one.values=gamma.one.v, gamma.two.values=gamma.two.v, ar.coefficients=ar.coefs, ma.coefficients=ma.coefs, likelihood=likelihood, optim.return.code=optim.like$convergence, variance=variance, AIC=aic, parameters=list(vect=optim.like$par, control=param.vector$control), seed.states=x.nought, fitted.values=c(fitted.values), errors=c(e), x=fitted.values.and.errors$x, seasonal.periods=seasonal.periods, k.vector=k.vector, y=y, p=p, q=q)
  class(model.for.output) <- c("tbats","bats")
  return(model.for.output)
}



calcLikelihoodTBATS <- function(param.vector, opt.env, use.beta, use.small.phi, seasonal.periods, param.control, p=0, q=0, tau=0, bc.lower=0, bc.upper=1) {
  #param vector should be as follows: Box-Cox.parameter, alpha, beta, small.phi, gamma.vector, ar.coefs, ma.coefs
  #Put the components of the param.vector into meaningful individual variables
  paramz <- unParameteriseTBATS(param.vector, param.control)
  box.cox.parameter <- paramz$lambda
  alpha <- paramz$alpha
  beta.v <- paramz$beta
  small.phi <- paramz$small.phi
  gamma.one.v <- paramz$gamma.one.v
  gamma.two.v <- paramz$gamma.two.v
  ar.coefs <- paramz$ar.coefs
  ma.coefs <- paramz$ma.coefs
  if(!is.null(paramz$ar.coefs)) {
    p <- length(paramz$ar.coefs)
    ar.coefs <- matrix(paramz$ar.coefs,nrow=1,ncol=p)
  } else {
    ar.coefs <- NULL
    p <- 0
  }
  if(!is.null(paramz$ma.coefs)) {
    q <- length(paramz$ma.coefs)
    ma.coefs <- matrix(paramz$ma.coefs, nrow=1, ncol=q)
  } else {
    ma.coefs <- NULL
    q <- 0
  }
  x.nought <- BoxCox(opt.env$x.nought.untransformed, lambda=box.cox.parameter)


  .Call("updateWtransposeMatrix", wTranspose_s=opt.env$w.transpose, smallPhi_s=small.phi, tau_s=as.integer(tau), arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, p_s=as.integer(p), q_s=as.integer(q), PACKAGE = "forecast")

  if(!is.null(opt.env$gamma.bold)) {
    .Call("updateTBATSGammaBold", gammaBold_s=opt.env$gamma.bold, kVector_s=opt.env$k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v)
  }
  .Call("updateTBATSGMatrix", g_s=opt.env$g, gammaBold_s=opt.env$gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE="forecast")

  .Call("updateFMatrix", opt.env$F, small.phi, alpha, beta.v, opt.env$gamma.bold, ar.coefs, ma.coefs, tau, PACKAGE="forecast")

  mat.transformed.y <- BoxCox(opt.env$y, box.cox.parameter)
  n <- ncol(opt.env$y)


  .Call("calcTBATSFaster", ys=mat.transformed.y, yHats=opt.env$y.hat, wTransposes= opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s=x.nought, PACKAGE="forecast")

  ##
  ####
  ####################################################################


  log.likelihood <- n*log(sum(opt.env$e^2))-2*(box.cox.parameter-1)*sum(log(opt.env$y))
  if(is.na(log.likelihood)) # Not sure why this would occur
    return(Inf)


  assign("D", (opt.env$F - opt.env$g %*% opt.env$w.transpose), envir=opt.env)
  if(checkAdmissibility(opt.env, box.cox=box.cox.parameter, small.phi=small.phi, ar.coefs=ar.coefs, ma.coefs=ma.coefs, tau=sum(seasonal.periods), bc.lower=bc.lower, bc.upper=bc.upper)) {
    return(log.likelihood)
  } else {
    return(Inf)
  }
}

calcLikelihoodNOTransformedTBATS <- function(param.vector, opt.env, x.nought, use.beta, use.small.phi, seasonal.periods, param.control, p=0, q=0, tau=0) {
  #The likelihood function without the Box-Cox Transformation
  #param vector should be as follows: alpha, beta, small.phi, gamma.vector, ar.coefs, ma.coefs
  #Put the components of the param.vector into meaningful individual variables
  paramz <- unParameteriseTBATS(param.vector, param.control)
  box.cox.parameter <- paramz$lambda
  alpha <- paramz$alpha
  beta.v <- paramz$beta
  small.phi <- paramz$small.phi
  gamma.one.v <- paramz$gamma.one.v
  gamma.two.v <- paramz$gamma.two.v

  if(!is.null(paramz$ar.coefs)) {
    p <- length(paramz$ar.coefs)
    ar.coefs <- matrix(paramz$ar.coefs,nrow=1,ncol=p)
  } else {
    ar.coefs <- NULL
    p <- 0
  }

  if(!is.null(paramz$ma.coefs)) {
    q <- length(paramz$ma.coefs)
    ma.coefs <- matrix(paramz$ma.coefs, nrow=1, ncol=q)
  } else {
    ma.coefs <- NULL
    q <- 0
  }

  .Call("updateWtransposeMatrix", wTranspose_s=opt.env$w.transpose, smallPhi_s=small.phi, tau_s=as.integer(tau), arCoefs_s=ar.coefs, maCoefs_s=ma.coefs, p_s=as.integer(p), q_s=as.integer(q), PACKAGE = "forecast")

  if(!is.null(opt.env$gamma.bold)) {
    .Call("updateTBATSGammaBold", gammaBold_s=opt.env$gamma.bold, kVector_s=opt.env$k.vector, gammaOne_s=gamma.one.v, gammaTwo_s=gamma.two.v)
  }

  .Call("updateTBATSGMatrix", g_s=opt.env$g, gammaBold_s=opt.env$gamma.bold, alpha_s=alpha, beta_s=beta.v, PACKAGE="forecast")


  .Call("updateFMatrix", opt.env$F, small.phi, alpha, beta.v, opt.env$gamma.bold, ar.coefs, ma.coefs, tau, PACKAGE="forecast")

  n <- ncol(opt.env$y)

  .Call("calcTBATSFaster", ys=opt.env$y, yHats=opt.env$y.hat, wTransposes= opt.env$w.transpose, Fs=opt.env$F, xs=opt.env$x, gs=opt.env$g, es=opt.env$e, xNought_s=x.nought, PACKAGE="forecast")
  ##
  ####
  ####################################################################


  log.likelihood <- n*log(sum(opt.env$e*opt.env$e))
  if(is.na(log.likelihood)) # Not sure why this would occur
    return(Inf)

  assign("D", (opt.env$F - opt.env$g %*% opt.env$w.transpose), envir=opt.env)


  if(checkAdmissibility(opt.env=opt.env, box.cox=NULL, small.phi=small.phi, ar.coefs=ar.coefs, ma.coefs=ma.coefs, tau=tau)) {
    return(log.likelihood)
  } else {
    return(Inf)
  }
}


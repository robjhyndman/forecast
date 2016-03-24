####################################################################
## Double Seasonal Holt Winters method as per Taylor (2003)
## Periods must be nested. 
## y can be an msts object, or periods can be passed explicitly.
####################################################################

dshw <- function(y, period1=NULL, period2=NULL, h=2*max(period1,period2), 
  alpha=NULL, beta=NULL, gamma=NULL, omega=NULL, phi=NULL, lambda=NULL, biasadj=FALSE, 
  armethod=TRUE, model = NULL)
{
  if(min(y,na.rm=TRUE) <= 0)
    stop("dshw not suitable when data contain zeros or negative numbers")
  if (!is.null(model) && model$method == "DSHW") {
    period1 <- model$period1
    period2 <- model$period2
  } else if(any(class(y) == "msts") & (length(attr(y, "msts")) == 2)) {
	  period1 <- as.integer(sort(attr(y, "msts"))[1])
	  period2 <- as.integer(sort(attr(y, "msts"))[2])
  } else if(is.null(period1) | is.null(period2)) {
	  stop("Error in dshw(): y must either be an msts object with two seasonal periods OR the seasonal periods should be specified with period1= and period2=")
  } else {
    if(period1 > period2)
    {
      tmp <- period2
      period2 <- period1
      period1 <- tmp
    }
  }
  if(any(class(y) != "msts"))
    y <- msts(y, c(period1, period2))
	
  if(!armethod)
    phi <- 0
  
  if(period1 < 1 | period1 == period2)
    stop("Inappropriate periods") 
  ratio <- period2/period1
  if(ratio-trunc(ratio) > 1e-10)
    stop("Seasonal periods are not nested")

  if (!is.null(model)) {
    lambda <- model$model$lambda
  }

  if (!is.null(lambda))
  {
    origy <- y
    y <- BoxCox(y, lambda)
  }

  if (!is.null(model)) {
    pars <- model$model
    alpha <- pars$alpha
    beta <- pars$beta
    gamma <- pars$gamma
    omega <- pars$omega
    phi <- pars$phi
  } else {
    pars <- rep(NA,5)
    if(!is.null(alpha))
      pars[1] <- alpha
    if(!is.null(beta))
      pars[2] <- beta
    if(!is.null(gamma))
      pars[3] <- gamma
    if(!is.null(omega))
      pars[4] <- omega
    if(!is.null(phi))
      pars[5] <- phi
  }

  # Estimate parameters
  if(sum(is.na(pars)) > 0)
  {
    pars <- par_dshw(y,period1,period2,pars)
    alpha <- pars[1]
    beta <- pars[2]
    gamma <- pars[3]
    omega <- pars[4]
    phi <- pars[5]
  }

  ## Allocate space
  n <- length(y)
  yhat <- numeric(n)

  ## Starting values
  I <- seasindex(y,period1)
  wstart <- seasindex(y,period2)
  wstart <- wstart / rep(I,ratio)
  w <- wstart
  x <- c(0,diff(y[1:period2]))
  t <- t.start <- mean(((y[1:period2]- y[(period2+1):(2*period2)])/period2 ) + x )/2
  s <- s.start <- (mean(y[1:(2*period2)])-(period2+0.5)*t)

  ## In-sample fit
  for(i in 1: n)
  {
    yhat[i] <- (s+t) * I[i]*w[i]
    snew <- alpha*(y[i]/(I[i]*w[i]))+(1-alpha)*(s+t)
    tnew <- beta*(snew-s)+(1-beta)*t
    I[i+period1] <- gamma*(y[i]/(snew*w[i])) + (1-gamma)*I[i]
    w[i+period2] <- omega*(y[i]/(snew*I[i])) + (1-omega)*w[i]
    s <- snew
    t <- tnew
  }

	# Forecasts
  fcast <- (s + (1:h)*t) * rep(I[n+(1:period1)],h/period1 + 1)[1:h] * rep(w[n+(1:period2)],h/period2 + 1)[1:h]
  fcast <- ts(fcast,frequency=frequency(y),start=tsp(y)[2]+1/tsp(y)[3])
  
  # Calculate MSE and MAPE
  yhat <- ts(yhat)
  tsp(yhat) <- tsp(y)
  yhat <- msts(yhat, c(period1, period2))
  e <- y - yhat
  e <- msts(e, c(period1, period2))
  if(armethod)
	{
		yhat <- yhat + phi * c(0,e[-n])
		e <- y - yhat
	  fcast <- fcast + phi^(1:h)*e[n]
	}
	mse <- mean(e^2)
	mape <- mean(abs(e)/y)*100
  

  end.y <- end(y)
  if(end.y[2] == frequency(y)) {
	  end.y[1] <- end.y[1]+1
	  end.y[2] <- 1
  } else {
	  end.y[2] <- end.y[2]+1
  }
  
  fcast <- msts(fcast, c(period1, period2))
  
  if(!is.null(lambda))
  {
    y <- origy
    fcast <- InvBoxCox(fcast,lambda)
    if(biasadj){
      fcast <- InvBoxCoxf(x = fcast, fvar = var(e), lambda = lambda)
    }
    #Does this also need a biasadj backtransform?
    yhat <- InvBoxCox(yhat,lambda)
  }

  return(structure(list(mean=fcast,method="DSHW",x=y,residuals=e,fitted=yhat,
              model=list(mape=mape,mse=mse,alpha=alpha,beta=beta, gamma=gamma,omega=omega,phi=phi,
                         lambda = lambda, l0=s.start,b0=t.start,s10=wstart,s20=I), period1 = period1,
                         period2 = period2),class="forecast"))

}

### Double Seasonal Holt-Winters smoothing parameter optimization

par_dshw <- function(y, period1, period2, pars)
{
  start <- c(0.1,0.01,0.001,0.001,0.0)[is.na(pars)]
  out <- optim(start, dshw.mse, y=y, period1=period1, period2=period2, pars=pars)
  pars[is.na(pars)] <- out$par
  return(pars)
}

dshw.mse <- function(par, y, period1, period2, pars)
{
  pars[is.na(pars)] <- par
  if(max(pars) > 0.99 | min(pars) < 0 | pars[5] > .9)
    return(1e20)
  else
    return(dshw(y, period1, period2, h=1, pars[1], pars[2], pars[3], pars[4], pars[5], armethod=(abs(pars[5]) >1e-7))$model$mse)
}

### Calculating seasonal indexes
seasindex <- function(y,p)
{
  #require(zoo)
  n <- length(y)
  n2 <- 2*p
  shorty <- y[1:n2]
  average <- numeric(n)
  simplema <- zoo::rollmean.default(shorty, p)
  if (identical(p%%2,0)) # Even order
  {
    centeredma <- zoo::rollmean.default(simplema[1:(n2-p+1)],2)
    average[p/2 + 1:p] <- shorty[p/2 + 1:p]/centeredma[1:p]
    si <- average[c(p+(1:(p/2)),(1+p/2):p)]
  }
  else # Odd order
  {
    average[(p-1)/2 + 1:p] <- shorty[(p-1)/2 + 1:p]/simplema[1:p]
    si <- average[c(p+(1:((p-1)/2)),(1+(p-1)/2):p)]
  } 
  return(si)
}

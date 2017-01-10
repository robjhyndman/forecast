# Mean forecast
meanf <- function(y,h=10,level=c(80,95),fan=FALSE, lambda=NULL, biasadj=FALSE, x=y)
{
  n <- length(x)
  if(!is.null(lambda))
  {
    origx <- x
    x <- BoxCox(x,lambda)
  }
  meanx <- mean(x, na.rm=TRUE)
  fits <- rep(meanx,length(x))
  res <- x-fits
  f <- rep(meanx,h)
  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nconf <- length(level)
  lower <- upper <- matrix(NA,nrow=h,ncol=nconf)
  s <- sd(x,na.rm=TRUE)
  for(i in 1:nconf)
  {
    if(n > 1)
      tfrac <- qt( 0.5 - level[i]/200, n-1)
    else
      tfrac <- -Inf
    w <- -tfrac * s*sqrt(1+1/n)
    lower[,i] <- f-w
    upper[,i] <- f+w
  }
  colnames(lower) <- colnames(upper) <- paste(level,"%",sep="")
  if(is.ts(x))
  {
    fits <- ts(fits)
    res <- ts(res)
    tsp(fits) <- tsp(res) <- tsp(x)
    freq <- frequency(x)
    f <- ts(f,start=tsp(x)[2]+1/freq,frequency=freq)
    lower <- ts(lower,start=tsp(x)[2]+1/freq,frequency=freq)
    upper <- ts(upper,start=tsp(x)[2]+1/freq,frequency=freq)
  }

  if(!is.null(lambda))
  {
    fits <- InvBoxCox(fits,lambda)
    x <- origx
    f <- InvBoxCox(f, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    lower <- InvBoxCox(lower,lambda)
    upper <- InvBoxCox(upper,lambda)
  }

  out <- list(method="Mean",level=level,x=x,series=deparse(substitute(y)),mean=f,lower=lower,upper=upper,
    model=list(mu=f[1],mu.se=s/sqrt(length(x)),sd=s), lambda=lambda, fitted=fits, residuals=res)
  out$model$call <- match.call()

  return(structure(out,class="forecast"))
}


BoxCox <- function(x,lambda)
{
  if(lambda < 0)
   x[x < 0] <- NA
  if(lambda==0)
    out <- log(x)
  else
    out <- (sign(x)*abs(x)^lambda - 1)/lambda
  if(!is.null(colnames(x)))
    colnames(out) <- colnames(x)
  return(out)
}

InvBoxCox <- function(x, lambda, biasadj=FALSE, fvar=NULL)
{
  if(lambda < 0)
    x[x > -1/lambda] <- NA
  if(lambda==0)
    out <- exp(x)
  else
  {
    xx <- x*lambda + 1
    out <- sign(xx)*abs(xx)^(1/lambda)
  }
  if(!is.null(colnames(x)))
    colnames(out) <- colnames(x)

  if(is.null(biasadj)){
    biasadj <- attr(lambda, "biasadj")
  }
  if(!is.logical(biasadj)){
    warning("biasadj information not found, defaulting to FALSE.")
    biasadj <- FALSE
  }
  if(biasadj){
    if(is.null(fvar)){
      stop("fvar must be provided when biasadj=TRUE")
    }
    if(is.list(fvar)){ #Create fvar from forecast interval
      level <- max(fvar$level)
    if(NCOL(fvar$upper)>1 & NCOL(fvar$lower)){
        i <- match(level,fvar$level)
        fvar$upper <- fvar$upper[,i]
        fvar$lower <- fvar$lower[,i]
      }
      if(level>1)
        level <- level/100
      level <- mean(c(level,1))
      #Note: Use BoxCox transformed upper and lower values
      fvar <- ((fvar$upper-fvar$lower)/stats::qnorm(level)/2)^2
    }
    if(is.matrix(fvar)){
      fvar <- diag(fvar)
    }
    out <- out * (1 + 0.5*fvar*(1-lambda)/(out)^(2*lambda))
  }
  return(out)
}

#Deprecated
InvBoxCoxf <- function(x=NULL, fvar=NULL, lambda=NULL){
  message("Deprecated, use InvBoxCox instead")
  if(is.null(lambda))
    stop("Must specify lambda using lambda=numeric(1)")
  if(is.null(fvar))
  {
    level <- max(x$level)
    if(NCOL(x$upper)>1 & NCOL(x$lower))
    {
      i <- match(level,x$level)
      x$upper <- x$upper[,i]
      x$lower <- x$lower[,i]
    }
    if(level>1)
      level <- level/100
    level <- mean(c(level,1))
    #Note: Use BoxCox transformed upper and lower values
    fvar <- ((x$upper-x$lower)/stats::qnorm(level)/2)^2
  }
  else
    x <- list(mean=x)
  if("matrix"%in%class(fvar))
    fvar <- diag(fvar)

  return(x$mean * (1 + 0.5*fvar*(1-lambda)/(x$mean)^(2*lambda)))
}

forecast.StructTS <- function(object,h=ifelse(object$coef["epsilon"]>1e-10, 2*object$xtsp[3], 10),level=c(80,95),fan=FALSE,lambda=NULL,biasadj=NULL,...)
{
  x <- object$data
  pred <- predict(object,n.ahead=h)
  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nint <- length(level)
  upper <- lower <- matrix(NA,ncol=nint,nrow=length(pred$pred))
  for(i in 1:nint)
  {
    qq <- qnorm(0.5*(1+level[i]/100))
    lower[,i] <- pred$pred - qq*pred$se
    upper[,i] <- pred$pred + qq*pred$se
  }
  colnames(lower) = colnames(upper) = paste(level,"%",sep="")
  if(is.element("seas",names(object$coef)))
    method <- "Basic structural model"
  else if(is.element("slope",names(object$coef)))
    method <- "Local linear structural model"
  else
    method <- "Local level structural model"

  fits <- x - residuals(object)
  if(!is.null(lambda))
  {
    fits <- InvBoxCox(fits,lambda)
    x <- InvBoxCox(x,lambda)
    pred$pred <- InvBoxCox(pred$pred, lambda, biasadj, list(level=level, upper=upper, lower=lower))
    lower <- InvBoxCox(lower,lambda)
    upper <- InvBoxCox(upper,lambda)
  }


  return(structure(list(method=method, model=object, level=level, mean=pred$pred,
    lower=lower, upper=upper, x=x, series=object$series, fitted=fits, residuals=residuals(object)),
    class="forecast"))
}

forecast.HoltWinters <- function(object, h=ifelse(frequency(object$x)>1,2*frequency(object$x),10),
  level=c(80,95), fan=FALSE, lambda=NULL, biasadj=NULL,...)
{
  x <- object$x
  if(!is.null(object$exponential))
    if(object$exponential)
      stop("Forecasting for exponential trend not yet implemented.")

  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nint <- length(level)

  pred <- predict(object,n.ahead=h,prediction.interval=TRUE,level=level[1]/100)
  pmean <- pred[,1]
  upper <- lower <- matrix(NA,ncol=nint,nrow=length(pred[,1]))
  se <- (pred[,2]-pred[,3])/(2*qnorm(0.5*(1+level[1]/100)))
  for(i in 1:nint)
  {
    qq <- qnorm(0.5*(1+level[i]/100))
    lower[,i] <- pmean - qq*se
    upper[,i] <- pmean + qq*se
  }
  colnames(lower) = colnames(upper) = paste(level,"%",sep="")


  if(!is.null(lambda))
  {
    fitted <- InvBoxCox(object$fitted[,1],lambda)
    x <- InvBoxCox(x,lambda)
    pmean <- InvBoxCox(pmean, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    lower <- InvBoxCox(lower,lambda)
    upper <- InvBoxCox(upper,lambda)
  }
  else
    fitted <- object$fitted[,1]

  # Pad fitted values with NAs
  nf <- length(fitted)
  n <- length(x)
  fitted <- ts(c(rep(NA,n-nf), fitted))
  tsp(fitted) <- tsp(object$x)

  return(structure(list(method="HoltWinters", model=object, level=level,
    mean=pmean, lower=lower, upper=upper, x=x, series=deparse(object$call$x),
    fitted=fitted, residuals=x-fitted),
    class="forecast"))
}


## CROSTON

croston <- function(y,h=10,alpha=0.1,x=y)
{
  if(sum(x<0) > 0)
    stop("Series should not contain negative values")
  out <- croston2(x,h,alpha)
  out$x <- x
  if(!is.null(out$fitted))
    out$residuals <- x-out$fitted
  out$method <- "Croston's method"
  out$series <- deparse(substitute(y))
  return(structure(out,class="forecast"))
}

croston2 <- function(x,h=10,alpha=0.1,nofits=FALSE)
{
  x <- as.ts(x)
  y <- x[x>0]
  tsp.x <- tsp(x)
  freq.x <- tsp.x[3]
  start.f <- tsp.x[2] + 1/freq.x
  if(length(y)==0) # All historical values are equal to zero
  {
    fc <- ts(rep(0,h), start=start.f, frequency=freq.x)
    if(nofits)
      return(fc)
    else
      return(list(mean=fc, fitted=ts(x*0, start=tsp.x[1], frequency=freq.x)))
  }
  tt <- diff(c(0,(1:length(x))[x>0])) # Times between non-zero observations
  if(length(y)==1 & length(tt)==1) # Only one non-zero observation
  {
    y.f <- list(mean=ts(rep(y,h), start=start.f, frequency=freq.x))
    p.f <- list(mean=ts(rep(tt,h), start=start.f, frequency=freq.x))
  }
  else if(length(y)<=1 | length(tt)<=1) # length(tt)==0 but length(y)>0. How does that happen?
    return(list(mean=ts(rep(NA,h), start=start.f, frequency=freq.x)))
  else
  {
    y.f <- ses(y,alpha=alpha,initial="simple",h=h, PI=FALSE)
    p.f <- ses(tt,alpha=alpha,initial="simple",h=h, PI=FALSE)
  }
  ratio <- ts(y.f$mean/p.f$mean,start=start.f, frequency = freq.x)
  if(nofits)
    return(ratio)
  else
  {
    n <- length(x)
    fits <- x*NA
    if(n > 1)
    {
      for(i in 1:(n-1))
        fits[i+1] <- croston2(x[1:i],h=1,alpha=alpha,nofits=TRUE)
    }
    fits <- ts(fits)
    tsp(fits) <- tsp.x
    return(list(mean = ratio, fitted = fits, model=list(demand=y.f,period=p.f)))
  }
}


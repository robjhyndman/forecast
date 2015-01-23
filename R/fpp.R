# Replacement for the acf() function.
Acf <- function(x, lag.max=NULL, type=c("correlation","partial"), plot=TRUE, main=NULL, ylim=NULL, 
  na.action=na.contiguous, ...)
{
  type <- match.arg(type)
  if(is.null(main))
    main <- paste("Series:",deparse(substitute(x)))
  if (is.null(lag.max))
    lag.max <- max(floor(10 * log10(length(x))), 2*frequency(x))
  lag.max <- min(lag.max, length(x) - 1)
  if (lag.max < 0)
    stop("'lag.max' must be at least 0")
  junk1 <- acf(c(x), lag.max=lag.max, type=type, plot=FALSE, na.action=na.action, ...)
  junk1$series <- deparse(substitute(x))
  if(!plot)
    return(junk1)
  if(type=="correlation")
    junk1$acf[1, 1, 1] <- 0
  if(is.null(ylim))
  {
    ylim <- c(-1, 1) * 3/sqrt(length(x))
    ylim <- range(ylim, junk1$acf[,1,1])
  }
  plot(junk1, ylim = ylim, xlim = c(1, dim(junk1$acf)[1]-(type=="correlation")), xaxt="n", main=main, ...)

  # Make nice horizontal axis
  nlags <- dim(junk1$acf)[1]
  freqx <- frequency(x)
  if(freqx==1)
  {
    if(nlags <= 16)
      axis(1, at=(1:(nlags-1)), ...)
    else
      axis(1, ...)
  }
  else 
  {
    # Compute number of seasonal periods
    np <- nlags/freqx
    if(nlags <= 16)
    {
      axis(1, at=(1:(nlags-1)),tcl=-0.2,...)
      axis(1, at=freqx*(1:np), labels=FALSE, tcl=-0.6, ...)
    }
    else if(np <= 16)
      axis(1,at=freqx*(1:np), ...)
    else
      axis(1, ...)
  }
  if(type=="correlation")
    junk1$acf[1, 1, 1] <- 1
  return(invisible(junk1))
}

Pacf <- function (x, main=NULL, ...)
{
  if(is.null(main))
    main <- paste("Series:",deparse(substitute(x)))
  Acf(x, type="partial", main=main, ...)
}


CV <- function(obj)
{
  n <- length(obj$residuals)
  k <- extractAIC(obj)[1]-1 # number of predictors (constant removed)
  aic <- extractAIC(obj)[2]+2 # add 2 for the variance estimate
  aicc <- aic + 2*(k+2)*(k+3)/(n-k-3)
  bic <- aic + (k+2)*(log(n)-2)
  cv <- mean((residuals(obj)/(1-hatvalues(obj)))^2, na.rm=TRUE)
  adjr2 <- summary(obj)$adj
  out <- c(cv,aic,aicc,bic,adjr2)
  names(out) <- c("CV","AIC","AICc","BIC","AdjR2")
  return(out)
}



ma <- function(x,order,centre=TRUE)
{
  tt <- 1:length(x)
  if(order%%2) #odd
  {
    temp1 <- ts(ksmooth(tt,x, x.points=tt,bandwidth = order-1)$y)
    j <- trunc(order/2)
    temp1[c(1:j,length(x)-(1:j)+1)] <- NA
  }
  else
  {
    temp1 <- ts(ksmooth(tt,x, x.points=tt+0.5,bandwidth = order-1)$y)
    j <- trunc(order/2)
    temp1[c(1:(j-1),length(x)-(1:j)+1)] <- NA
    if(centre)
    {
      temp2 <- ksmooth(tt,x, x.points=tt-0.5,bandwidth = order-1)$y
      temp2[c(1:j,length(x)-(1:(j-1))+1)] <- NA
      temp1 <- ts((temp1+temp2)/2)
    }
  }
  tsp(temp1) <- tsp(x)
  return(temp1)
}

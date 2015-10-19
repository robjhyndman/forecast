# Replacement for the acf() function.
Acf <- function(x, lag.max=NULL, type=c("correlation","partial"), plot=TRUE, main=NULL,
  xlim=NULL, ylim=NULL, xlab="Lag", ylab=NULL, na.action=na.contiguous, ...)
{
  type <- match.arg(type)
  if(is.null(main))
    main <- paste("Series:",deparse(substitute(x)))
  if (is.null(lag.max))
    lag.max <- max(floor(10 * log10(length(x))), 2*frequency(x))
  lag.max <- min(lag.max, length(x) - 1)
  if (lag.max < 0)
    stop("'lag.max' must be at least 0")
  junk1 <- stats::acf(c(x), lag.max=lag.max, type=type, plot=FALSE, na.action=na.action, ...)
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
  if(is.null(xlim))
  {
    xlim <- c(1, dim(junk1$acf)[1]-(type=="correlation"))
  }
  plot(junk1, ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, xaxt="n", main=main, ...)

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

kappa <- function(x)
{
  k <- rep(0,length(x))
  x <- abs(x)
  k[x <= 1] <- 1
  k[x > 1 & x <= 2] <- 2-x[x>1 & x<=2]
  return(k)
}

# McMurray-Politis estimate of ACF

wacf <- function (x, lag.max = length(x)-1)
{
  n <- length(x)
  lag.max <- min(lag.max, n-1)
  if (lag.max < 0)
    stop("'lag.max' must be at least 0")

  # Standard estimator
  acfest <- stats::acf(c(x), lag.max = lag.max, plot = FALSE, na.action = na.contiguous)
  acfest$series <- deparse(substitute(x))

  # Taper estimates
  s <- 1:length(acfest$acf[,,1])
  upper <- 2*sqrt(log(n, 10)/n)
  ac <- abs(acfest$acf[,,1])
  # Find l: ac < upper for 5 consecutive lags
  j <- (ac < upper)
  l <- 0
  k <- 1
  N <- length(j)-4
  while(l < 1 & k <= N)
  {
    if(all(j[k:(k+4)]))
      l <- k
    else
      k <- k+1
  }
  acfest$acf[,,1] <- acfest$acf[,,1] * kappa(s/l)
  # End of Tapering

  # Now do some shrinkage towards white noise using eigenvalues
  # Construct covariance matrix
  gamma <- acfest$acf[,,1]
  s <- length(gamma)
  Gamma <- matrix(1, s, s)
  d <- row(Gamma) - col(Gamma)
  for(i in 1:(s-1))
    Gamma[d==i | d==(-i)] <- gamma[i+1]
  # Compute eigenvalue decomposition
  ei <- eigen(Gamma)
  # Shrink eigenvalues
  d <- pmax(ei$values, 20/n)
  # Construct new covariance matrix
  Gamma2 <- ei$vectors %*% diag(d) %*% t(ei$vectors)
  Gamma2 <- Gamma2/mean(d)
  # Estimate new ACF
  d <- row(Gamma2) - col(Gamma2)
  for(i in 2:s)
    gamma[i] <- mean(Gamma2[d==(i-1)])
  acfest$acf[,,1] <- gamma
  ############### end of shrinkage

  return(acfest)
}

# Find tapered PACF using LD recursions

wpacf  <- function(x, lag.max=length(x)-1)
{
  # Compute pacf as usual, just to set up structure
  out <- Pacf(x, lag.max=lag.max, plot=FALSE)
  # Compute acf using tapered estimate
  acvf <- wacf(x, lag.max=lag.max)$acf[,,1]

  # Durbin-Levinson recursions
  # Modified from http://faculty.washington.edu/dbp/s519/R-code/LD-recursions.R
  p <- length(acvf) - 1
  phis <- acvf[2]/acvf[1]
  pev  <- rep(acvf[1],p+1)
  pacf <- rep(phis,p)
  pev[2] <- pev[1]*(1-phis^2)
  if(p > 1)
  {
    for(k in 2:p)
    {
      old.phis <- phis
      phis <- rep(0,k)
      ## compute kth order pacf (reflection coefficient)
      phis[k] <- (acvf[k+1] - sum(old.phis*acvf[k:2]))/pev[k]
      phis[1:(k-1)] <- old.phis - phis[k]*rev(old.phis)
      pacf[k]  <- phis[k]
      pev[k+1] <- pev[k]*(1-phis[k]^2)
      #if(abs(pacf[k]) > 1)
      #  warning("PACF larger than 1 in absolute value")
    }
  }
  out$acf[,,1] <- pacf

  return(out)
}


# Function to produce new style plot of ACF or PACF with CI
# x = time series
taperedacf <- function(x, lag.max=NULL, type=c("correlation","partial"),
  plot=TRUE, calc.ci=TRUE, level=95, nsim=100, xlim=NULL, ylim=NULL,
  xlab="Lag",  ylab=NULL, ...)
{
  type <- match.arg(type)

  if (is.null(lag.max))
    lag.max <- max(floor(20 * log10(length(x))), 4*frequency(x))
  lag <- min(lag.max, length(x)-1)

  if(type=="correlation")
    z <- wacf(x, )$acf[2:(lag+1),,1]
  else
    z <- wpacf(x, )$acf[1:lag,,1]
  out <- list(z=z, lag=lag, type=type, x=x)

  if(calc.ci)
  {
    # Get confidence intervals for plots
    bootsim <- lpb(x, nsim=nsim)
    s1 <- matrix(0, nrow=lag, ncol=nsim)
    if(type=="correlation")
    {
      for(i in 1:nsim)
        s1[,i] <- wacf(bootsim[,i])$acf[2:(lag+1),,1]
    }
    else
    {
      for(i in 1:nsim)
        s1[,i] <- wpacf(bootsim[,i])$acf[1:lag,,1]
    }
    prob <- (100-level)/200
    out$upper <- apply(s1, 1, quantile, prob=1-prob)
    out$lower <- apply(s1, 1, quantile, prob=prob)
  }
  out <- structure(out, class="mpacf")

  if(!plot)
    return(out)
  else
  {
    if(is.null(ylab))
      ylab <- ifelse(type=="partial","PACF","ACF")

    plot(out, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
    return(invisible(out))
  }
  return(out)
}


taperedpacf <- function (x, ...)
{
  taperedacf(x, type="partial", ...)
}


plot.mpacf <- function(object, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
{
  lagx <- 1:object$lag

  if(is.null(xlim))
    xlim <- c(1,object$lag)
  if(is.null(ylim))
    ylim <- range(object$z, object$upper, object$lower)

  plot(lagx, object$z, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, xaxt="n", ...)

  grid(col=gray(.80), nx=NA, ny=NULL, lty=1)
  abline(h=0, col=gray(.4))
  if(frequency(object$x) > 1)
  {
    axis(1, at=(0:100)*frequency(object$x))
    for(i in 1:100)
      abline(v=(i-1)*frequency(object$x), lty=1, col=gray(0.80))
  }
  else
  {
    axis(1)
    grid(col=gray(.80),ny=NA,lty=1)
  }
  if(!is.null(object$lower))
  {
    for(j in 1:object$lag)
    {
      polygon(lagx[j] + c(-0.55,0.55,0.55,-0.55), c(rep(object$lower[j],2),rep(object$upper[j],2)),
        col=gray(0.60), border=FALSE)
    }
#    polygon(c(lagx,rev(lagx)),c(object$lower,rev(object$upper)),col=gray(.60),border=FALSE)
  }
  lines(lagx, object$z, lwd=1.5)
  j <- (object$lower<0 & object$upper>0)
  points(lagx[j], object$z[j], pch=1, cex=0.5)
  points(lagx[!j], object$z[!j], pch=19)
}

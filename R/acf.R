# Replacement for the acf() function.


#' (Partial) Autocorrelation and Cross-Correlation Function Estimation
#'
#' The function \code{Acf} computes (and by default plots) an estimate of the
#' autocorrelation function of a (possibly multivariate) time series. Function
#' \code{Pacf} computes (and by default plots) an estimate of the partial
#' autocorrelation function of a (possibly multivariate) time series. Function
#' \code{Ccf} computes the cross-correlation or cross-covariance of two
#' univariate series.
#'
#' The functions improve the \code{\link[stats]{acf}},
#' \code{\link[stats]{pacf}} and \code{\link[stats]{ccf}} functions. The main
#' differences are that \code{Acf} does not plot a spike at lag 0 when
#' \code{type=="correlation"} (which is redundant) and the horizontal axes show
#' lags in time units rather than seasonal units.
#'
#' The tapered versions implement the ACF and PACF estimates and plots
#' described in Hyndman (2015), based on the banded and tapered estimates of
#' autocovariance proposed by McMurry and Politis (2010).
#'
#' @param x a univariate or multivariate (not Ccf) numeric time series object
#' or a numeric vector or matrix.
#' @param y a univariate numeric time series object or a numeric vector.
#' @param lag.max maximum lag at which to calculate the acf. Default is
#' $10*log10(N/m)$ where $N$ is the number of observations and $m$ the number
#' of series. Will be automatically limited to one less than the number of
#' observations in the series.
#' @param type character string giving the type of acf to be computed. Allowed
#' values are \dQuote{\code{correlation}} (the default),
#' \dQuote{\code{covariance}} or \dQuote{\code{partial}}.
#' @param plot logical. If \code{TRUE} (the default) the resulting acf, pacf or
#' ccf is plotted.
#' @param na.action function to handle missing values. Default is
#' \code{\link[stats]{na.contiguous}}.  Useful alternatives are
#' \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.
#' @param demean Should covariances be about the sample means?
#' @param calc.ci If \code{TRUE}, confidence intervals for the ACF/PACF
#' estimates are calculated.
#' @param level Percentage level used for the confidence intervals.
#' @param nsim The number of bootstrap samples used in estimating the
#' confidence intervals.
#' @param ... Additional arguments passed to the plotting function.
#' @return The \code{Acf}, \code{Pacf} and \code{Ccf} functions return objects
#' of class "acf" as described in \code{\link[stats]{acf}} from the stats
#' package. The \code{taperedacf} and \code{taperedpacf} functions return
#' objects of class "mpacf".
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{acf}}, \code{\link[stats]{pacf}},
#' \code{\link[stats]{ccf}}, \code{\link{tsdisplay}}
#' @references Hyndman, R.J. (2015). Discussion of ``High-dimensional
#' autocovariance matrices and optimal linear prediction''. \emph{Electronic
#' Journal of Statistics}, 9, 792-796.
#'
#' McMurry, T. L., & Politis, D. N. (2010). Banded and tapered estimates for
#' autocovariance matrices and the linear process bootstrap. \emph{Journal of
#' Time Series Analysis}, 31(6), 471-482.
#' @keywords ts
#' @examples
#'
#' Acf(wineind)
#' Pacf(wineind)
#' \dontrun{
#' taperedacf(wineind, nsim=50)
#' taperedpacf(wineind, nsim=50)
#' }
#'
#' @export
Acf <- function(x, lag.max = NULL,
                type = c("correlation", "covariance", "partial"),
                plot = TRUE, na.action = na.contiguous, demean=TRUE, ...)
{
  type <- match.arg(type)

  # Set maximum lag
  nseries <- NCOL(x)
  if (is.null(lag.max))
    lag.max <- as.integer(max(floor(10 * (log10(NROW(x)) - log10(nseries))),
      2*frequency(x)))

  acf.out <- stats::acf(x, plot=FALSE, lag.max=lag.max,
    type=type, na.action=na.action, demean=demean)

  acf.out$tsp <- tsp(x)
  acf.out$periods <- attributes(x)$msts

  if(nseries==1)
  {
    vname <- deparse(substitute(x))
    acf.out$series <- vname
  }
  # Make lags in integer units
  nlags <- dim(acf.out$lag)[1]
  if(type=="partial")
    acf.out$lag[,,] <- 1:(nlags)
  else
    acf.out$lag[,,] <- 0:(nlags-1)

  # Plot if required
  if(plot)
  {
    plot.out <- acf.out
    # Hide 0 lag if autocorrelations
    if(type=="correlation")
    {
      for(i in 1:NCOL(x))
      {
        plot.out$lag[1,i,i] <- 1
        plot.out$acf[1,i,i] <- 0
      }
    }
    if(nseries > 1)
      plot(plot.out, ...)
    else
    {
      # Check if there is a ylim input
      input_list <- as.list(substitute(list(...)))
      ylimarg <- is.element("ylim",names(input_list))
      if(ylimarg)
        plot(plot.out, xaxt="n", ...)
      else
      {
        ylim <- c(-1, 1) * 3/sqrt(length(x))
        ylim <- range(ylim, plot.out$acf)
        plot(plot.out, ylim=ylim, xaxt="n", ...)
      }
      # Make nice horizontal axis
      if(is.element("msts", class(x)))
        seasonalaxis(attributes(x)$msts, nlags, type="acf")
      else
        seasonalaxis(frequency(x), nlags, type="acf")
      if(type=="covariance")
        axis(at=0, side=1)
    }
    return(invisible(acf.out))
  }
  else
    return(acf.out)
}

# Make nice horizontal axis with ticks at seasonal lags
# Return tick points if breaks=TRUE
seasonalaxis <- function(frequency, nlags, type, plot=TRUE)
{
  # List of unlabelled tick points
  out2 <- NULL
  # Check for non-seasonal data
  if(length(frequency)==1)
  {
    # Compute number of seasonal periods
    np <- trunc(nlags/frequency)
    evenfreq <- (frequency %% 2L)==0L

    # Defaults for labelled tick points
    if(type=="acf")
      out <- pretty(1:nlags)
    else
      out <- pretty(-nlags:nlags)

    if(frequency == 1)
    {
      if(type=="acf" & nlags <= 16)
        out <- 1:nlags
      else if(type=="ccf" & nlags <= 8)
        out <- (-nlags:nlags)
      else
      {
        if(nlags <= 30 & type=="acf")
          out2 <- 1:nlags
        else if(nlags <= 15 & type=="ccf")
          out2 <- (-nlags:nlags)
        if(!is.null(out2))
          out <- pretty(out2)
      }
    }
    else if(frequency > 1 &
      ((type=="acf" & np >= 2L) | (type=="ccf" & np >= 1L)))
    {
      if(type=="acf" & nlags <= 40)
      {
        out <- frequency*(1:np)
        out2 <- 1:nlags
        # Add half-years
        if(nlags <= 30 & evenfreq & np <= 3)
          out <- c(out,frequency*((1:np)-0.5))
      }
      else if(type=="ccf" & nlags <= 20)
      {
        out <- frequency*(-np:np)
        out2 <- (-nlags:nlags)
        # Add half-years
        if(nlags <= 15 & evenfreq & np <= 3)
          out <- c(out, frequency*((-np:np)+0.5))
      }
      else if(np < (12 - 4*(type=="ccf")))
          out <- frequency*(-np:np)
    }
  }
  else
  {
    # Determine which frequency to show
    np <- trunc(nlags/frequency)
    frequency <- frequency[which(np <= 16)]
    if(length(frequency) > 0L)
      frequency <- min(frequency)
    else
      frequency <- 1
    out <- seasonalaxis(frequency, nlags, type, plot=FALSE)
  }
  if(plot)
  {
    axis(1, at=out)
    if(!is.null(out2))
      axis(1, at=out2, tcl=-0.2,labels=FALSE)
  }
  else
    return(out)
}

#' @rdname Acf
#' @export
Pacf <- function (x, lag.max=NULL,
                plot = TRUE, na.action = na.contiguous, demean=TRUE, ...)
{
  object <- Acf(x, lag.max=lag.max, type="partial",
    na.action=na.action, demean=demean, plot=FALSE)
  object$series <- deparse(substitute(x))

  # Plot if required
  if(plot)
  {
    nlags <- dim(object$lag)[1]
    plot.out <- object
    # Check if there is a ylim input
    input_list <- as.list(substitute(list(...)))
    ylimarg <- is.element("ylim",names(input_list))
    if(ylimarg)
      plot(plot.out, xaxt="n", ...)
    else
    {
      ylim <- c(-1, 1) * 3/sqrt(length(x))
      ylim <- range(ylim, plot.out$acf)
      plot(plot.out, ylim=ylim, xaxt="n", ...)
    }
    # Make nice horizontal axis
    if(is.element("msts", class(x)))
      seasonalaxis(attributes(x)$msts, nlags, type="acf")
    else
      seasonalaxis(frequency(x), nlags, type="acf")
    return(invisible(object))
  }
  else
    return(object)

}

#' @rdname Acf
#' @export
Ccf <- function (x, y, lag.max=NULL, type=c("correlation","covariance"),
                 plot=TRUE, na.action=na.contiguous, ...)
{
  type <- match.arg(type)

  if (is.null(lag.max))
    lag.max <- as.integer(max(floor(10 * log10(NROW(x))), 2*frequency(x)))

  ccf.out <- stats::ccf(x, y, plot=FALSE, type=type,
    lag.max=lag.max, na.action=na.action)

  # Make lags in integer units
  nlags <- (dim(ccf.out$lag)[1]-1)/2
  ccf.out$lag[,1,1] <- -nlags:nlags
  # Plot if required
  if(plot)
  {
    vnames <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    ccf.out$snames <- paste(vnames, collapse = " & ")
    plot(ccf.out, ylab="CCF", xaxt="n", ...)
    seasonalaxis(frequency(x), nlags, type="ccf")
    return(invisible(ccf.out))
  }
  else
    return(ccf.out)
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
  acfest <- stats::acf(c(x), lag.max = lag.max,
    plot = FALSE, na.action = na.contiguous)
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

#' @rdname Acf
#' @export
taperedacf <- function(x, lag.max=NULL, type=c("correlation","partial"),
                       plot=TRUE, calc.ci=TRUE, level=95, nsim=100, ...)
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
    plot(out, ...)
    return(invisible(out))
  }
  return(out)
}

#' @rdname Acf
#' @export
taperedpacf <- function (x, ...)
{
  taperedacf(x, type="partial", ...)
}


plot.mpacf <- function(object, xlim=NULL, ylim=NULL,
  xlab="Lag", ylab="", ...)
{
  lagx <- 1:object$lag

  if(is.null(xlim))
    xlim <- c(1,object$lag)
  if(is.null(ylim))
    ylim <- range(object$z, object$upper, object$lower)
  if(ylab=="")
    ylab <- ifelse(object$type=="partial","PACF","ACF")

  plot(lagx, object$z, type="n", xlim=xlim, ylim=ylim,
    xlab=xlab, ylab=ylab, xaxt="n", ...)

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
      polygon(lagx[j] + c(-0.55,0.55,0.55,-0.55),
        c(rep(object$lower[j],2),rep(object$upper[j],2)),
        col=gray(0.60), border=FALSE)
    }
    #    polygon(c(lagx,rev(lagx)),c(object$lower,rev(object$upper)),col=gray(.60),border=FALSE)
  }
  lines(lagx, object$z, lwd=1.5)
  j <- (object$lower<0 & object$upper>0)
  points(lagx[j], object$z[j], pch=1, cex=0.5)
  points(lagx[!j], object$z[!j], pch=19)
}

#' @rdname is.ets
#' @export
is.acf <- function(x){
  inherits(x, "acf")
}

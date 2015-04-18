auto.arima <- function(x, d=NA, D=NA, max.p=5, max.q=5,
    max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1, 
    start.p=2, start.q=2, start.P=1, start.Q=1,
    stationary=FALSE, seasonal=TRUE, ic=c("aicc","aic","bic"),
    stepwise=TRUE, trace=FALSE,
    approximation=(length(x)>100 | frequency(x)>12), xreg=NULL,
    test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
    allowdrift=TRUE,allowmean=TRUE,lambda=NULL,
    parallel=FALSE, num.cores=2)
{
  # Only non-stepwise parallel implemented so far.
  if (stepwise==TRUE & parallel==TRUE)
  {
    warning("Parallel computer is only implemented when stepwise=FALSE, the model will be fit in serial.")
    parallel <- FALSE
  }

  series <- deparse(substitute(x))
  x <- as.ts(x)

  # Check for constant data
  if(is.constant(x))
  {
    fit <- Arima(x,order=c(0,0,0),fixed=mean(x,na.rm=TRUE))
    fit$x <- x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x=x)
    fit$constant <- TRUE
    return(fit)
  }
  ic <- match.arg(ic)
  test <- match.arg(test)
  seasonal.test <- match.arg(seasonal.test)

  # Use AIC if npar <= 3
  # AICc won't work for tiny samples.
  if(length(x) <= 3L)
    ic <- "aic"

  # Only consider non-seasonal models
  if(seasonal)
    m <- frequency(x)
  else
    m <- 1
  if(m < 1)
  {
    #warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    m <- 1
  }
	max.p<-ifelse(max.p <= floor(length(x)/3), max.p, floor(length(x)/3))
	max.q<-ifelse(max.q <= floor(length(x)/3), max.q, floor(length(x)/3))
	max.P<-ifelse(max.P <= floor((length(x)/3)/m), max.P, floor((length(x)/3)/m))
	max.Q<-ifelse(max.Q <= floor((length(x)/3)/m), max.Q, floor((length(x)/3)/m))

  orig.x <- x
  if(!is.null(lambda))
    x <- BoxCox(x,lambda)

  # Choose order of differencing
  if(!is.null(xreg))
  {
    nmxreg <- deparse(substitute(xreg))
    xreg <- as.matrix(xreg)
    if(ncol(xreg)==1 & length(nmxreg) > 1)
      nmxreg <- "xreg"
    if (is.null(colnames(xreg)))
        colnames(xreg) <- if (ncol(xreg) == 1) nmxreg
                          else paste(nmxreg, 1:ncol(xreg), sep = "")
    j <- !is.na(x) & !is.na(rowSums(xreg))
    xx <- x
    xx[j] <- residuals(lm(x ~ xreg))
  }
  else
    xx <- x
  if(stationary)
    d <- D <- 0
  if(m == 1)
    D <- max.P <- max.Q <- 0
  else if(is.na(D))
    D <- nsdiffs(xx, m=m, test=seasonal.test, max.D=max.D)
  if(D > 0)
    dx <- diff(xx,differences=D,lag=m)
  else
    dx <- xx
  if(is.na(d))
    d <- ndiffs(dx,test=test, max.d=max.d)
  if(d>0)
    dx <- diff(dx,differences=d,lag=1)

  if(is.constant(dx))
  {
    if(is.null(xreg))
    {
      if(D>0)
        fit <- Arima(x,order=c(0,d,0),seasonal=list(order=c(0,D,0),period=m), fixed=mean(dx,na.rm=TRUE), include.constant=TRUE)
      else if(d < 2)
        fit <- Arima(x,order=c(0,d,0),fixed=mean(dx,na.rm=TRUE),include.constant=TRUE)
      else
        stop("Data follow a simple polynomial and are not suitable for ARIMA modelling.")
    }
    else # Perfect regression
    {
      if(D>0)
        fit <- Arima(x,order=c(0,d,0),seasonal=list(order=c(0,D,0),period=m), xreg=xreg)
      else
        fit <- Arima(x,order=c(0,d,0),xreg=xreg)
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x=x)
    return(fit)
  }

  if(m > 1)
  {
    if(max.P > 0)
      max.p <- min(max.p, m-1)
    if(max.Q > 0)
      max.q <- min(max.q, m-1)
  }

  # Find constant offset for AIC calculation using simple AR(1) model
  if(approximation)
  {
    if(D==0)
      fit <- try(arima(x,order=c(1,d,0),xreg=xreg))
    else
      fit <- try(arima(x,order=c(1,d,0),seasonal=list(order=c(0,D,0),period=m,xreg=xreg)))
    if(!is.element("try-error",class(fit)))
      offset <- -2*fit$loglik - length(x)*log(fit$sigma2)
    else
    {
      warning("Unable to calculate AIC offset")
      offset <- 0
    }
  }
  else
    offset <- 0

  if(!stepwise)
  {
    bestfit <- search.arima(x,d,D,max.p,max.q,max.P,max.Q,max.order,stationary,ic,trace,approximation,xreg=xreg,offset=offset,allowdrift=allowdrift,parallel=parallel, num.cores=num.cores)
    bestfit$call <- match.call()
    bestfit$call$x <- data.frame(x=x)
    bestfit$lamba <- lambda
    bestfit$x <- orig.x
    bestfit$series <- series
    bestfit$fitted <- fitted(bestfit)
    if(!is.null(lambda))
    {
      bestfit$fitted <- InvBoxCox(bestfit$fitted,lambda)
      bestfit$lambda <- lambda
    }
    return(bestfit)
  }

  # Starting model
  p <- start.p <- min(start.p,max.p)
  q <- start.q <- min(start.q,max.q)
  P <- start.P <- min(start.P,max.P)
  Q <- start.Q <- min(start.Q,max.Q)

  allowdrift <- allowdrift & (d+D)==1
  allowmean <- allowmean & (d+D)==0

  constant <- allowdrift | allowmean

  results <- matrix(NA,nrow=100,ncol=8)

  bestfit <- myarima(x,order=c(p,d,q),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
  results[1,] <- c(p,d,q,P,D,Q,constant,bestfit$ic)
  # Null model
  fit <- myarima(x,order=c(0,d,0),seasonal=c(0,D,0),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
  results[2,] <- c(0,d,0,0,D,0,constant,fit$ic)
  if(fit$ic < bestfit$ic)
  {
    bestfit <- fit
    p <- q <- P <- Q <- 0
  }
  # Basic AR model
  if(max.p > 0 | max.P > 0)
  {
    fit <- myarima(x,order=c(max.p>0,d,0),seasonal=c((m>1)&(max.P>0),D,0),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
    results[3,] <- c(1,d,0,m>1,D,0,constant,fit$ic)
    if(fit$ic < bestfit$ic)
    {
      bestfit <- fit
      p <- (max.p>0)
      P <- (m>1) & (max.P>0)
      q <- Q <- 0
    }
  }
  # Basic MA model
  if(max.q > 0 | max.Q > 0)
  {
    fit <- myarima(x,order=c(0,d,max.q>0),seasonal=c(0,D,(m>1)&(max.Q>0)),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
    results[4,] <- c(0,d,1,0,D,m>1,constant,fit$ic)
    if(fit$ic < bestfit$ic)
    {
      bestfit <- fit
      p <- P <- 0
      Q <- (m>1) & (max.Q>0)
      q <- (max.q>0)
    }
  }
   k <- 4

  startk <- 0
  while(startk < k & k < 94)
  {
    startk <- k
    if(P > 0 & newmodel(p,d,q,P-1,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P-1,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P-1,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        P <- (P-1)
      }
    }
    if(P < max.P & newmodel(p,d,q,P+1,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P+1,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P+1,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        P <- (P+1)
      }
    }
    if(Q > 0 & newmodel(p,d,q,P,D,Q-1,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P,D,Q-1),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P,D,Q-1,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        Q <- (Q-1)
      }
    }
    if(Q < max.Q & newmodel(p,d,q,P,D,Q+1,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P,D,Q+1),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P,D,Q+1,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        Q <- (Q+1)
      }
    }
    if(Q > 0 & P > 0 & newmodel(p,d,q,P-1,D,Q-1,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P-1,D,Q-1),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P-1,D,Q-1,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        Q <- (Q-1)
        P <- (P-1)
      }
    }
    if(Q < max.Q & P < max.P & newmodel(p,d,q,P+1,D,Q+1,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q),seasonal=c(P+1,D,Q+1),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q,P+1,D,Q+1,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        Q <- (Q+1)
        P <- (P+1)
      }
    }

    if(p > 0 & newmodel(p-1,d,q,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p-1,d,q),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p-1,d,q,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        p <- (p-1)
      }
    }
    if(p < max.p & newmodel(p+1,d,q,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p+1,d,q),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p+1,d,q,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        p <- (p+1)
      }
    }
    if(q > 0 & newmodel(p,d,q-1,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q-1),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q-1,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        q <- (q-1)
      }
    }
    if(q < max.q & newmodel(p,d,q+1,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p,d,q+1),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p,d,q+1,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        q <- (q+1)
      }
    }
    if(q > 0 & p > 0 & newmodel(p-1,d,q-1,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p-1,d,q-1),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p-1,d,q-1,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        q <- (q-1)
        p <- (p-1)
      }
    }
    if(q < max.q & p < max.p & newmodel(p+1,d,q+1,P,D,Q,constant,results[1:k,]))
    {
      k <- k + 1
      fit <- myarima(x,order=c(p+1,d,q+1),seasonal=c(P,D,Q),constant=constant,ic,trace,approximation,offset=offset,xreg=xreg)
      results[k,] <- c(p+1,d,q+1,P,D,Q,constant,fit$ic)
      if(fit$ic < bestfit$ic)
      {
        bestfit <- fit
        q <- (q+1)
        p <- (p+1)
      }
    }
    if(allowdrift | allowmean)
    {
      if(newmodel(p,d,q,P,D,Q,!constant,results[1:k,]))
      {
        k <- k + 1
        fit <- myarima(x,order=c(p,d,q),seasonal=c(P,D,Q),constant=!constant,ic,trace,approximation,offset=offset,xreg=xreg)
        results[k,] <- c(p,d,q,P,D,Q,!constant,fit$ic)
        if(fit$ic < bestfit$ic)
        {
          bestfit <- fit
          constant <- !constant
        }
      }
    }
  }

  # Refit using ML if approximation used for IC
  if(approximation & !is.null(bestfit$arma))
  {
    #constant <- length(bestfit$coef) > sum(bestfit$arma[1:4])
    newbestfit <- myarima(x,order=bestfit$arma[c(1,6,2)],
      seasonal=bestfit$arma[c(3,7,4)],constant=constant,ic,trace=FALSE,approximation=FALSE,xreg=xreg)
    if(newbestfit$ic == Inf)
    {
      warning("Unable to fit final model using maximum likelihood. AIC value approximated")
    }
    else
      bestfit <- newbestfit
  }

  # Nothing fitted
  if(bestfit$ic == Inf)
  {
    cat("\n")
    stop("No suitable ARIMA model found")
  }

  # Return best fit

  bestfit$x <- orig.x
  bestfit$series <- series
  bestfit$ic <- NULL
  bestfit$call <- match.call()
  bestfit$call$x <- data.frame(x=x)
  bestfit$lambda <- lambda
  #bestfit$fitted <- fitted(bestfit)

  if(trace)
    cat("\n\n Best model:",arima.string(bestfit),"\n\n")

  return(bestfit)
}


# Calls arima from stats package and adds data to the returned object
# Also allows refitting to new data
# and drift terms to be included.
myarima <- function(x, order = c(0, 0, 0), seasonal = c(0, 0, 0), constant=TRUE, ic="aic", trace=FALSE,approximation=FALSE,offset=0,xreg=NULL)
{
    n <- length(x)
    m <- frequency(x)
    use.season <- (sum(seasonal)>0) & m>0
    diffs <- order[2]+seasonal[2]
    if(approximation)
        method <- "CSS"
    else
        method <- "CSS-ML"
    if(diffs==1 & constant)
    {
        xreg <- cbind(drift=1:length(x),xreg)
        if(use.season)
            suppressWarnings(fit <- try(stats::arima(x=x,order=order,seasonal=list(order=seasonal,period=m),xreg=xreg,method=method),silent=TRUE))
        else
            suppressWarnings(fit <- try(stats::arima(x=x,order=order,xreg=xreg,method=method),silent=TRUE))
    }
    else
    {
        if(use.season)
            suppressWarnings(fit <- try(stats::arima(x=x,order=order,seasonal=list(order=seasonal,period=m),include.mean=constant,method=method,xreg=xreg),silent=TRUE))
        else
            suppressWarnings(fit <- try(stats::arima(x=x,order=order,include.mean=constant,method=method,xreg=xreg),silent=TRUE))
    }
    if(is.null(xreg))
      nxreg <- 0
    else
      nxreg <- ncol(as.matrix(xreg))
    if(!is.element("try-error",class(fit)))
    {
        nstar <- n - order[2] - seasonal[2]*m
        if(diffs==1 & constant)
        {
            #fitnames <- names(fit$coef)
            #fitnames[length(fitnames)-nxreg] <- "drift"
            #names(fit$coef) <- fitnames
            fit$xreg <- xreg
        }
        npar <- length(fit$coef) + 1
        if(approximation)
            fit$aic <- offset + nstar * log(fit$sigma2) + 2 * npar
        if(!is.na(fit$aic))
        {
            fit$bic <- fit$aic + npar*(log(nstar) - 2)
            fit$aicc <- fit$aic + 2*npar*(npar+1)/(nstar-npar-1)
            fit$ic <- switch(ic,bic=fit$bic,aic=fit$aic,aicc=fit$aicc)
        }
        else
            fit$aic <- fit$bic <- fit$aicc <- fit$ic <- Inf
        # Check for unit roots
        minroot <- 2
        if(order[1] + seasonal[1] > 0)
        {
            testvec <- fit$model$phi
            k <- abs(testvec) > 1e-8
            if(sum(k) > 0)
              last.nonzero <- max(which(k))
            else
              last.nonzero <- 0
            if(last.nonzero > 0)
            {
                testvec <- testvec[1:last.nonzero]
                minroot <- min(minroot,abs(polyroot(c(1,-testvec))))
            }
        }
        if(order[3] + seasonal[3] > 0)
        {
            testvec <- fit$model$theta
            k <- abs(testvec) > 1e-8
            if(sum(k) > 0)
              last.nonzero <- max(which(k))
            else
              last.nonzero <- 0
            if(last.nonzero > 0)
            {
                testvec <- testvec[1:last.nonzero]
                minroot <- min(minroot,abs(polyroot(c(1,testvec))))
            }
        }
        if(minroot < 1 + 1e-2) # Previously 1+1e-3
            fit$ic <- Inf # Don't like this model
        if(trace)
            cat("\n",arima.string(fit),":",fit$ic)
        fit$xreg <- xreg
        return(structure(fit,class=c("ARIMA","Arima")))
    }
    else
    {
        if(trace)
        {
            cat("\n ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
            if(use.season)
                cat("(",seasonal[1],",",seasonal[2],",",seasonal[3],")[",m,"]",sep="")
            if(constant & (order[2]+seasonal[2] == 0))
                cat(" with non-zero mean")
            else if(constant & (order[2]+seasonal[2] == 1))
                cat(" with drift        ")
            else if(!constant & (order[2]+seasonal[2] == 0))
                cat(" with zero mean    ")
            else
                cat("         ")
            cat(" :",Inf,"*")
        }
        return(list(ic=Inf))
    }
}

newmodel <- function(p,d,q,P,D,Q,constant,results)
{
    n <- nrow(results)
    for(i in 1:n)
    {
        if(identical(c(p,d,q,P,D,Q,constant),results[i,1:7]))
            return(FALSE)
    }
    return(TRUE)
}

arima.string <- function(object)
{
    order <- object$arma[c(1,6,2,3,7,4,5)]
    result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
    if(order[7]>1 & sum(order[4:6]) > 0)
        result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
    if(is.element("constant",names(object$coef)) | is.element("intercept",names(object$coef)))
        result <- paste(result,"with non-zero mean")
    else if(is.element("drift",names(object$coef)))
        result <- paste(result,"with drift        ")
    else if(order[2]==0 & order[5]==0)
        result <- paste(result,"with zero mean    ")
    else
        result <- paste(result,"                  ")
    return(result)
}

summary.Arima <- function(object,...)
{
    print(object)
    cat("\nTraining set error measures:\n")
    print(accuracy(object))
}



# Number of seasonal differences
nsdiffs <- function(x, m=frequency(x), test=c("ocsb", "ch"), max.D=1)
{

  if(is.constant(x))
    return(0)

  test <- match.arg(test)
  if(m==1)
    stop("Non seasonal data")
  else if(m < 1)
  {
    warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    return(0)
  }

  D <- 0
  if(test=="ch")
    dodiff <- CHtest(x,m)
  else
    dodiff <- OCSBtest(x,m)

  while(dodiff==1 & D < max.D)
  {
    D <- D + 1
    x <- diff(x, lag=m)
    if(is.constant(x))
      return(D)
    if(test=="ch")
      dodiff <- CHtest(x,m)
    else
      dodiff <- OCSBtest(x,m)
  }
  return(D)
}

CHtest <- function(x,m)
{

    if(length(x) < 2*m + 5)
      return(0)
    chstat <- SD.test(x, m)
    crit.values <- c(0.4617146,0.7479655,1.0007818,1.2375350,1.4625240,1.6920200,1.9043096,2.1169602,
        2.3268562,2.5406922,2.7391007)
    if(m <= 12)
        D <- as.numeric(chstat > crit.values[m-1])
    else if (m == 24)
        D <- as.numeric(chstat > 5.098624)
    else if (m ==52)
        D <- as.numeric(chstat > 10.341416)
    else if (m ==365)
        D <- as.numeric(chstat > 65.44445)
    else
        D <- as.numeric(chstat > 0.269 * m^(0.928))
    return(D)
}

# Return critical values for OCSB test at 5% level
# Approximation based on extensive simulations.
calcOCSBCritVal <- function(seasonal.period)
{
    log.m <- log(seasonal.period)
    return(-0.2937411*exp(-0.2850853*(log.m-0.7656451)+(-0.05983644)*((log.m-0.7656451)^2))-1.652202)
}


OCSBtest <- function(time.series, period)
{
    if(length(time.series) < (2*period+5))
    {
      #warning("Time series too short for seasonal differencing")
      return(0)
    }

    # Compute (1-B)(1-B^m)y_t
    seas.diff.series <- diff(time.series, lag = period, differences=1)
    diff.series <- diff(seas.diff.series, lag = 1, differences=1)

    # Compute (1-B^m)y_{t-1}
    y.one <- time.series[2:length(time.series)]
    y.one <- diff(y.one, lag=period, differences=1)

    # Compute (1-B)y_{t-m}
    y.two <- time.series[(1+period):length(time.series)]
    y.two <- diff(y.two, lag=1, differences=1)

    # Make all series of the same length and matching time periods
    y.one <- y.one[(1+period):(length(y.one)-1)]
    y.two <- y.two[2:(length(y.two)-period)]
    diff.series <- diff.series[(1+period+1):(length(diff.series))]
    contingent.series <- diff.series

    x.reg <- cbind(y.one, y.two)
    diff.series <- ts(data = diff.series, frequency=period)

    regression <- try(Arima(diff.series, order=c(3,0,0), seasonal=list(order=c(1,0,0),period=period), xreg=x.reg), silent=TRUE)

    if(is.element("try-error",class(regression)) | tryCatch(checkarima(regression), error=function(e) TRUE))
    {
        regression <- try(Arima(diff.series, order=c(3,0,0), seasonal=list(order=c(0,0,0),period=period), xreg=x.reg), silent=TRUE)

        if(is.element("try-error",class(regression)) | tryCatch(checkarima(regression), error=function(e) TRUE))
        {
            regression <- try(Arima(diff.series, order=c(2,0,0), seasonal=list(order=c(0,0,0),period=period), xreg=x.reg), silent=TRUE)

            if(is.element("try-error",class(regression)) | tryCatch(checkarima(regression), error=function(e) TRUE))
            {
                regression <- try(Arima(diff.series, order=c(1,0,0), seasonal=list(order=c(0,0,0),period=period), xreg=x.reg), silent=TRUE)

                if(is.element("try-error",class(regression)) | tryCatch(checkarima(regression), error=function(e) TRUE))
                {
                    regression <- try(lm(contingent.series ~ y.one + y.two - 1, na.action=NULL), silent=TRUE)
                    if(is.element("try-error",class(regression)))
                      stop("The OCSB regression model cannot be estimated")
                    # Check if perfect regression. In that case, safest to do no differencing
                    meanratio <- mean(abs(regression$residuals),na.rm=TRUE)/mean(abs(contingent.series), na.rm=TRUE)
                    if(is.nan(meanratio))
                      return(0)
                    if(meanratio < 1e-10)
                      return(0)
                    # Proceed to do OCSB test.
                    reg.summary <- summary(regression)
                    reg.coefs <- reg.summary$coefficients
                    t.two.pos <- grep("t.two", rownames(reg.coefs), fixed = TRUE)
                    if(length(t.two.pos) != 0)
                      t.two <- reg.coefs[t.two.pos,3]
                    else
                      t.two <- NA

                    if((is.nan(t.two)) | (is.infinite(t.two)) | (is.na(t.two)) | (is.element("try-error",class(regression))))
                      return(1)
                    else
                      return(as.numeric(t.two >= calcOCSBCritVal(period)))
                }
            }
        }
    }

    suppressWarnings(se <- sqrt(diag(regression$var.coef)))
    t.two <- regression$coef[names(regression$coef)=="y.two"]/se[names(se)=="y.two"]

    return(as.numeric(t.two >= calcOCSBCritVal(period)))
}

# Check that Arima object has positive coefficient variances without returning warnings
checkarima <- function(object)
{
  suppressWarnings(test <- any(is.nan(sqrt(diag(object$var.coef)))))
  return(test)
}

is.constant <- function(x)
{
  x <- as.numeric(x)
  y <- rep(x[1],length(x))
  isequal <- all.equal(c(x),y)
  return(isequal==TRUE)
}


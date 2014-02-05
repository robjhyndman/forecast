search.arima <- function(x, d=NA, D=NA, max.p=5, max.q=5,
    max.P=2, max.Q=2, max.order=5, stationary=FALSE, ic=c("aic","aicc","bic"),
    trace=FALSE,approximation=FALSE,xreg=NULL,offset=offset,allowdrift=TRUE,
    parallel=FALSE, num.cores=NULL)
{
    #dataname <- substitute(x)
    ic <- match.arg(ic)
    m <- frequency(x)

    oldwarn <- options()$warn
    options(warn=-1)
    on.exit(options(warn=oldwarn))

    if(allowdrift)
        maxK <- (d+D <= 1)
    else
        maxK <- ((d+D) == 0)

    # Choose model orders
    #Serial - technically could be combined with the code below
    if (parallel==FALSE)
    {
        best.ic <- 1e20
        for(i in 0:max.p)
        {
            for(j in 0:max.q)
            {
                for(I in 0:max.P)
                {
                    for(J in 0:max.Q)
                    {
                        if(i+j+I+J <= max.order)
                        {
                            for(K in 0:maxK)
                            {
                                fit <- myarima(x,order=c(i,d,j),seasonal=c(I,D,J),constant=(K==1),trace=trace,ic=ic,approximation=approximation,offset=offset,xreg=xreg)
                                if(fit$ic < best.ic)
                                {
                                    best.ic <- fit$ic
                                    bestfit <- fit
                                    constant <- (K==1)
                                }
                            }
                        }
                    }
                }
            }
        }
    } else
################################################################################
    # Parallel
    if (parallel==TRUE){

		to.check <- WhichModels(max.p, max.q, max.P, max.Q, maxK)

            par.all.arima <- function(l){
                .tmp <- UndoWhichModels(l)
                i <- .tmp[1]; j <- .tmp[2]; I <- .tmp[3]; J <- .tmp[4]; K <- .tmp[5]==1

                if (i+j+I+J <= max.order){
                    fit <- myarima(x,order=c(i,d,j),seasonal=c(I,D,J),constant=(K==1),trace=trace,ic=ic,approximation=approximation,offset=offset,xreg=xreg)
                }
                if (exists("fit")){
                    return(cbind(fit, K))
                } else return(NULL)
            }

			if(is.null(num.cores)) {
				num.cores <- detectCores()
			}
            cl <- makeCluster(num.cores)
            all.models <- parLapply(cl=cl, X=to.check, fun=par.all.arima)
            stopCluster(cl=cl)

            # Removing null elements
            all.models <- all.models[!sapply(all.models, is.null)]

            # Choosing best model
            best.ic <- 1e20
            for (i in 1:length(all.models)){
                if(!is.null(all.models[[i]][, 1]$ic) && all.models[[i]][, 1]$ic < best.ic){
                    bestfit <- all.models[[i]][, 1]
                    best.ic <- bestfit$ic
                    constant <- unlist(all.models[[i]][1, 2])
                }
            }
            class(bestfit) <- "Arima"
        }
################################################################################
    if(exists("bestfit"))
    {
        # Refit using ML if approximation used for IC
        if(approximation)
        {
            #constant <- length(bestfit$coef) - ncol(xreg) > sum(bestfit$arma[1:4])
            newbestfit <- myarima(x,order=bestfit$arma[c(1,6,2)],
                seasonal=bestfit$arma[c(3,7,4)],constant=constant,ic,trace=FALSE,approximation=FALSE,xreg=xreg)
            if(newbestfit$ic > 1e19)
            {
                options(warn=oldwarn)
                warning("Unable to fit final model using maximum likelihood. AIC value approximated")
            }
            else
                bestfit <- newbestfit
        }
    }
    else
        stop("No ARIMA model able to be estimated")

    bestfit$x <- x
    bestfit$series <- deparse(substitute(x))
    bestfit$ic <- NULL
    bestfit$call <- match.call()
    #bestfit$call$data <- dataname
#    bestfit$xreg <- xreg

    if(trace)
        cat("\n\n")

    return(bestfit)
}


ndiffs <- function(x,alpha=0.05,test=c("kpss","adf","pp"), max.d=2)
{
  test <- match.arg(test)
  x <- c(na.omit(c(x)))
  d <- 0

  if(is.constant(x))
    return(d)

  oldwarn <- options(warn=-1)
  on.exit(options(warn=oldwarn$warn))
  if(test=="kpss")
    dodiff <- tseries::kpss.test(x)$p.value < alpha
  else if(test=="adf")
    dodiff <- tseries::adf.test(x)$p.value > alpha
  else if(test=="pp")
    dodiff <- tseries::pp.test(x)$p.value > alpha
  else
    stop("This shouldn't happen")
  if(is.na(dodiff))
  {
    return(d)
  }
  while(dodiff & d < max.d)
  {
    d <- d+1
    x <- diff(x)
    if(is.constant(x))
      return(d)
    if(test=="kpss")
      dodiff <- tseries::kpss.test(x)$p.value < alpha
    else if(test=="adf")
      dodiff <- tseries::adf.test(x)$p.value > alpha
    else if(test=="pp")
      dodiff <- tseries::pp.test(x)$p.value > alpha
    else
      stop("This shouldn't happen")
    if(is.na(dodiff))
      return(d-1)
  }
  return(d)
}

# Set up seasonal dummies using Fourier series
SeasDummy <- function(x)
{
    n <- length(x)
    m <- frequency(x)
    if(m==1)
        stop("Non-seasonal data")
    tt <- 1:n
    fmat <- matrix(NA,nrow=n,ncol=2*m)
    for(i in 1:m)
    {
        fmat[,2*i] <- sin(2*pi*i*tt/m)
        fmat[,2*(i-1)+1] <- cos(2*pi*i*tt/m)
    }
    return(fmat[,1:(m-1)])
}

# CANOVA-HANSEN TEST
# Largely based on uroot package code for CH.test()
SD.test <- function (wts, s=frequency(wts))
{
    if(any(is.na(wts)))
        stop("Series contains missing values. Please choose order of seasonal differencing manually.")
    if(s==1)
        stop("Not seasonal data")
    t0 <- start(wts)
    N <- length(wts)
    if(N <= s)
        stop("Insufficient data")
    frec <- rep(1, as.integer((s+1)/2))
    ltrunc <- round(s * (N/100)^0.25)
    R1 <- as.matrix(SeasDummy(wts))
    lmch <- lm(wts ~ R1, na.action=na.exclude)   # run the regression : y(i)=mu+f(i)'gamma(i)+e(i)
    Fhat <- Fhataux <- matrix(nrow=N, ncol=s-1)
    for (i in 1:(s-1))
        Fhataux[, i] <- R1[,i] * residuals(lmch)
    for (i in 1:N)
    {
        for (n in 1:(s - 1))
            Fhat[i, n] <- sum(Fhataux[1:i, n])
    }
    wnw <- 1 - seq(1, ltrunc, 1)/(ltrunc + 1)
    Ne <- nrow(Fhataux)
    Omnw <- 0
    for (k in 1:ltrunc)
        Omnw <- Omnw + (t(Fhataux)[, (k + 1):Ne] %*% Fhataux[1:(Ne - k), ]) * wnw[k]
    Omfhat <- (crossprod(Fhataux) + Omnw + t(Omnw))/Ne
    sq <- seq(1, s-1, 2)
    frecob <- rep(0,s - 1)
    for (i in 1:length(frec))
    {
       if (frec[i] == 1 && i == as.integer(s/2))
           frecob[sq[i]] <- 1
       if (frec[i] == 1 && i < as.integer(s/2))
           frecob[sq[i]] <- frecob[sq[i] + 1] <- 1
    }
    a <- length(which(frecob == 1))
    A <- matrix(0, nrow=s - 1, ncol=a)
    j <- 1
    for (i in 1:(s - 1)) if (frecob[i] == 1)
    {
        A[i, j] <- 1
        ifelse(frecob[i] == 1, j <- j + 1, j <- j)
    }
    tmp <- t(A) %*% Omfhat %*% A
    problems <- (min(svd(tmp)$d) < .Machine$double.eps)
    if(problems)
        stL <- 0
    else
        stL <- (1/N^2) * sum(diag(solve(tmp, tol=1e-25) %*% t(A) %*% t(Fhat) %*% Fhat %*% A))
    return(stL)
}


forecast.Arima <- function (object, h=ifelse(object$arma[5] > 1, 2 * object$arma[5], 10),
    level=c(80, 95), fan=FALSE, xreg=NULL, lambda=object$lambda,  bootstrap=FALSE, npaths=5000,...)
{
#    use.constant <- is.element("constant",names(object$coef))
    use.drift <- is.element("drift", names(object$coef))
    x <- object$x <- getResponse(object)
    usexreg <- (!is.null(xreg) | use.drift | is.element("xreg",names(object)))# | use.constant)
#    if(use.constant)
#        xreg <- as.matrix(rep(1,h))
    if(!is.null(xreg))
    {
        origxreg <- xreg <- as.matrix(xreg)
        h <- nrow(xreg)
    }
    else
      origxreg <- NULL
    if (use.drift)
    {
        n <- length(x)
        if(!is.null(xreg))
            xreg <- cbind((1:h)+n,xreg)
        else
            xreg <- as.matrix((1:h)+n)
    }
    if(usexreg)
    {
        if(is.null(xreg))
          stop("No regressors provided")
        if(!is.null(object$xreg))
            object$call$xreg <- object$xreg
        else # object from arima() rather than Arima()
        {
            xr <- object$call$xreg
            object$call$xreg <- if (!is.null(xr))
                                    eval.parent(xr)
                                else NULL
        }
        if(ncol(xreg) != ncol(object$call$xreg))
          stop("Number of regressors does not match fitted model")
        pred <- predict(object, n.ahead=h, newxreg=xreg)
    }
    else
        pred <- predict(object, n.ahead=h)

    # Fix time series characteristics if there are missing values at end of series.
    if(!is.null(x))
    {
      tspx <- tsp(x)
      nx <- max(which(!is.na(x)))
      if(nx != length(x))
      {
        tspx[2] <- time(x)[nx]
        start.f <- tspx[2]+1/tspx[3]
        pred$pred <- ts(pred$pred,frequency=tspx[3],start=start.f)
        pred$se <- ts(pred$se,frequency=tspx[3],start=start.f)
      }
    }

    if(fan)
        level <- seq(51,99,by=3)
    else
    {
        if(min(level) > 0 & max(level) < 1)
            level <- 100*level
        else if(min(level) < 0 | max(level) > 99.99)
            stop("Confidence limit out of range")
    }

    # Compute prediction intervals
    nint <- length(level)
    if(bootstrap) # Compute prediction intervals using simulations
    {
        sim <- matrix(NA,nrow=npaths,ncol=h)
        for(i in 1:npaths)
            sim[i,] <- simulate(object, nsim=h, bootstrap=TRUE, xreg=origxreg, lambda=lambda)
        lower <- apply(sim, 2, quantile, 0.5 - level/200, type = 8)
        upper <- apply(sim, 2, quantile, 0.5 + level/200, type = 8)
        if (nint > 1L) {
          lower <- t(lower)
          upper <- t(upper)
        }
    }
    else { # Compute prediction intervals via the normal distribution
      lower <- matrix(NA, ncol=nint, nrow=length(pred$pred))
      upper <- lower
      for (i in 1:nint)
      {
          qq <- qnorm(0.5 * (1 + level[i]/100))
          lower[, i] <- pred$pred - qq * pred$se
          upper[, i] <- pred$pred + qq * pred$se
      }
    }
    colnames(lower)=colnames(upper)=paste(level, "%", sep="")
    method <- arima.string(object)
    fits <- fitted(object)
    if(!is.null(lambda)) { # Back-transform point forecasts and prediction intervals
      pred$pred <- InvBoxCox(pred$pred,lambda)
      if(!bootstrap) { # Bootstrapped intervals already back-transformed
        lower <- InvBoxCox(lower,lambda)
        upper <- InvBoxCox(upper,lambda)
      }
    }
    return(structure(list(method=method, model=object, level=level,
        mean=pred$pred, lower=lower, upper=upper, x=x,
        xname=deparse(substitute(x)), fitted=fits, residuals=residuals(object)),
        class="forecast"))
}


forecast.ar <- function(object,h=10,level=c(80,95),fan=FALSE, lambda=NULL,  bootstrap=FALSE, npaths=5000,...)
{
    pred <- predict(object,n.ahead=h)
    if(bootstrap) # Recompute se using simulations
    {
        sim <- matrix(NA,nrow=npaths,ncol=h)
        for(i in 1:npaths)
            sim[i,] <- simulate(object, nsim=h, bootstrap=TRUE)
        pred$se <- apply(sim,2,sd)
    }
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
    lower <- matrix(NA,ncol=nint,nrow=length(pred$pred))
    upper <- lower
    for(i in 1:nint)
    {
        qq <- qnorm(0.5*(1+level[i]/100))
        lower[,i] <- pred$pred - qq*pred$se
        upper[,i] <- pred$pred + qq*pred$se
    }
    colnames(lower)=colnames(upper)=paste(level,"%",sep="")
    method <- paste("AR(",object$order,")",sep="")
    x <- getResponse(object)
    f <- frequency(x)
    res <- ts(object$resid[-(1:object$order)],start=tsp(x)[1]+object$order/f,frequency=f)
    fits <- x-res

    if(!is.null(lambda))
    {
      pred$pred <- InvBoxCox(pred$pred,lambda)
      lower <- InvBoxCox(lower,lambda)
      upper <- InvBoxCox(upper,lambda)
      fits <- InvBoxCox(fits,lambda)
      x <- InvBoxCox(x,lambda)
    }

    return(structure(list(method=method,model=object,level=level,mean=pred$pred,lower=lower,upper=upper,
        x=x, xname=deparse(substitute(x)), fitted=fits,residuals=res)
        ,class="forecast"))
}

# Extract errors from ARIMA model (as distinct from residuals)
arima.errors <- function(z)
{
  if(!is.list(z))
    stop("z must be a list")
    x <- getResponse(z)
  if(!is.element("xreg",names(z)))
  {
    if(!is.element("xreg",names(z$coef)))
      return(x)
    else
      xreg <- eval.parent(z$coef$xreg)
  }
  else
    xreg <- z$xreg
  norder <- sum(z$arma[1:4])
  if(is.element("intercept",names(z$coef)))
    xreg <- cbind(rep(1,length(x)),xreg)
  return(ts(c(x - xreg %*% as.matrix(z$coef[(norder+1):length(z$coef)])),frequency=frequency(x),start=start(x)))
}

# Return one-step fits
fitted.Arima <- function(object,...)
{
    x <- getResponse(object)
    if(is.null(x))
    {
        #warning("Fitted values are unavailable due to missing historical data")
        return(NULL)
    }
    if(is.null(object$lambda))
        return(x - object$residuals)
    else
        return(InvBoxCox(BoxCox(x,object$lambda) - object$residuals, object$lambda))
}

# Calls arima from stats package and adds data to the returned object
# Also allows refitting to new data
# and drift terms to be included.
Arima <- function(x, order=c(0, 0, 0),
      seasonal=c(0, 0, 0), 
      xreg=NULL, include.mean=TRUE, include.drift=FALSE, include.constant, lambda=model$lambda,
    transform.pars=TRUE,
      fixed=NULL, init=NULL, method=c("CSS-ML", "ML", "CSS"),
      n.cond, optim.control=list(), kappa=1e6, model=NULL)
{
    # Remove outliers near ends
    #j <- time(x)
    #x <- na.contiguous(x)
    #if(length(j) != length(x))
    #    warning("Missing values encountered. Using longest contiguous portion of time series")

  series <- deparse(substitute(x))

  origx <- x
  if(!is.null(lambda))
    x <- BoxCox(x,lambda)

  if (!is.null(xreg))
  {
    nmxreg <- deparse(substitute(xreg))
    xreg <- as.matrix(xreg)
    if (is.null(colnames(xreg)))
        colnames(xreg) <- if (ncol(xreg) == 1) nmxreg else paste(nmxreg, 1:ncol(xreg), sep="")
  }

  if(!is.list(seasonal))
  {
    if(frequency(x) <= 1)
      seasonal <- list(order=c(0,0,0), period=NA)
  	else
      seasonal <- list(order=seasonal, period=frequency(x))
  }

  if(!missing(include.constant))
  {
    if(include.constant)
    {
      include.mean <- TRUE
      if((order[2] + seasonal$order[2]) == 1)
        include.drift <- TRUE
    }
    else
    {
      include.mean <- include.drift <- FALSE
    }
  }
  if((order[2] + seasonal$order[2]) > 1 & include.drift)
  {
    warning("No drift term fitted as the order of difference is 2 or more.")
    include.drift <- FALSE
  }

  if(!is.null(model))
  {
    tmp <- arima2(x,model,xreg=xreg)
    xreg <- tmp$xreg
  }
  else
  {
    if(include.drift)
    {
      drift <- 1:length(x)
      xreg <- cbind(drift=drift,xreg)
    }
    if(is.null(xreg))
      tmp <- stats::arima(x=x,order=order,seasonal=seasonal,include.mean=include.mean,
          transform.pars=transform.pars,fixed=fixed,init=init,method=method,n.cond=n.cond,optim.control=optim.control,kappa=kappa)
    else
      tmp <- stats::arima(x=x,order=order,seasonal=seasonal,xreg=xreg,include.mean=include.mean,
             transform.pars=transform.pars,fixed=fixed,init=init,method=method,n.cond=n.cond,optim.control=optim.control,kappa=kappa)
  }

  # Calculate aicc & bic based on tmp$aic
  npar <- length(tmp$coef) + 1
  nstar <- length(tmp$residuals) - tmp$arma[6] - tmp$arma[7]*tmp$arma[5]
  tmp$aicc <- tmp$aic + 2*npar*(nstar/(nstar-npar-1) - 1)
  tmp$bic <- tmp$aic + npar*(log(nstar) - 2)
  tmp$series <- series
  tmp$xreg <- xreg
  tmp$call <- match.call()
  tmp$lambda <- lambda
  tmp$x <- origx

  return(tmp)
}

# Refits the model to new data x
arima2 <- function (x, model, xreg)
{
    use.drift <- is.element("drift",names(model$coef))
    use.intercept <- is.element("intercept",names(model$coef))
    use.xreg <- is.element("xreg",names(model$call))
    if(use.drift)
    {
      driftmod <- lm(model$xreg[,"drift"] ~ I(time(model$x)))
      newxreg <- driftmod$coeff[1] + driftmod$coeff[2]*time(x)
      if(!is.null(xreg))
        xreg[,"drift"] <- newxreg
      else
        xreg <- as.matrix(data.frame(drift=newxreg))
      use.xreg <- TRUE
    }

    if(model$arma[5]>1 & sum(abs(model$arma[c(3,4,7)]))>0) # Seasonal model
    {
        if(use.xreg)
            refit <- Arima(x,order=model$arma[c(1,6,2)],seasonal=list(order=model$arma[c(3,7,4)],period=model$arma[5]),
                fixed=model$coef,include.mean=use.intercept,xreg=xreg)
        else
            refit <- Arima(x,order=model$arma[c(1,6,2)],seasonal=list(order=model$arma[c(3,7,4)],period=model$arma[5]),
                fixed=model$coef,include.mean=use.intercept)
    }
    else if(length(model$coef)>0) # Nonseasonal model with some parameters
    {
        if(use.xreg)
           refit <- Arima(x,order=model$arma[c(1,6,2)],fixed=model$coef,xreg=xreg,include.mean=use.intercept)
        else
            refit <- Arima(x,order=model$arma[c(1,6,2)],fixed=model$coef,include.mean=use.intercept)
    }
    else # No parameters
            refit <- Arima(x,order=model$arma[c(1,6,2)],include.mean=FALSE)

    refit$var.coef <- matrix(0,length(refit$coef),length(refit$coef))
    if(use.xreg) # Why is this needed?
      refit$xreg <- xreg
    return(refit)
}

# Modified version of function in stats package

print.Arima <- function (x, digits=max(3, getOption("digits") - 3), se=TRUE,
    ...)
{
    cat("Series:",x$series,"\n")
    cat(arima.string(x),"\n")
    if(!is.null(x$lambda))
        cat("Box Cox transformation: lambda=",x$lambda,"\n")
    #cat("\nCall:", deparse(x$call, width.cutoff=75), "\n", sep=" ")
#    if(!is.null(x$xreg))
#    {
#        cat("\nRegression variables fitted:\n")
#        xreg <- as.matrix(x$xreg)
#        for(i in 1:3)
#            cat("  ",xreg[i,],"\n")
#        cat("   . . .\n")
#        for(i in 1:3)
#            cat("  ",xreg[nrow(xreg)-3+i,],"\n")
#    }
    if (length(x$coef) > 0) {
        cat("\nCoefficients:\n")
        coef <- round(x$coef, digits=digits)
        if (se && NROW(x$var.coef)) {
            ses <- rep(0, length(coef))
            ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits=digits)
            coef <- matrix(coef, 1L, dimnames=list(NULL, names(coef)))
            coef <- rbind(coef, s.e.=ses)
        }
        print.default(coef, print.gap=2)
    }
    cm <- x$call$method
    if (is.null(cm) || cm != "CSS")
    {
        cat("\nsigma^2 estimated as ", format(x$sigma2, digits=digits),
            ":  log likelihood=", format(round(x$loglik, 2L)),"\n",sep="")
        npar <- length(x$coef) + 1
        nstar <- length(x$residuals) - x$arma[6] - x$arma[7]*x$arma[5]
        bic <- x$aic + npar*(log(nstar) - 2)
        aicc <- x$aic + 2*npar*(nstar/(nstar-npar-1) - 1)
        cat("AIC=", format(round(x$aic, 2L)), sep="")
        cat("   AICc=", format(round(aicc, 2L)), sep="")
        cat("   BIC=", format(round(bic, 2L)), "\n",sep="")
    }
    else cat("\nsigma^2 estimated as ", format(x$sigma2, digits=digits),
        ":  part log likelihood=", format(round(x$loglik, 2)),
        "\n", sep="")
    invisible(x)
}

# Modified version of function in stats package

predict.Arima <- function(object, n.ahead=1, newxreg=NULL, se.fit=TRUE, ...)
{
    myNCOL <- function(x) if (is.null(x))
        0
    else NCOL(x)
    rsd <- object$residuals
    ## LINES ADDED
    if(!is.null(object$xreg))
        object$call$xreg <- object$xreg
    ## END ADDITION
    xr <- object$call$xreg
    xreg <- if (!is.null(xr))
        eval.parent(xr)
    else NULL
    ncxreg <- myNCOL(xreg)
    if (myNCOL(newxreg) != ncxreg)
        stop("'xreg' and 'newxreg' have different numbers of columns: ", ncxreg, " != ", myNCOL(newxreg))
    class(xreg) <- NULL
    xtsp <- tsp(rsd)
    n <- length(rsd)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1:4])
    if (length(coefs) > narma) {
        if (names(coefs)[narma + 1] == "intercept") {
            xreg <- cbind(intercept=rep(1, n), xreg)
            newxreg <- cbind(intercept=rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg + 1
        }
        xm <- if (narma == 0)
            drop(as.matrix(newxreg) %*% coefs)
        else drop(as.matrix(newxreg) %*% coefs[-(1:narma)])
    }
    else xm <- 0
    if (arma[2] > 0) {
        ma <- coefs[arma[1] + 1:arma[2]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if (arma[4] > 0) {
        ma <- coefs[sum(arma[1:3]) + 1:arma[4]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1]] + xm, start=xtsp[2] + deltat(rsd), frequency=xtsp[3])
    if (se.fit) {
        se <- ts(sqrt(z[[2]] * object$sigma2), start=xtsp[2] +
            deltat(rsd), frequency=xtsp[3])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}

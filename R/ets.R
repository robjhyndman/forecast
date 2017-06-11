#' Exponential smoothing state space model
#'
#' Returns ets model applied to \code{y}.
#'
#' Based on the classification of methods as described in Hyndman et al (2008).
#'
#' The methodology is fully automatic. The only required argument for ets is
#' the time series. The model is chosen automatically if not specified. This
#' methodology performed extremely well on the M3-competition data. (See
#' Hyndman, et al, 2002, below.)
#'
#' @aliases print.ets summary.ets as.character.ets coef.ets tsdiag.ets
#'
#' @param y a numeric vector or time series of class \code{ts}
#' @param model Usually a three-character string identifying method using the
#' framework terminology of Hyndman et al. (2002) and Hyndman et al. (2008).
#' The first letter denotes the error type ("A", "M" or "Z"); the second letter
#' denotes the trend type ("N","A","M" or "Z"); and the third letter denotes
#' the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive,
#' "M"=multiplicative and "Z"=automatically selected. So, for example, "ANN" is
#' simple exponential smoothing with additive errors, "MAM" is multiplicative
#' Holt-Winters' method with multiplicative errors, and so on.
#'
#' It is also possible for the model to be of class \code{"ets"}, and equal to
#' the output from a previous call to \code{ets}. In this case, the same model
#' is fitted to \code{y} without re-estimating any smoothing parameters. See
#' also the \code{use.initial.values} argument.
#' @param damped If TRUE, use a damped trend (either additive or
#' multiplicative). If NULL, both damped and non-damped trends will be tried
#' and the best model (according to the information criterion \code{ic})
#' returned.
#' @param alpha Value of alpha. If NULL, it is estimated.
#' @param beta Value of beta. If NULL, it is estimated.
#' @param gamma Value of gamma. If NULL, it is estimated.
#' @param phi Value of phi. If NULL, it is estimated.
#' @param additive.only If TRUE, will only consider additive models. Default is
#' FALSE.
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise,
#' data transformed before model is estimated. When \code{lambda} is specified,
#' \code{additive.only} is set to \code{TRUE}.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities.
#' @param lower Lower bounds for the parameters (alpha, beta, gamma, phi)
#' @param upper Upper bounds for the parameters (alpha, beta, gamma, phi)
#' @param opt.crit Optimization criterion. One of "mse" (Mean Square Error),
#' "amse" (Average MSE over first \code{nmse} forecast horizons), "sigma"
#' (Standard deviation of residuals), "mae" (Mean of absolute residuals), or
#' "lik" (Log-likelihood, the default).
#' @param nmse Number of steps for average multistep MSE (1<=\code{nmse}<=30).
#' @param bounds Type of parameter space to impose: \code{"usual" } indicates
#' all parameters must lie between specified lower and upper bounds;
#' \code{"admissible"} indicates parameters must lie in the admissible space;
#' \code{"both"} (default) takes the intersection of these regions.
#' @param ic Information criterion to be used in model selection.
#' @param restrict If \code{TRUE} (default), the models with infinite variance
#' will not be allowed.
#' @param allow.multiplicative.trend If \code{TRUE}, models with multiplicative
#' trend are allowed when searching for a model. Otherwise, the model space
#' excludes them. This argument is ignored if a multiplicative trend model is
#' explicitly requested (e.g., using \code{model="MMN"}).
#' @param use.initial.values If \code{TRUE} and \code{model} is of class
#' \code{"ets"}, then the initial values in the model are also not
#' re-estimated.
#' @param ... Other undocumented arguments.
#' @return An object of class "\code{ets}".
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{ets} and associated
#' functions.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{HoltWinters}}, \code{\link{rwf}},
#' \code{\link{Arima}}.
#' @references Hyndman, R.J., Koehler, A.B., Snyder, R.D., and Grose, S. (2002)
#' "A state space framework for automatic forecasting using exponential
#' smoothing methods", \emph{International J. Forecasting}, \bold{18}(3),
#' 439--454.
#'
#' Hyndman, R.J., Akram, Md., and Archibald, B. (2008) "The admissible
#' parameter space for exponential smoothing models". \emph{Annals of
#' Statistical Mathematics}, \bold{60}(2), 407--426.
#'
#' Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
#' \emph{Forecasting with exponential smoothing: the state space approach},
#' Springer-Verlag. \url{http://www.exponentialsmoothing.net}.
#' @keywords ts
#' @examples
#' fit <- ets(USAccDeaths)
#' plot(forecast(fit))
#'
#' @export
ets <- function(y, model="ZZZ", damped=NULL,
    alpha=NULL, beta=NULL, gamma=NULL, phi=NULL, additive.only=FALSE, lambda=NULL, biasadj=FALSE,
    lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98),
    opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3, bounds=c("both","usual","admissible"),
    ic=c("aicc","aic","bic"),restrict=TRUE, allow.multiplicative.trend=FALSE,
    use.initial.values=FALSE, ...)
{
  #dataname <- substitute(y)
  opt.crit <- match.arg(opt.crit)
  bounds <- match.arg(bounds)
  ic <- match.arg(ic)

  seriesname <- deparse(substitute(y))

  if(any(class(y) %in% c("data.frame","list","matrix","mts")))
    stop("y should be a univariate time series")
  y <- as.ts(y)

  # Check if data is constant
  if (missing(model) & is.constant(y))
    return(ses(y, alpha=0.99999, initial='simple')$model)

  # Remove missing values near ends
  ny <- length(y)
  y <- na.contiguous(y)
  if(ny != length(y))
    warning("Missing values encountered. Using longest contiguous portion of time series")

  orig.y <- y
  if(class(model)=="ets" & is.null(lambda))
    lambda <- model$lambda
  if(!is.null(lambda))
  {
    y <- BoxCox(y,lambda)
    additive.only=TRUE
  }

  if(nmse < 1 | nmse > 30)
    stop("nmse out of range")
  m <- frequency(y)

  if(any(upper < lower))
    stop("Lower limits must be less than upper limits")

  # If model is an ets object, re-fit model to new data
  if(class(model)=="ets")
  {
    # Prevent alpha being zero (to avoid divide by zero in the C code)
    alpha <- max(model$par["alpha"], 1e-10)
    beta <- model$par["beta"]
    if(is.na(beta))
      beta <- NULL
    gamma <- model$par["gamma"]
    if(is.na(gamma))
      gamma <- NULL
    phi <- model$par["phi"]
    if(is.na(phi))
      phi <- NULL
    modelcomponents <- paste(model$components[1],model$components[2],model$components[3],sep="")
    damped <- (model$components[4]=="TRUE")
    if(use.initial.values)
    {
      errortype  <- substr(modelcomponents,1,1)
      trendtype  <- substr(modelcomponents,2,2)
      seasontype <- substr(modelcomponents,3,3)

      # Recompute errors from pegelsresid.C
      e <- pegelsresid.C(y, m, model$initstate, errortype, trendtype, seasontype, damped, alpha, beta, gamma, phi, nmse)

      # Compute error measures
      np <- length(model$par) + 1
      model$loglik <- -0.5*e$lik
      model$aic <- e$lik + 2*np
      model$bic <- e$lik + log(ny)*np
      model$aicc <- model$aic +  2*np*(np+1)/(ny-np-1)
      model$mse <- e$amse[1]
      model$amse <- mean(e$amse)

      # Compute states, fitted values and residuals
      tsp.y <- tsp(y)
      model$states=ts(e$states,frequency=tsp.y[3],start=tsp.y[1]-1/tsp.y[3])
      colnames(model$states)[1] <- "l"
      if(trendtype!="N")
        colnames(model$states)[2] <- "b"
      if(seasontype!="N")
        colnames(model$states)[(2+(trendtype!="N")):ncol(model$states)] <- paste("s",1:m,sep="")
      if(errortype=="A")
        model$fitted <- ts(y-e$e,frequency=tsp.y[3],start=tsp.y[1])
      else
        model$fitted <- ts(y/(1+e$e),frequency=tsp.y[3],start=tsp.y[1])
      model$residuals <- ts(e$e,frequency=tsp.y[3],start=tsp.y[1])
      model$sigma2 <- mean(model$residuals^2,na.rm=TRUE)
      model$x <- orig.y
      model$series <- seriesname
      if(!is.null(lambda))
      {
        model$fitted <- InvBoxCox(model$fitted, lambda, biasadj, var(model$residuals))
        attr(lambda, "biasadj") <- biasadj
      }
      model$lambda <- lambda

      # Return model object
      return(model)
    }
    else
    {
      model <- modelcomponents
      if(missing(use.initial.values)){
        message("Model is being refit with current smoothing parameters but initial states are being re-estimated.\nSet 'use.initial.values=TRUE' if you want to re-use existing initial values.")
      }
    }
  }

  errortype  <- substr(model,1,1)
  trendtype  <- substr(model,2,2)
  seasontype <- substr(model,3,3)

  if(!is.element(errortype,c("M","A","Z")))
    stop("Invalid error type")
  if(!is.element(trendtype,c("N","A","M","Z")))
    stop("Invalid trend type")
  if(!is.element(seasontype,c("N","A","M","Z")))
    stop("Invalid season type")

  if(m < 1 | length(y) <= m)
  {
    #warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    seasontype <- "N"
  }
  if(m == 1)
  {
    if(seasontype=="A" | seasontype=="M")
      stop("Nonseasonal data")
    else
      substr(model,3,3) <- seasontype <- "N"
  }
  if(m > 24)
  {
    if(is.element(seasontype,c("A","M")))
      stop("Frequency too high")
    else if(seasontype=="Z")
    {
      warning("I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts.")
      substr(model,3,3) <- seasontype <- "N"
      #m <- 1
    }
  }

  # Check inputs
  if(restrict)
  {
    if((errortype=="A" & (trendtype=="M" | seasontype=="M")) |
        (errortype=="M" & trendtype=="M" & seasontype=="A") |
        (additive.only & (errortype=="M" | trendtype=="M" | seasontype=="M")))
      stop("Forbidden model combination")
  }

  data.positive <- (min(y) > 0)

  if(!data.positive & errortype=="M")
    stop("Inappropriate model for data with negative or zero values")

  if(!is.null(damped))
  {
    if(damped & trendtype=="N")
      stop("Forbidden model combination")
  }

  n <- length(y)
  # Check we have enough data to fit a model
  npars <- 2L # alpha + l0
  if(trendtype=="A" | trendtype=="M")
    npars <- npars + 2L # beta + b0
  if(seasontype=="A" | seasontype=="M")
    npars <- npars + m # gamma + s
  if(!is.null(damped))
    npars <- npars + as.numeric(damped)

  # Produce something non-optimized for tiny data sets
  if(n <= npars+4L)
  {
    if(!is.null(damped))
        if(damped)
          warning("Not enough data to use damping")
    if(seasontype=="A" | seasontype=="M")
    {
      fit <- try(HoltWintersZZ(orig.y,
        alpha=alpha, beta=beta, gamma=gamma, phi=phi,
        exponential=(trendtype=="M"),
        seasonal=ifelse(seasontype!="A","multiplicative","additive"),
        lambda=lambda, biasadj=biasadj), silent=TRUE)
      if(!("try-error" %in% class(fit)))
      {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse(substitute(y))
        return(fit)
      }
      else
        warning("Seasonal component could not be estimated")
    }
    if(trendtype=="A" | trendtype=="M")
    {
      fit <- try(HoltWintersZZ(orig.y,
        alpha=alpha, beta=beta, gamma=FALSE, phi=phi,
        exponential=(trendtype=="M"),
        lambda=lambda, biasadj=biasadj), silent=TRUE)
      if(!("try-error" %in% class(fit)))
      {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse(substitute(y))
        return(fit)
      }
      else
        warning("Trend component could not be estimated")
    }
    if(trendtype=="N" & seasontype=="N")
    {
      fit <- try(HoltWintersZZ(orig.y,
        alpha=alpha, beta=FALSE, gamma=FALSE,
        lambda=lambda, biasadj=biasadj), silent=TRUE)
      if(!("try-error" %in% class(fit)))
      {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse(substitute(y))
        return(fit)
      }
    }
    # Try holt and ses and return best
    fit1 <- try(HoltWintersZZ(orig.y,
        alpha=alpha, beta=beta, gamma=FALSE, phi=phi,
        exponential=(trendtype=="M"),
        lambda=lambda, biasadj=biasadj), silent=TRUE)
    fit2 <- try(HoltWintersZZ(orig.y,
        alpha=alpha, beta=FALSE, gamma=FALSE, phi=phi,
        exponential=(trendtype=="M"),
        lambda=lambda, biasadj=biasadj), silent=TRUE)
    if("try-error" %in% class(fit1))
      fit <- fit2
    else if(fit1$sigma2 < fit2$sigma2)
      fit <- fit1
    else
      fit <- fit2
    fit$call <- match.call()
    fit$method <- as.character(fit)
    fit$series <- deparse(substitute(y))
    return(fit)
  }

  # Fit model (assuming only one nonseasonal model)
  if(errortype=="Z")
    errortype <- c("A","M")
  if(trendtype=="Z")
  {
    if(allow.multiplicative.trend)
      trendtype <- c("N","A","M")
    else
      trendtype <- c("N","A")
  }
  if(seasontype=="Z")
    seasontype <- c("N","A","M")
  if(is.null(damped))
    damped <- c(TRUE,FALSE)
  best.ic <- Inf
  for(i in 1:length(errortype))
  {
    for(j in 1:length(trendtype))
    {
      for(k in 1:length(seasontype))
      {
        for(l in 1:length(damped))
        {
          if(trendtype[j]=="N" & damped[l])
            next
          if(restrict)
          {
            if(errortype[i]=="A" & (trendtype[j]=="M" | seasontype[k]=="M"))
              next
            if(errortype[i]=="M" & trendtype[j]=="M" & seasontype[k]=="A")
              next
            if(additive.only & (errortype[i]=="M" | trendtype[j]=="M" | seasontype[k]=="M"))
              next
          }
          if(!data.positive & errortype[i]=="M")
            next
          fit <- etsmodel(y,errortype[i],trendtype[j],seasontype[k],damped[l],alpha,beta,gamma,phi,
              lower=lower,upper=upper,opt.crit=opt.crit,nmse=nmse,bounds=bounds, ...)
          fit.ic <- switch(ic,aic=fit$aic,bic=fit$bic,aicc=fit$aicc)
          if(!is.na(fit.ic))
          {
            if(fit.ic < best.ic)
            {
              model <- fit
              best.ic <- fit.ic
              best.e <- errortype[i]
              best.t <- trendtype[j]
              best.s <- seasontype[k]
              best.d <- damped[l]
            }
          }
        }
      }
    }
  }
  if(best.ic == Inf)
    stop("No model able to be fitted")

  model$m <- m
  model$method <- paste("ETS(",best.e,",",best.t,ifelse(best.d,"d",""),",",best.s,")",sep="")
  model$series <- seriesname
  model$components <- c(best.e,best.t,best.s,best.d)
  model$call <- match.call()
  model$initstate <- model$states[1,]
  model$sigma2 <- mean(model$residuals^2,na.rm=TRUE)
  model$x <- orig.y
  if(!is.null(lambda))
  {
    model$fitted <- InvBoxCox(model$fitted,lambda, biasadj, var(model$residuals))
    attr(lambda, "biasadj") <- biasadj
  }

  model$lambda <- lambda
  #model$call$data <- dataname

  return(structure(model,class="ets"))
}

#' @export
as.character.ets <- function(x, ...)
{
  paste("ETS(",
        x$components[1],",",
        x$components[2],
        ifelse(x$components[4],"d",""),",",
        x$components[3],")",
    sep="")
}

# myRequire <- function(libName) {

#   req.suc <- require(libName, quietly=TRUE, character.only=TRUE)
#   if(!req.suc) stop("The ",libName," package is not available.")

#   req.suc
# }

# getNewBounds <- function(par, lower, upper, nstate) {

#   myLower <- NULL
#   myUpper <- NULL

#   if("alpha" %in% names(par)) {
#     myLower <- c(myLower, lower[1])
#     myUpper <- c(myUpper, upper[1])
#   }
#   if("beta" %in% names(par)) {
#     myLower <- c(myLower, lower[2])
#     myUpper <- c(myUpper, upper[2])
#   }
#   if("gamma" %in% names(par)) {
#     myLower <- c(myLower, lower[3])
#     myUpper <- c(myUpper, upper[3])
#   }
#   if("phi" %in% names(par)) {
#     myLower <- c(myLower, lower[4])
#     myUpper <- c(myUpper, upper[4])
#   }

#   myLower <- c(myLower,rep(-1e8,nstate))
#   myUpper <- c(myUpper,rep(1e8,nstate))

#   list(lower=myLower, upper=myUpper)
# }


etsmodel <- function(y, errortype, trendtype, seasontype, damped,
    alpha=NULL, beta=NULL, gamma=NULL, phi=NULL,
    lower, upper, opt.crit, nmse, bounds, maxit=2000, control=NULL, seed=NULL, trace=FALSE)
{

  tsp.y <- tsp(y)
  if(is.null(tsp.y))
    tsp.y <- c(1,length(y),1)
  if(seasontype != "N")
    m <- tsp.y[3]
  else
    m <- 1

  # Initialize smoothing parameters
  par <- initparam(alpha,beta,gamma,phi,trendtype,seasontype,damped,lower,upper,m)
  names(alpha) <- names(beta) <- names(gamma) <- names(phi) <- NULL
  par.noopt <- c(alpha=alpha,beta=beta,gamma=gamma,phi=phi)
  if(!is.null(par.noopt))
    par.noopt <- c(na.omit(par.noopt))
  if(!is.na(par["alpha"]))
    alpha <- par["alpha"]
  if(!is.na(par["beta"]))
    beta <- par["beta"]
  if(!is.na(par["gamma"]))
    gamma <- par["gamma"]
  if(!is.na(par["phi"]))
    phi <- par["phi"]

#    if(errortype=="M" | trendtype=="M" | seasontype=="M")
#        bounds="usual"
  if(!check.param(alpha,beta,gamma,phi,lower,upper,bounds,m))
  {
    print(paste("Model: ETS(",errortype,",",trendtype,ifelse(damped,"d",""),",",seasontype,")",sep=""))
    stop("Parameters out of range")
  }

  # Initialize state
  init.state <- initstate(y,trendtype,seasontype)
  nstate <- length(init.state)
  par <- c(par,init.state)
  lower <- c(lower,rep(-Inf,nstate))
  upper <- c(upper,rep(Inf,nstate))

  np <- length(par)
  if(np >= length(y)-1) # Not enough data to continue
    return(list(aic=Inf,bic=Inf,aicc=Inf,mse=Inf,amse=Inf,fit=NULL,par=par,states=init.state))

#-------------------------------------------------

#  if(is.null(seed)) seed <- 1000*runif(1)

  # if(solver=="malschains" || solver=="malschains_c") {

  #   malschains <- NULL
  #   if(!myRequire("Rmalschains"))
  #     stop("malschains optimizer unavailable")

  #   func <- NULL
  #   #env <- NULL

  #   if(solver=="malschains") {

  #     func <- function(myPar) {
  #       names(myPar) <- names(par)
  #       res <- lik(myPar,y=y,nstate=nstate, errortype=errortype, trendtype=trendtype,
  #           seasontype=seasontype, damped=damped, par.noopt=par.noopt, lowerb=lower, upperb=upper,
  #           opt.crit=opt.crit, nmse=nmse, bounds=bounds, m=m,pnames=names(par),pnames2=names(par.noopt))
  #       res
  #     }

  #     env <- new.env()

  #   } else {

  #     env <- etsTargetFunctionInit(par=par, y=y, nstate=nstate, errortype=errortype, trendtype=trendtype,
  #         seasontype=seasontype, damped=damped, par.noopt=par.noopt, lowerb=lower, upperb=upper,
  #         opt.crit=opt.crit, nmse=nmse, bounds=bounds, m=m,pnames=names(par),pnames2=names(par.noopt))

  #     func <- .Call("etsGetTargetFunctionRmalschainsPtr", PACKAGE="forecast")

  #   }

  #   myBounds <- getNewBounds(par, lower, upper, nstate)

  #   if(is.null(control)) {
  #     control <- Rmalschains::malschains.control(ls="simplex", lsOnly=TRUE)
  #   }

  #   control$optimum <- if(opt.crit=="lik") -1e12 else 0

  #   fredTmp <- Rmalschains::malschains(func, env=env, lower=myBounds$lower, upper=myBounds$upper,
  #       maxEvals=maxit, seed=seed, initialpop=par, control=control)

  #   fred <- NULL
  #   fred$par <- fredTmp$sol

  #   fit.par <- fred$par

  #   names(fit.par) <- names(par)

#  } else if (solver=="Rdonlp2") {
#
#    donlp2 <- NULL
#    myRequire("Rdonlp2")
#
#    env <- etsTargetFunctionInit(par=par, y=y, nstate=nstate, errortype=errortype, trendtype=trendtype,
#        seasontype=seasontype, damped=damped, par.noopt=par.noopt, lowerb=lower, upperb=upper,
#        opt.crit=opt.crit, nmse=nmse, bounds=bounds, m=m,pnames=names(par),pnames2=names(par.noopt))
#
#    func <- .Call("etsGetTargetFunctionRdonlp2Ptr", PACKAGE="forecast")
#
#    myBounds <- getNewBounds(par, lower, upper, nstate)
#
#    fred <- donlp2(par, func, env=env, par.lower=myBounds$lower, par.upper=myBounds$upper)#, nlin.lower=c(-1), nlin.upper=c(1)) #nlin.lower=c(0,-Inf, -Inf, -Inf), nlin.upper=c(0,0,0,0))
#
#    fit.par <- fred$par
#
#    names(fit.par) <- names(par)

#  } else if(solver=="optim_c"){

    env <- etsTargetFunctionInit(par=par, y=y, nstate=nstate, errortype=errortype, trendtype=trendtype,
        seasontype=seasontype, damped=damped, par.noopt=par.noopt, lowerb=lower, upperb=upper,
        opt.crit=opt.crit, nmse=as.integer(nmse), bounds=bounds, m=m,pnames=names(par),pnames2=names(par.noopt))

    fred <- .Call("etsNelderMead", par, env, -Inf,
        sqrt(.Machine$double.eps), 1.0, 0.5, 2.0, trace, maxit, PACKAGE="forecast")

    fit.par <- fred$par

    names(fit.par) <- names(par)

  # } else { #if(solver=="optim")

  #   # Optimize parameters and state
  #   if(length(par)==1)
  #     method <- "Brent"
  #   else
  #   	method <- "Nelder-Mead"

  #   fred <- optim(par,lik,method=method,y=y,nstate=nstate, errortype=errortype, trendtype=trendtype,
  #       seasontype=seasontype, damped=damped, par.noopt=par.noopt, lowerb=lower, upperb=upper,
  #       opt.crit=opt.crit, nmse=nmse, bounds=bounds, m=m,pnames=names(par),pnames2=names(par.noopt),
  #       control=list(maxit=maxit))

  #   fit.par <- fred$par
  #   names(fit.par) <- names(par)
  # }

#-------------------------------------------------

  init.state <- fit.par[(np-nstate+1):np]
  # Add extra state
  if(seasontype!="N")
    init.state <- c(init.state, m*(seasontype=="M") - sum(init.state[(2+(trendtype!="N")):nstate]))

  if(!is.na(fit.par["alpha"]))
    alpha <- fit.par["alpha"]
  if(!is.na(fit.par["beta"]))
    beta <- fit.par["beta"]
  if(!is.na(fit.par["gamma"]))
    gamma <- fit.par["gamma"]
  if(!is.na(fit.par["phi"]))
    phi <- fit.par["phi"]

  e <- pegelsresid.C(y,m,init.state,errortype,trendtype,seasontype,damped,alpha,beta,gamma,phi,nmse)

  np <- np + 1
  ny <- length(y)
  aic <- e$lik + 2*np
  bic <- e$lik + log(ny)*np
  aicc <- aic +  2*np*(np+1)/(ny-np-1)

  mse <- e$amse[1]
  amse <- mean(e$amse)

  states=ts(e$states,frequency=tsp.y[3],start=tsp.y[1]-1/tsp.y[3])
  colnames(states)[1] <- "l"
  if(trendtype!="N")
    colnames(states)[2] <- "b"
  if(seasontype!="N")
    colnames(states)[(2+(trendtype!="N")):ncol(states)] <- paste("s",1:m,sep="")

  tmp <- c("alpha",rep("beta",trendtype!="N"),rep("gamma",seasontype!="N"),rep("phi",damped))
  fit.par <- c(fit.par,par.noopt)
#    fit.par <- fit.par[order(names(fit.par))]
  if(errortype=="A")
    fits <- y-e$e
  else
    fits <- y/(1+e$e)

  return(list(loglik=-0.5*e$lik,aic=aic,bic=bic,aicc=aicc,mse=mse,amse=amse,fit=fred,residuals=ts(e$e,frequency=tsp.y[3],start=tsp.y[1]),fitted=ts(fits,frequency=tsp.y[3],start=tsp.y[1]),
          states=states,par=fit.par))
}


etsTargetFunctionInit <- function(par,y,nstate,errortype,trendtype,seasontype,damped,par.noopt,lowerb,upperb,
    opt.crit,nmse,bounds,m,pnames,pnames2)
{

  names(par) <- pnames
  names(par.noopt) <- pnames2
  alpha <- c(par["alpha"],par.noopt["alpha"])["alpha"]
  if(is.na(alpha))
    stop("alpha problem!")
  if(trendtype!="N")
  {
    beta <- c(par["beta"],par.noopt["beta"])["beta"]
    if(is.na(beta))
      stop("beta Problem!")
  }
  else
    beta <- NULL
  if(seasontype!="N")
  {
    gamma <- c(par["gamma"],par.noopt["gamma"])["gamma"]
    if(is.na(gamma))
      stop("gamma Problem!")
  }
  else
  {
    m <- 1
    gamma <- NULL
  }
  if(damped)
  {
    phi <- c(par["phi"],par.noopt["phi"])["phi"]
    if(is.na(phi))
      stop("phi Problem!")
  }
  else
    phi <- NULL

  #determine which values to optimize and which ones are given by the user/not needed
  optAlpha <- !is.null(alpha)
  optBeta <- !is.null(beta)
  optGamma <- !is.null(gamma)
  optPhi <- !is.null(phi)

  givenAlpha <- FALSE
  givenBeta <- FALSE
  givenGamma <- FALSE
  givenPhi <- FALSE

  if(!is.null(par.noopt["alpha"])) if(!is.na(par.noopt["alpha"])) {
      optAlpha <- FALSE
      givenAlpha <- TRUE
    }
  if(!is.null(par.noopt["beta"])) if(!is.na(par.noopt["beta"])) {
      optBeta <- FALSE
      givenBeta <- TRUE
    }
  if(!is.null(par.noopt["gamma"])) if(!is.na(par.noopt["gamma"])) {
      optGamma <- FALSE
      givenGamma <- TRUE
    }
  if(!is.null(par.noopt["phi"])) if(!is.na(par.noopt["phi"])) {
      optPhi <- FALSE
      givenPhi <- TRUE
    }


  if(!damped)
    phi <- 1;
  if(trendtype == "N")
    beta <- 0;
  if(seasontype == "N")
    gamma <- 0;

#  cat("alpha: ", alpha)
#  cat(" beta: ", beta)
#  cat(" gamma: ", gamma)
#  cat(" phi: ", phi, "\n")
#
#  cat("useAlpha: ", useAlpha)
#  cat(" useBeta: ", useBeta)
#  cat(" useGamma: ", useGamma)
#  cat(" usePhi: ", usePhi, "\n")

  env <- new.env()

  res <- .Call("etsTargetFunctionInit", y=y, nstate=nstate, errortype=switch(errortype,"A"=1,"M"=2),
      trendtype=switch(trendtype,"N"=0,"A"=1,"M"=2), seasontype=switch(seasontype,"N"=0,"A"=1,"M"=2),
      damped=damped, lowerb=lowerb, upperb=upperb,
      opt.crit=opt.crit, nmse=as.integer(nmse), bounds=bounds, m=m,
      optAlpha, optBeta, optGamma, optPhi,
      givenAlpha, givenBeta, givenGamma, givenPhi,
      alpha, beta, gamma, phi, env, PACKAGE="forecast")
  res
}



initparam <- function(alpha,beta,gamma,phi,trendtype,seasontype,damped,lower,upper,m)
{
  if(any(lower > upper))
    stop("Inconsistent parameter boundaries")

  # Select alpha
  if(is.null(alpha))
  {
    alpha <- lower[1] + 0.5*(upper[1]-lower[1])/m
    par <- c(alpha=alpha)
  }
  else
    par <- numeric(0)

  # Select beta
  if(trendtype !="N" & is.null(beta))
  {
    # Ensure beta < alpha
    upper[2] <- min(upper[2], alpha)
    beta <- lower[2] + 0.1*(upper[2]-lower[2])
    par <- c(par,beta=beta)
  }

  # Select gamma
  if(seasontype != "N" & is.null(gamma))
  {
    # Ensure gamma < 1-alpha
    upper[3] <- min(upper[3], 1-alpha)
    gamma <- lower[3] + 0.05*(upper[3]-lower[3])
    par <- c(par,gamma=gamma)
  }

  # Select phi
  if(damped & is.null(phi))
  {
    phi <- lower[4] + .99*(upper[4]-lower[4])
    par <- c(par,phi=phi)
  }

  return(par)
}

check.param <- function(alpha,beta,gamma,phi,lower,upper,bounds,m)
{
  if(bounds != "admissible")
  {
    if(!is.null(alpha))
    {
      if(alpha < lower[1] | alpha > upper[1])
        return(0)
    }
    if(!is.null(beta))
    {
      if(beta < lower[2] | beta > alpha | beta > upper[2])
        return(0)
    }
    if(!is.null(phi))
    {
      if(phi < lower[4] | phi > upper[4])
        return(0)
    }
    if(!is.null(gamma))
    {
      if(gamma < lower[3] | gamma > 1-alpha | gamma > upper[3])
        return(0)
    }
  }
  if(bounds != "usual")
  {
    if(!admissible(alpha,beta,gamma,phi,m))
      return(0)
  }
  return(1)
}

initstate <- function(y,trendtype,seasontype)
{
  if(seasontype!="N")
  {
    # Do decomposition
    m <- frequency(y)
    n <- length(y)
    if(n < 4)
      stop("You've got to be joking (not enough data).")
    else if(n < 3*m) # Fit simple Fourier model.
    {
      fouriery <- fourier(y,1)
      fit <- tslm(y ~ trend + fouriery)
      if(seasontype=="A")
        y.d <- list(seasonal=y -fit$coef[1] - fit$coef[2]*(1:n))
      else # seasontype=="M". Biased method, but we only need a starting point
        y.d <- list(seasonal=y / (fit$coef[1] + fit$coef[2]*(1:n)))
    }
    else # n is large enough to do a decomposition
      y.d <- decompose(y,type=switch(seasontype, A="additive", M="multiplicative"))

    init.seas <- rev(y.d$seasonal[2:m]) # initial seasonal component
    names(init.seas) <- paste("s",0:(m-2),sep="")
    # Seasonally adjusted data
    if(seasontype=="A")
      y.sa <- y-y.d$seasonal
    else
    {
      init.seas <- pmax(init.seas, 1e-2) # We do not want negative seasonal indexes
      if(sum(init.seas) > m)
      	init.seas <- init.seas/sum(init.seas + 1e-2)
      y.sa <- y/pmax(y.d$seasonal, 1e-2)
    }
  }
  else # non-seasonal model
  {
    m <- 1
    init.seas <- NULL
    y.sa <- y
  }

  maxn <- min(max(10,2*m),length(y.sa))

  if(trendtype=="N")
  {
    l0 <- mean(y.sa[1:maxn])
    b0 <- NULL
  }
  else  # Simple linear regression on seasonally adjusted data
  {
    fit <- lsfit(1:maxn,y.sa[1:maxn])
    if(trendtype=="A")
    {
      l0 <- fit$coef[1]
      b0 <- fit$coef[2]
      # If error type is "M", then we don't want l0+b0=0.
      # So perturb just in case.
      if(abs(l0+b0) < 1e-8)
      {
        l0 <- l0*(1+1e-3)
        b0 <- b0*(1-1e-3)
      }
    }
    else #if(trendtype=="M")
    {
      l0 <- fit$coef[1]+fit$coef[2] # First fitted value
      if(abs(l0) < 1e-8)
        l0 <- 1e-7
      b0 <- (fit$coef[1] + 2*fit$coef[2])/l0 # Ratio of first two fitted values
      l0 <- l0/b0 # First fitted value divided by b0
      if(abs(b0) > 1e10) # Avoid infinite slopes
        b0 <- sign(b0)*1e10
      if(l0 < 1e-8 | b0 < 1e-8) # Simple linear approximation didn't work.
      {
        l0 <- max(y.sa[1],1e-3)
        b0 <- max(y.sa[2]/y.sa[1],1e-3)
      }
    }
  }

  names(l0) <- "l"
  if(!is.null(b0))
    names(b0) <- "b"
  return(c(l0,b0,init.seas))
}


lik <- function(par,y,nstate,errortype,trendtype,seasontype,damped,par.noopt,lowerb,upperb,
    opt.crit,nmse,bounds,m,pnames,pnames2)
{

  #browser()

  #cat("par: ", par, "\n")

  names(par) <- pnames
  names(par.noopt) <- pnames2
  alpha <- c(par["alpha"],par.noopt["alpha"])["alpha"]
  if(is.na(alpha))
    stop("alpha problem!")
  if(trendtype!="N")
  {
    beta <- c(par["beta"],par.noopt["beta"])["beta"]
    if(is.na(beta))
      stop("beta Problem!")
  }
  else
    beta <- NULL
  if(seasontype!="N")
  {
    gamma <- c(par["gamma"],par.noopt["gamma"])["gamma"]
    if(is.na(gamma))
      stop("gamma Problem!")
  }
  else
  {
    m <- 1
    gamma <- NULL
  }
  if(damped)
  {
    phi <- c(par["phi"],par.noopt["phi"])["phi"]
    if(is.na(phi))
      stop("phi Problem!")
  }
  else
    phi <- NULL

  if(!check.param(alpha,beta,gamma,phi,lowerb,upperb,bounds,m))
    return(Inf)

  np <- length(par)

  init.state <- par[(np-nstate+1):np]
  # Add extra state
  if(seasontype!="N")
    init.state <- c(init.state, m*(seasontype=="M") - sum(init.state[(2+(trendtype!="N")):nstate]))
  # Check states
  if(seasontype=="M")
  {
    seas.states <- init.state[-(1:(1+(trendtype!="N")))]
    if(min(seas.states) < 0)
      return(Inf)
  }

  e <- pegelsresid.C(y,m,init.state,errortype,trendtype,seasontype,damped,alpha,beta,gamma,phi,nmse)

  if(is.na(e$lik))
    return(Inf)
  if(e$lik < -1e10) # Avoid perfect fits
    return(-1e10)

#      cat("lik: ", e$lik, "\n")
#    points(alpha,e$lik,col=2)

  if(opt.crit=="lik")
    return(e$lik)
  else if(opt.crit=="mse")
    return(e$amse[1])
  else if(opt.crit=="amse")
    return(mean(e$amse))
  else if(opt.crit=="sigma")
    return(mean(e$e^2))
  else if(opt.crit=="mae")
    return(mean(abs(e$e)))
}

#' @export
print.ets <- function(x,...)
{
  cat(paste(x$method, "\n\n"))
  cat(paste("Call:\n", deparse(x$call), "\n\n"))
  ncoef <- length(x$initstate)
  if(!is.null(x$lambda))
    cat("  Box-Cox transformation: lambda=",round(x$lambda,4), "\n\n")

  cat("  Smoothing parameters:\n")
  cat(paste("    alpha =", round(x$par["alpha"], 4), "\n"))
  if(x$components[2]!="N")
    cat(paste("    beta  =", round(x$par["beta"], 4), "\n"))
  if(x$components[3]!="N")
    cat(paste("    gamma =", round(x$par["gamma"], 4), "\n"))
  if(x$components[4]!="FALSE")
    cat(paste("    phi   =", round(x$par["phi"], 4), "\n"))

  cat("\n  Initial states:\n")
  cat(paste("    l =", round(x$initstate[1], 4), "\n"))
  if (x$components[2]!="N")
    cat(paste("    b =", round(x$initstate[2], 4), "\n"))
  else
  {
    x$initstate <- c(x$initstate[1], NA, x$initstate[2:ncoef])
    ncoef <- ncoef+1
  }
  if (x$components[3]!="N")
  {
    cat("    s=")
    if (ncoef <= 8)
      cat(round(x$initstate[3:ncoef], 4))
    else
    {
      cat(round(x$initstate[3:8], 4))
      cat("\n           ")
      cat(round(x$initstate[9:ncoef], 4))
    }
    cat("\n")
  }

  cat("\n  sigma:  ")
  cat(round(sqrt(x$sigma2),4))
  if(!is.null(x$aic))
  {
    stats <- c(x$aic,x$aicc,x$bic)
    names(stats) <- c("AIC","AICc","BIC")
    cat("\n\n")
    print(stats)
  }
#    cat("\n  AIC:    ")
#    cat(round(x$aic,4))
#    cat("\n  AICc:   ")
#    cat(round(x$aicc,4))
#    cat("\n  BIC:    ")
#    cat(round(x$bic,4))
}


pegelsresid.C <- function(y,m,init.state,errortype,trendtype,seasontype,damped,alpha,beta,gamma,phi,nmse)
{
  n <- length(y)
  p <- length(init.state)
  x <- numeric(p*(n+1))
  x[1:p] <- init.state
  e <- numeric(n)
  lik <- 0;
  if(!damped)
    phi <- 1;
  if(trendtype == "N")
    beta <- 0;
  if(seasontype == "N")
    gamma <- 0;

  amse <- numeric(nmse)

  Cout <- .C("etscalc",
      as.double(y),
      as.integer(n),
      as.double(x),
      as.integer(m),
      as.integer(switch(errortype,"A"=1,"M"=2)),
      as.integer(switch(trendtype,"N"=0,"A"=1,"M"=2)),
      as.integer(switch(seasontype,"N"=0,"A"=1,"M"=2)),
      as.double(alpha),
      as.double(beta),
      as.double(gamma),
      as.double(phi),
      as.double(e),
      as.double(lik),
      as.double(amse),
      as.integer(nmse),
      PACKAGE="forecast")
  if(!is.na(Cout[[13]]))
  {
    if(abs(Cout[[13]]+99999) < 1e-7)
      Cout[[13]] <- NA
  }
  tsp.y <- tsp(y)
  e <- ts(Cout[[12]])
  tsp(e) <- tsp.y

  return(list(lik=Cout[[13]], amse=Cout[[14]], e=e, states=matrix(Cout[[3]], nrow=n+1, ncol=p, byrow=TRUE)))
}

admissible <- function(alpha,beta,gamma,phi,m)
{
  if(is.null(phi))
    phi <- 1
  if(phi < 0 | phi > 1+1e-8)
    return(0)
  if(is.null(gamma))
  {
    if(alpha < 1-1/phi | alpha > 1+1/phi)
      return(0)
    if(!is.null(beta))
    {
      if(beta < alpha * (phi-1) | beta > (1+phi)*(2-alpha))
        return(0)
    }
  }
  else if(m > 1) # Seasonal model
  {
    if(is.null(beta))
      beta <- 0
    if(gamma < max(1-1/phi-alpha,0) | gamma > 1+1/phi-alpha)
      return(0)
    if(alpha < 1-1/phi-gamma*(1-m+phi+phi*m)/(2*phi*m))
      return(0)
    if(beta < -(1-phi)*(gamma/m+alpha))
      return(0)

    # End of easy tests. Now use characteristic equation
    P <- c(phi*(1-alpha-gamma),alpha+beta-alpha*phi+gamma-1,rep(alpha+beta-alpha*phi,m-2),(alpha+beta-phi),1)
    roots <- polyroot(P)

    #cat("maxpolyroots: ", max(abs(roots)), "\n")

    if(max(abs(roots)) > 1+1e-10)
      return(0)
  }
  # Passed all tests
  return(1)
}

### PLOT COMPONENTS


#' Plot components from ETS model
#'
#' Produces a plot of the level, slope and seasonal components from an ETS
#' model.
#'
#' \code{autoplot} will produce an equivelant plot as a ggplot object.
#'
#' @param x Object of class \dQuote{ets}.
#' @param object Object of class \dQuote{ets}. Used for ggplot graphics (S3
#' method consistency).
#' @param range.bars Logical indicating if each plot should have a bar at its
#' right side representing relative size. If NULL, automatic selection takes
#' place.
#' @param ... Other plotting parameters to affect the plot.
#' @return None. Function produces a plot
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso \code{\link{ets}}
#' @keywords hplot
#' @examples
#'
#' fit <- ets(USAccDeaths)
#' plot(fit)
#' plot(fit,plot.type="single",ylab="",col=1:3)
#'
#' library(ggplot2)
#' autoplot(fit)
#'
#' @export
plot.ets <- function(x,...)
{
  if(!is.null(x$lambda))
    y <- BoxCox(x$x,x$lambda)
  else
    y <- x$x
  if(x$components[3]=="N" & x$components[2]=="N")
  {
    plot(cbind(observed=y, level=x$states[,1]),
        main=paste("Decomposition by",x$method,"method"),...)
  }
  else if(x$components[3]=="N")
  {
    plot(cbind(observed=y, level=x$states[,1], slope=x$states[,"b"]),
        main=paste("Decomposition by",x$method,"method"),...)
  }
  else if(x$components[2]=="N")
  {
    plot(cbind(observed=y, level=x$states[,1], season=x$states[,"s1"]),
        main=paste("Decomposition by",x$method,"method"),...)
  }
  else
  {
    plot(cbind(observed=y, level=x$states[,1], slope=x$states[,"b"],
            season=x$states[,"s1"]),
        main=paste("Decomposition by",x$method,"method"),...)
  }
}

#' @export
summary.ets <- function(object,...)
{
  print(object)
  cat("\nTraining set error measures:\n")
  print(accuracy(object))
}

#' @export
coef.ets <- function(object,...)
{
  object$par
}

#' @rdname fitted.Arima
#' @export
fitted.ets <- function(object, h=1, ...){
  if(h==1){
    return(object$fitted)
  }
  else{
    return(hfitted(object=object, h=h, FUN="ets", ...))
  }
}

#' @export
logLik.ets <- function(object,...)
{
  structure(object$loglik,df=length(object$par)+1,class="logLik")
}

#' @export
nobs.ets <- function(object, ...)
{
  length(object$x)
}



#' Is an object a particular model type?
#'
#' Returns true if the model object is of a particular type
#'
#' @param x object to be tested
#' @export
is.ets <- function(x){
  inherits(x, "ets")
}

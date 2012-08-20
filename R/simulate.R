simulate.ets <- function(object, nsim=length(object$x), seed=NULL, future=TRUE, bootstrap=FALSE,...)
{
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else
  {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  if(is.null(tsp(object$x)))
    object$x <- ts(object$x,frequency=1,start=1)

  if(future)
    initstate <- object$state[length(object$x)+1,]

  else # choose a random starting point
    initstate <- object$state[sample(1:length(object$x),1),]

  if(bootstrap)
    e <- sample(object$residuals,nsim,replace=TRUE)
  else
    e <- rnorm(nsim,0,sqrt(object$sigma))
  if(object$components[1]=="M")
    e <- pmax(-1,e)
  tmp <- ts(.C("etssimulate",
      as.double(initstate),
      as.integer(object$m),
      as.integer(switch(object$components[1],"A"=1,"M"=2)),
      as.integer(switch(object$components[2],"N"=0,"A"=1,"M"=2)),
      as.integer(switch(object$components[3],"N"=0,"A"=1,"M"=2)),
      as.double(object$par["alpha"]),
      as.double(ifelse(object$components[2]=="N",0,object$par["beta"])),
      as.double(ifelse(object$components[3]=="N",0,object$par["gamma"])),
      as.double(ifelse(object$components[4]=="FALSE",1,object$par["phi"])),
      as.integer(nsim),
      as.double(numeric(nsim)),
      as.double(e),
    PACKAGE="forecast")[[11]],frequency=object$m,start=tsp(object$x)[2]+1/tsp(object$x)[3])
  if(is.na(tmp[1]))
    stop("Problem with multiplicative damped trend")
  if(!is.null(object$lambda))
    tmp <- InvBoxCox(tmp,object$lambda)
  return(tmp)
}


# Simulate ARIMA model starting with observed data x
# Some of this function is borrowed from the arima.sim() function in the stats package.
# Note that myarima.sim() does simulation conditional on the values of observed x, whereas
# arima.sim() is unconditional on any observed x.

myarima.sim <- function (model, n, x, e, ...)
{
  start.innov <- residuals(model)
  innov <- e
  data <- x
  # Remove initial NAs
  first.nonmiss <- which(!is.na(x))[1]
  if(first.nonmiss > 1)
  {
    tsp.x <- tsp(x)
    start.x <- tsp.x[1] + (first.nonmiss-1)/tsp.x[3]
    x <- window(x,start=start.x)
    start.innov <- window(start.innov, start=start.x)
  }
  model$x <- x
  n.start <- length(x)
  x <- ts(c(start.innov, innov), start = 1 - n.start, frequency=model$seasonal.period)
  flag.noadjust <- FALSE
  if(is.null(tsp(data)))
    data <- ts(data,frequency=1,start=1)
  if (!is.list(model))
    stop("'model' must be list")
  p <- length(model$ar)
  if (p)
  {
    minroots <- min(Mod(polyroot(c(1, -model$ar))))
    if (minroots <= 1)
      stop("'ar' part of model is not stationary")
  }
  q <- length(model$ma)
  d <- 0
  if (!is.null(ord <- model$order))
  {
    if (length(ord) != 3L)
      stop("'model$order' must be of length 3")
    if (p != ord[1L])
      stop("inconsistent specification of 'ar' order")
    if (q != ord[3L])
      stop("inconsistent specification of 'ma' order")
    d <- ord[2L]
    if (d != round(d) || d < 0)
      stop("number of differences must be a positive integer")
  }
  if (length(model$ma))
  {
    #MA filtering
    x <- filter(x, c(1, model$ma), method="convolution", sides = 1L)
    x[seq_along(model$ma)] <- 0
  }
  ##AR "filtering"
  len.ar <- length(model$ar)

  if(length(model$ar) & (len.ar <= length(data)))
  {
    if((model$seasonal.difference != 0) && (d != 0))
    {
      diff.data <- diff(data, lag=1, differences = d)
      diff.data <- diff(diff.data, lag=model$seasonal.period, differences = model$seasonal.difference)
    }
    else if((model$seasonal.difference != 0) && (d == 0))
    {
      diff.data <- diff(data, lag=model$seasonal.period, differences = model$seasonal.difference)
    }
    else if((model$seasonal.difference == 0) && (d != 0))
      diff.data <- diff(data, lag=1, differences = d)
    else
      diff.data <- data

    x.new.innovations <- x[(length(start.innov)+1):length(x)]
    x.with.data <- c(diff.data, x.new.innovations)

    for(i in (length(diff.data)+1):length(x.with.data))
    {
      lagged.x.values <- x.with.data[(i-len.ar):(i-1)]
      ar.coefficients <- model$ar[length(model$ar):1]
      sum.mutliplied.x <- sum(lagged.x.values * (ar.coefficients))
      x.with.data[i] <- x.with.data[i]+sum.mutliplied.x
    }

    x.end <- x.with.data[(length(diff.data)+1):length(x.with.data)]
    x <- ts(x.end, start = 1, frequency=model$seasonal.period)
    flag.noadjust <- TRUE
  }
  else if (length(model$ar)) # but data too short
  {
    #AR filtering for all other cases where AR is used.
    x <- filter(x, model$ar, method = "recursive")
  }
  if((d == 0) && (model$seasonal.difference == 0) && (flag.noadjust==FALSE)) # Adjust to ensure end matches approximately
  {
    # Last 20 diffs
    if(n.start >= 20)
      xdiff <- (model$x - x[1:n.start])[n.start-(19:0)]
    else
      xdiff <- model$x - x[1:n.start]
    # If all same sign, choose last
    if(all(sign(xdiff)==1) | all(sign(xdiff)==-1))
      xdiff <- xdiff[length(xdiff)]
    else # choose mean.
      xdiff <- mean(xdiff)
    x <- x + xdiff
  }
  if ((n.start > 0) && (flag.noadjust==FALSE))
  {
    x <- x[-(1:n.start)]
  }
  ##
  #####
  #Seasonal undifferencing, if there is no regular differencing
  if((model$seasonal.difference > 0) && (d == 0))
  {
    i <- length(data)-model$seasonal.difference*model$seasonal.period+1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag=model$seasonal.period, differences=model$seasonal.difference, xi=seasonal.xi)[-(1:length.s.xi)]
    data.new <- data
  }
  else
  {
    data.new <- data
  }
  ###End seasonal undifferencing

  ##Regular undifferencing, if there is no seasonal differencing
  if (d > 0 && (model$seasonal.difference == 0))
  {
  x <- diffinv(x, differences = d,xi=data.new[length(data.new)-(d:1)+1])[-(1:d)]
  }

  ########
  #Code for Undifferencing for where the differencing is both Seasonal and Non-Seasonal (Non-Seasonal First)
  #Regular first
  if((d > 0) && (model$seasonal.difference > 0))
  {
    delta.four <- diff(data, lag=model$seasonal.period, differences = model$seasonal.difference)
    regular.xi <- delta.four[(length(delta.four)-model$seasonal.difference):length(delta.four)]
    x <- diffinv(x, differences = d, xi=regular.xi[length(regular.xi)-(d:1)+1])[-(1:d)]
  }

  #Then seasonal
  if((model$seasonal.difference > 0) && (d > 0))
  {
    i <- length(data)-model$seasonal.difference*model$seasonal.period+1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag=model$seasonal.period, differences=model$seasonal.difference, xi=seasonal.xi)
    x <- x[-(1:length.s.xi)]
    data.new <- data
  }

  ########

  x <- ts(x[1:n],frequency=frequency(data),start=tsp(data)[2]+1/tsp(data)[3])
  return(x)
}

simulate.Arima <- function(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, bootstrap=FALSE, ...)
{
  #Error check:
  if(object$arma[7] < 0)
  {
    stop("Value for seasonal difference is < 0. Must be >= 0")
  }
  else if((sum(object$arma[c(3,4,7)])>0) && (object$arma[5] < 2))
  {
    stop("Invalid value for seasonal period")
  }

  ####
  #Random Seed Code
  if (!exists(".Random.seed", envir = .GlobalEnv))
    runif(1)
  if (is.null(seed))
    RNGstate <- .Random.seed
  else
  {
    R.seed <- .Random.seed
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  #############End Random seed code


  #Check for seasonal ARMA components and set flag accordingly. This will be used later in myarima.sim()
  flag.s.arma <- (sum(object$arma[c(3,4)])>0)
  #Check for Seasonality in ARIMA model
  if(sum(object$arma[c(3,4,7)])>0)
  {
    #return(simulateSeasonalArima(object, nsim=nsim, seed=seed, xreg=xreg, future=future, bootstrap=bootstrap, ...))
    if(sum(object$model$phi) == 0)
    {
      ar <- NULL
    }
    else
    {
      ar <- as.double(object$model$phi)
    }
    if(sum(object$model$theta) == 0)
    {
      ma <- NULL
    }
    else
    {
      ma <- as.double(object$model$theta)
    }
    order <- c(length(ar),object$arma[6],length(ma))

    if(future)
    {
      model <- list(order=order, ar=ar, ma=ma,sd=sqrt(object$sigma2),residuals=residuals(object), seasonal.difference=object$arma[7], seasonal.period=object$arma[5], flag.seasonal.arma=flag.s.arma, seasonal.order=object$arma[c(3,7,4)])
    }
    else
    {
      model <- list(order=order, ar=ar, ma=ma,sd=sqrt(object$sigma2),residuals=residuals(object))
    }
    flag.seasonal.diff <- (object$arma[7] > 0)
  }
  else
  {
    ####Non-Seasonal ARIMA specific code: Set up the model
    order <- object$arma[c(1, 6, 2)]
    if(order[1]>0)
      ar <- object$model$phi[1:order[1]]
    else
      ar <- NULL
    if(order[3]>0)
      ma <- object$model$theta[1:order[3]]
    else
      ma <- NULL
    if(object$arma[2] != length(ma))
      stop("MA length wrong")
    else if(object$arma[1] != length(ar))
      stop("AR length wrong")

    if(future)
    {
      model <- list(order=object$arma[c(1, 6, 2)],ar=ar,ma=ma,sd=sqrt(object$sigma2),residuals=residuals(object), seasonal.difference=0, flag.seasonal.arma=flag.s.arma, seasonal.order=c(0,0,0), seasonal.period=1)
    }
    else
    {
      model <- list(order=object$arma[c(1, 6, 2)],ar=ar,ma=ma,sd=sqrt(object$sigma2),residuals=residuals(object))
    }
    flag.seasonal.diff <- FALSE
    ###End non-seasonal ARIMA specific code
  }



  if (is.element("x", names(object)))
    x <- object$x
  else
    x <- object$x <- eval.parent(parse(text = object$series))

  if(is.null(tsp(x)))
    x <- ts(x,frequency=1,start=1)

  if(!is.null(object$lambda))
    x <- BoxCox(x,object$lambda)

  n <- length(x)
  d <- order[2]
  if(bootstrap)
    e <- sample(model$residuals,nsim+d,replace=TRUE)
  else
    e <- rnorm(nsim+d, 0, model$sd)

  use.drift <- is.element("drift", names(object$coef))
  usexreg <- (!is.null(xreg) | use.drift)
  if (!is.null(xreg))
  {
    xreg <- as.matrix(xreg)
    if(nrow(xreg) < nsim)
      stop("Not enough rows in xreg")
    else
      xreg <- xreg[1:nsim,]
  }
  if (use.drift)
  {
    dft <- as.matrix(1:nsim) + n
    xreg <- cbind(xreg, dft)
  }
  narma <- sum(object$arma[1L:4L])
  if(length(object$coef) > narma)
  {
    if (names(object$coef)[narma + 1L] == "intercept")
    {
      xreg <- cbind(intercept = rep(1, nsim), xreg)
      object$xreg <- cbind(intercept = rep(1, n), object$xreg)
    }
    if(!is.null(xreg))
    {
      xm <- if (narma == 0)
          drop(as.matrix(xreg) %*% object$coef)
        else
          drop(as.matrix(xreg) %*% object$coef[-(1L:narma)])
      oldxm <- if(narma == 0)
            drop(as.matrix(object$xreg) %*% object$coef)
          else
            drop(as.matrix(object$xreg) %*% object$coef[-(1L:narma)])
    }
  }
  else
  {
    xm <- oldxm <- 0
  }
  if(future)
  {
    sim <- myarima.sim(model,nsim,x-oldxm,e=e) + xm
  }
  else
  {
    if(flag.seasonal.diff)
    {
      zeros <- object$arma[5]*object$arma[7]
      sim <- arima.sim(model,nsim,innov=e)
      sim <- diffinv(sim, lag=object$arma[5], differences=object$arma[7])[-(1:zeros)]
      sim <- sim + xm
    }
    else
    {
      sim <- arima.sim(model,nsim,innov=e) + xm
    }
  }
  if(!is.null(object$lambda))
    sim <- InvBoxCox(sim,object$lambda)
  return(sim)
}

simulate.ar <- function(object, nsim=object$n.used, seed=NULL, future=TRUE, bootstrap=FALSE, ...)
{
  if (!exists(".Random.seed", envir = .GlobalEnv))
    runif(1)
  if (is.null(seed))
    RNGstate <- .Random.seed
  else
  {
    R.seed <- .Random.seed
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  if(future)
  {
      model <- list(ar=object$ar,sd=sqrt(object$var.pred),residuals=object$resid, seasonal.difference=0, seasonal.period=1, flag.seasonal.arma=FALSE)
  }
  else
  {
    model <- list(ar=object$ar,sd=sqrt(object$var.pred),residuals=object$resid)
  }
  x.mean <- object$x.mean
  if (!is.element("x", names(object)))
    object$x <- eval.parent(parse(text = object$series))
  if(is.null(tsp(object$x)))
    object$x <- ts(object$x,frequency=1,start=1)
  object$x <- eval.parent(parse(text = object$series)) - x.mean
  if(bootstrap)
    e <- sample(model$residuals,nsim,replace=TRUE)
  else
    e <- rnorm(nsim, 0, model$sd)
  if(future)
    return(myarima.sim(model,nsim,x=object$x,e=e) + x.mean)
  else
    return(arima.sim(model,nsim,innov=e) + x.mean)
}

simulate.fracdiff <- function(object, nsim=object$n, seed=NULL, future=TRUE, bootstrap=FALSE, ...)
{
  if (is.element("x", names(object)))
    x <- object$x
  else
    x <- object$x <- eval.parent(parse(text = as.character(object$call)[2]))
  if(is.null(tsp(x)))
    x <- ts(x,frequency=1,start=1)

  # Strip initial and final missing values
  xx <- na.ends(x)
  n <- length(xx)

  # Remove mean
  meanx <- mean(xx)
  xx <- xx - meanx

  y <- undo.na.ends(x,diffseries(xx, d = object$d))
  fit <- arima(y, order = c(length(object$ar), 0, length(object$ma)),
    include.mean = FALSE, fixed = c(object$ar, -object$ma))
  # Simulate ARMA
  ysim <- simulate(fit,nsim,seed,future=future,bootstrap=bootstrap)
  # Undo differencing
  return(unfracdiff(xx,ysim,n,nsim,object$d))
  # bin.c <- (-1)^(0:(n + nsim)) * choose(object$d, (0:(n + nsim)))
  # b <- numeric(n)
  # xsim <- LHS <- numeric(nsim)
  # RHS <- cumsum(ysim)
  # bs <- cumsum(bin.c[1:nsim])
  # b <- bin.c[(1:n) + 1]
  # xsim[1] <- RHS[1] <- ysim[1] - sum(b * rev(xx))
  # for (k in 2:nsim)
  # {
    # b <- b + bin.c[(1:n) + k]
    # RHS[k] <- RHS[k] - sum(b * rev(xx))
    # LHS[k] <- sum(rev(xsim[1:(k - 1)]) * bs[2:k])
    # xsim[k] <- RHS[k] - LHS[k]
  # }
  # tspx <- tsp(x)
  # return(ts(xsim,frequency=tspx[3],start=tspx[2]+1/tspx[3]))
}





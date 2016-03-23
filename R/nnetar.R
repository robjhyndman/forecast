# Defaults:
#For non-seasonal data, p chosen using AIC from linear AR(p) model
#For seasonal data, p chosen using AIC from linear AR(p) model after
#    seasonally adjusting with STL decomposition, and P=1
#size set to average of number of inputs and number of outputs: (p+P+1)/2
#if xreg is included then size = (p+P+ncol(xreg)+1)/2

nnetar <- function(x, p, P=1, size, repeats=20, xreg=NULL, lambda=NULL, model=NULL, scale.inputs=TRUE, ...)
{
  useoldmodel <- FALSE
  if (!is.null(model))
  {
    # Use previously fitted model
    useoldmodel <- TRUE
    # Check for conflicts between new and old data:
    # Check model class
    if (!is.nnetar(model))
      stop("Model must be a nnetar object")
    # Check new data
    m <- frequency(model$x)
    minlength <- max(c(model$p, model$P*m))
    if (length(x) < minlength)
      stop(paste("Series must be at least of length", minlength, "to use fitted model"))
    x <- as.ts(x)
    if (tsp(x)[3] != m)
    {
      warning(paste("Data frequency doesn't match fitted model, coercing to frequency =", m))
      x <- ts(x, frequency=m)
    }
    # Check xreg
    if (!is.null(model$xreg))
    {
      if (is.null(xreg))
        stop("No external regressors provided")
      if (NCOL(xreg) != NCOL(model$xreg))
        stop("Number of external regressors does not match fitted model")
    }
    # Update parameters with previous model
    lambda <- model$lambda
    size <- model$size
    p <- model$p
    P <- model$P
    if (is.null(model$scalex))
      scale.inputs <- FALSE
  }
  # Check for NAs in x
  if (any(is.na(x)))
    warning("Missing values in x, omitting rows")
  # Transform data
  if(!is.null(lambda))
    xx <- BoxCox(x,lambda)
  else
    xx <- x
  # Scale series
  scalex <- NULL
  if(scale.inputs)
  {
    if (useoldmodel)
    {
      xx <- scale(xx, center = model$scalex$mean, scale = model$scalex$sd)
      scalex <- list(mean = attr(xx,"scaled:center"),
                     sd = attr(xx,"scaled:scale"))
    }
    else
    {
      xx <- scale(xx, center = TRUE, scale = TRUE)
      scalex <- list(mean = attr(xx,"scaled:center"),
                     sd = attr(xx,"scaled:scale"))
    }
    xx <- xx[,1]
  }
  # Check xreg class & dim
  xxreg <- NULL
  scalexreg <- NULL
  if(!is.null(xreg))
  {
    xreg <- as.matrix(xreg)
    if(length(x) != NROW(xreg))
      stop("Number of rows in xreg does not match series length")
    # Check for NAs in xreg
    if(any(is.na(xreg)))
      warning("Missing values in xreg, omitting rows")
    # Scale xreg
    if(scale.inputs)
    {
      if (useoldmodel)
      {
        xxreg <- scale(xreg, center = model$scalexreg$mean, scale = model$scalexreg$sd)
        scalexreg <- list(mean = attr(xxreg,"scaled:center"),
                          sd = attr(xxreg,"scaled:scale"))
      }
      else
      {
        xxreg <- scale(xreg, center = TRUE, scale = TRUE)
        scalexreg <- list(mean = attr(xxreg,"scaled:center"),
                          sd = attr(xxreg,"scaled:scale"))
      }
    }
    else
    {
      xxreg <- xreg
    }
  }
  # Set up lagged matrix
  n <- length(xx)
  xx <- as.ts(xx)
  m <- frequency(xx)
  if(m==1)
  {
    if(missing(p))
      p <- max(length(ar(na.interp(xx))$ar),1)
    lags <- 1:p
    P <- 0
  }
  else
  {
    if(missing(p))
    {
      x.sa <- seasadj(stl(na.interp(xx),s.window=7))
      p <- max(length(ar(x.sa)$ar),1)
    }
    if(P > 0)
      lags <- sort(unique(c(1:p,m*(1:P))))
    else
      lags <- 1:p
  }
  maxlag <- max(lags)
  nlag <- length(lags)
  y <- xx[-(1:maxlag)]
  lags.X <- matrix(NA_real_,ncol=nlag,nrow=n-maxlag)
  for(i in 1:nlag)
    lags.X[,i] <- xx[(maxlag-lags[i]+1):(n-lags[i])]
  # Add xreg into lagged matrix
  lags.X <- cbind(lags.X, xxreg[-(1:maxlag), ])
  if(missing(size))
    size <- round((NCOL(lags.X)+1)/2)
  # Remove missing values if present
  j <- complete.cases(lags.X,y)
  ## Fit average ANN.
  if(useoldmodel)
    fit <- oldmodel_avnnet(lags.X[j,],y[j],size=size, model)
  else
    fit <- avnnet(lags.X[j,],y[j],size=size,linout=1,trace=FALSE,repeats=repeats, ...)
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$scalex <- scalex
  out$scalexreg <- scalexreg
  out$size <- size
  out$xreg <- xreg
  out$lambda <- lambda
  out$model <- fit
  out$nnetargs <- list(...)
  if (useoldmodel)
    out$nnetargs <- model$nnetargs
  fits <- c(rep(NA_real_,maxlag), rowMeans(sapply(fit, predict)))
  if(scale.inputs)
    fits <- fits * scalex$sd + scalex$mean
  fits <- ts(fits)
  if(!is.null(lambda))
    fits <- InvBoxCox(fits,lambda)
  out$fitted <- ts(rep(NA_real_, length(out$x)))
  out$fitted[c(rep(TRUE, maxlag), j)] <- fits
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- deparse(substitute(x))
  out$method <- paste("NNAR(",p,sep="")
  if(P>0)
    out$method <- paste(out$method,",",P,sep="")
  out$method <- paste(out$method,",",size,")",sep="")
  out$call <- match.call()
  return(structure(out,class=c("nnetar")))
}

# Aggregate several neural network models
avnnet <- function(x,y,repeats, ...)
{
  mods <- list()
  for(i in 1:repeats)
    mods[[i]] <- nnet::nnet(x, y, ...)
  return(structure(mods,class="nnetarmodels"))
}

# Fit old model to new data
oldmodel_avnnet <- function(x, y, size, model)
{
  repeats <- length(model$model)
  args <- list(x=x, y=y, size=size, linout=1, trace=FALSE)
  # include additional nnet arguments
  args <- c(args, model$nnetargs)
  # set iterations to zero (i.e. weights stay fixed)
  args$maxit <- 0
  mods <- list()
  for(i in 1:repeats)
  {
    args$Wts <- model$model[[i]]$wts
    mods[[i]] <- do.call(nnet::nnet, args)
  }
  return(structure(mods,class="nnetarmodels"))
}

print.nnetarmodels <- function(x, ...)
{
  cat(paste("\nAverage of",length(x),"networks, each of which is\n"))
  print(x[[1]])
}


forecast.nnetar <- function(object, h=ifelse(object$m > 1, 2 * object$m, 10), xreg=NULL, lambda=object$lambda, ...)
{
#  require(nnet)
  out <- object
  tspx <- tsp(out$x)

  # Check if xreg was used in fitted model
  if(is.null(object$xreg))
  {
    if(!is.null(xreg))
      warning("External regressors were not used in fitted model, xreg will be ignored")
    xreg <- NULL
  }
  else
  {
    if(is.null(xreg))
      stop("No external regressors provided")
    xreg <- as.matrix(xreg)
    if(NCOL(xreg) != NCOL(object$xreg))
      stop("Number of external regressors does not match fitted model")
    h <- NROW(xreg)
  }
  fcast <- numeric(h)
  xx <- object$x
  if(!is.null(lambda))
    xx <- BoxCox(xx,lambda)
  if(!is.null(object$scalex))
  {
    xx <- scale(xx, center = object$scalex$mean, scale = object$scalex$sd)
    if(!is.null(xreg))
      xreg <- scale(xreg, center = object$scalexreg$mean, scale = object$scalexreg$sd)
  }
  flag <- rev(tail(xx, n=max(object$lags)))
  for(i in 1:h)
  {
    fcast[i] <- mean(sapply(object$model, predict, newdata=c(flag[object$lags], xreg[i, ])))
    flag <- c(fcast[i],flag[-length(flag)])
  }
  if(!is.null(object$scalex))
    fcast <- fcast * object$scalex$sd + object$scalex$mean
  out$mean <- ts(fcast,start=tspx[2]+1/tspx[3],frequency=tspx[3])
  if(!is.null(lambda))
    out$mean <- InvBoxCox(out$mean,lambda)
  return(structure(out,class="forecast"))
}

print.nnetar <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  #cat("  one hidden layer with",x$size,"nodes\n")
  cat("Call:   ")
  print(x$call)
  print(x$model)
  cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits),
      "\n", sep = "")
  invisible(x)
}

is.nnetar <- function(x){
  inherits(x, "nnetar")
}

is.nnetarmodels <- function(x){
  inherits(x, "nnetarmodels")
}

# Scale a univariate time series
scale.ts <- function(x, center=TRUE, scale=TRUE)
{
  tspx <- tsp(x)
  x <- as.ts(scale.default(x, center=center, scale=scale))
  tsp(x) <- tspx
  return(x)
}

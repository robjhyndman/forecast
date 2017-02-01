## Generic forecast functions
## Part of forecast and demography packages

forecast <- function(object,...) UseMethod("forecast")

forecast.default <- function(object,...) forecast.ts(object,...)

## A function determining the appropriate period, if the data is of unknown period
## Written by Rob Hyndman
findfrequency <- function(x)
{
  n <- length(x)
  x <- as.ts(x)
  # Remove trend from data
  x <- residuals(tslm(x ~ trend))
  # Compute spectrum by fitting ar model to largest section of x
  n.freq <- 500
  spec <- spec.ar(c(na.contiguous(x)), plot=FALSE, n.freq=n.freq)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5)
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[(j[1]+1):n.freq])
        if(nextmax < length(spec$freq))
          period <- floor(1/spec$freq[nextmax] + 0.5)
        else
          period <- 1L
      }
      else
        period <- 1L
    }
  }
  else
    period <- 1L

  return(as.integer(period))
}

forecast.ts <- function(object, h=ifelse(frequency(object)>1, 2*frequency(object), 10),
  level=c(80,95), fan=FALSE, robust=FALSE, lambda = NULL, find.frequency = FALSE,
  allow.multiplicative.trend=FALSE, model=NULL, ...)
{
  n <- length(object)
  if (find.frequency) {
    object <- ts(object, frequency = findfrequency(object))
    obj.freq <- frequency(object)
  } else {
    obj.freq <- frequency(object)
  }
  if(robust)
    object <- tsclean(object, replace.missing=TRUE, lambda = lambda)

  if(!is.null(model))
  {
    if(inherits(model, "forecast")){
      model <- model$model
    }
    if(inherits(model, "ets"))
      fit <- ets(object, model=model, ...)
    else if(inherits(model, "Arima"))
      fit <- Arima(object, model=model, ...)
    else if(inherits(model, "bats"))
      fit <- bats(object, model=model, ...)
    else if(inherits(model, "tbats"))
      fit <- tbats(object, model=model, ...)
    else if(inherits(model, "nnetar"))
      fit <- nnetar(object, model=model, ...)
    else
      stop("Unknown model class")
    return(forecast(fit,h=h,level=level,fan=fan))
  }

  if(n > 3)
  {
    if(obj.freq < 13)
      out <- forecast(ets(object,lambda = lambda, allow.multiplicative.trend=allow.multiplicative.trend, ...),
        h=h,level=level,fan=fan)
    else if(n > 2*obj.freq)
      out <- stlf(object,h=h,level=level,fan=fan,lambda = lambda,
        allow.multiplicative.trend=allow.multiplicative.trend,...)
    else
      out <- forecast(ets(object, model="ZZN", lambda = lambda, allow.multiplicative.trend=allow.multiplicative.trend, ...),
        h=h,level=level,fan=fan)

  }
  else
    out <- meanf(object,h=h,level=level,fan=fan,lambda = lambda, ...)
  out$series <- deparse(substitute(object))
  return(out)
}

as.data.frame.mforecast <- function(x, ...)
{
  tmp <- lapply(x$forecast, as.data.frame)
  series <- names(tmp)
  times <- rownames(tmp[[1]])
  h <- NROW(tmp[[1]])
  output <- cbind(Time=times, Series=rep(series[1],h), tmp[[1]])
  if(length(tmp)>1)
  {
    for(i in 2:length(tmp))
      output <- rbind(output,
            cbind(Time=times, Series=rep(series[i],h), tmp[[i]]))
  }
  rownames(output) <- NULL
  return(output)
}

as.data.frame.forecast <- function(x,...)
{
    nconf <- length(x$level)
    out <- matrix(x$mean, ncol=1)
    ists <- is.ts(x$mean)
    fr.x <- frequency(x$mean)
    if(ists)
    {
        out <- ts(out)
        attributes(out)$tsp <- attributes(x$mean)$tsp
    }
    names <- c("Point Forecast")
    if (!is.null(x$lower) & !is.null(x$upper) & !is.null(x$level))
    {
        x$upper <- as.matrix(x$upper)
        x$lower <- as.matrix(x$lower)
        for (i in 1:nconf)
        {
            out <- cbind(out, x$lower[, i,drop=FALSE], x$upper[, i,drop=FALSE])
            names <- c(names, paste("Lo", x$level[i]), paste("Hi", x$level[i]))
        }
    }
    colnames(out) <- names
    tx <- time(x$mean)
    if(max(abs(tx-round(tx))) < 1e-11)
      nd <- 0
    else
      nd <- max(round(log10(fr.x)+1),2)
    rownames(out) <- format(tx, nsmall=nd, digits=nd)
    # Rest of function borrowed from print.ts(), but with header() omitted
    if(!ists)
        return(as.data.frame(out))

    x <- as.ts(out)
    calendar <- any(fr.x == c(4, 12)) && length(start(x)) ==  2L
    Tsp <- tsp(x)
    if (is.null(Tsp))
    {
        warning("series is corrupt, with no 'tsp' attribute")
        print(unclass(x))
        return(invisible(x))
    }
    nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
    if (NROW(x) != nn)
    {
        warning(gettextf("series is corrupt: length %d with 'tsp' implying %d", NROW(x), nn), domain=NA, call.=FALSE)
        calendar <- FALSE
    }
    if (NCOL(x) == 1)
    {
        if (calendar)
        {
            if (fr.x > 1)
            {
                dn2 <- if (fr.x == 12)
                  month.abb
                else if (fr.x == 4)
                  c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                else paste("p", 1L:fr.x, sep="")
                if (NROW(x) <= fr.x && start(x)[1L] == end(x)[1L])
                {
                  dn1 <- start(x)[1L]
                  dn2 <- dn2[1 + (start(x)[2L] - 2 + seq_along(x))%%fr.x]
                  x <- matrix(format(x, ...), nrow=1L, byrow=TRUE,
                    dimnames=list(dn1, dn2))
                }
                else
                {
                  start.pad <- start(x)[2L] - 1
                  end.pad <- fr.x - end(x)[2L]
                  dn1 <- start(x)[1L]:end(x)[1L]
                  x <- matrix(c(rep.int("", start.pad), format(x, ...), rep.int("", end.pad)), ncol=fr.x,
                    byrow=TRUE, dimnames=list(dn1, dn2))
                }
            }
            else
            {
                attributes(x) <- NULL
                names(x) <- tx
            }
        }
        else
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    else
    {
        if (calendar && fr.x > 1)
        {
            tm <- time(x)
            t2 <- 1 + round(fr.x * ((tm + 0.001)%%1))
            p1 <- format(floor(zapsmall(tm)))
            rownames(x) <- if (fr.x == 12)
                paste(month.abb[t2], p1, sep=" ")
            else paste(p1, if (fr.x == 4)
                c("Q1", "Q2", "Q3", "Q4")[t2]
            else format(t2), sep=" ")
        }
        else
            rownames(x) <- format(time(x), nsmall=nd, digits=nd)
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    return(as.data.frame(x))
}

print.forecast <- function(x ,...)
{
    print(as.data.frame(x))
}


summary.forecast <- function(object,...)
{
    cat(paste("\nForecast method:",object$method))
#    cat(paste("\n\nCall:\n",deparse(object$call)))
    cat(paste("\n\nModel Information:\n"))
    print(object$model)
    cat("\nError measures:\n")
    print(accuracy(object))
    if(is.null(object$mean))
        cat("\n No forecasts\n")
    else
    {
        cat("\nForecasts:\n")
        print(object)
    }
}

plotlmforecast <- function(object, PI, shaded, shadecols, col, fcol, pi.col, pi.lty,
  xlim=NULL, ylim, main, ylab, xlab, ...)
{
  xvar <- attributes(terms(object$model))$term.labels
  if(length(xvar) > 1)
    stop("Forecast plot for regression models only available for a single predictor")
  else if(ncol(object$newdata)==1) # Make sure column has correct name
    colnames(object$newdata) <- xvar
  if(is.null(xlim))
    xlim <- range(object$newdata[,xvar],model.frame(object$model)[,xvar])
  if(is.null(ylim))
    ylim <- range(object$upper,object$lower,fitted(object$model)+residuals(object$model))
  plot(formula(object$model),data=model.frame(object$model),
    xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,col=col,...)
  abline(object$model)
  nf <- length(object$mean)
  if(PI)
  {
    nint <- length(object$level)
    idx <- rev(order(object$level))
    if(is.null(shadecols))
    {
      #require(colorspace)
      if(min(object$level) < 50) # Using very small confidence levels.
        shadecols <- rev(colorspace::sequential_hcl(100)[object$level])
      else # This should happen almost all the time. Colors mapped to levels.
        shadecols <- rev(colorspace::sequential_hcl(52)[object$level-49])
    }
    if(length(shadecols)==1)
    {
      if(shadecols=="oldstyle") # Default behaviour up to v3.25.
        shadecols <- heat.colors(nint+2)[switch(1+(nint>1),2,nint:1)+1]
    }

    for(i in 1:nf)
    {
      for(j in 1:nint)
      {
        if(shaded)
          lines(rep(object$newdata[i,xvar],2), c(object$lower[i,idx[j]],object$upper[i,idx[j]]), col=shadecols[j],lwd=6)
        else
          lines(rep(object$newdata[i,xvar],2), c(object$lower[i,idx[j]],object$upper[i,idx[j]]), col=pi.col, lty=pi.lty)
      }
    }
  }
  points(object$newdata[,xvar],object$mean,pch=19,col=fcol)
}

plot.forecast <- function(x, include, PI=TRUE, showgap = TRUE, shaded=TRUE, shadebars=(length(x$mean)<5),
        shadecols=NULL, col=1, fcol=4, pi.col=1, pi.lty=2, ylim=NULL, main=NULL, xlab="",
        ylab="", type="l",  flty = 1, flwd = 2, ...)
{
  if(is.element("x",names(x))) # Assume stored as x
    xx <- x$x
  else
    xx=NULL
  if(is.null(x$lower) | is.null(x$upper) | is.null(x$level)){
    PI <- FALSE
  }
  else if(!is.finite(max(x$upper))){
    PI <- FALSE
  }

  if(!shaded)
    shadebars <- FALSE
  if(is.null(main))
    main <- paste("Forecasts from ",x$method,sep="")
  if(PI)
  {
    x$upper <- as.matrix(x$upper)
    x$lower <- as.matrix(x$lower)
  }

  if(is.element("lm",class(x$model)) & !is.element("ts",class(x$mean))) # Non time series linear model
  {
    plotlmforecast(x, PI=PI, shaded=shaded, shadecols=shadecols, col=col, fcol=fcol, pi.col=pi.col, pi.lty=pi.lty,
      ylim=ylim, main=main, xlab=xlab, ylab=ylab, ...)
    if(PI)
      return(invisible(list(mean=x$mean,lower=as.matrix(x$lower),upper=as.matrix(x$upper))))
    else
      return(invisible(list(mean=x$mean)))
  }

  # Otherwise assume x is from a time series forecast
  n <- length(xx)
  if(n==0)
    include <- 0
  else if(missing(include))
    include <- length(xx)

  # Check if all historical values are missing
  if(n > 0)
  {
    if(sum(is.na(xx))==length(xx))
      n <- 0
  }
  if(n > 0)
  {
    xx <- as.ts(xx)
    freq <- frequency(xx)
    strt <- start(xx)
    nx <- max(which(!is.na(xx)))
    xxx <- xx[1:nx]
    include <- min(include,nx)

    if(!showgap){
      lastObs <- x$x[length(x$x)]
      lastTime <- time(x$x)[length(x$x)]
      x$mean <- ts(c(lastObs, x$mean), start = lastTime, frequency = freq)
      x$upper <- ts(rbind(lastObs, x$upper), start = lastTime, frequency = freq)
      x$lower <- ts(rbind(lastObs, x$lower), start = lastTime, frequency = freq)
    }
  }
  else
  {
    freq <- frequency(x$mean)
    strt <- start(x$mean)
    nx <- include <- 1
    xx <- xxx <- ts(NA,frequency=freq,end=tsp(x$mean)[1]-1/freq)

    if(!showgap){
      warning("Removing the gap requires historical data, provide this via model$x. Defaulting showgap to TRUE.")
    }
  }

  pred.mean <- x$mean

  if(is.null(ylim))
  {
    ylim <- range(c(xx[(n-include+1):n],pred.mean),na.rm=TRUE)
    if(PI)
      ylim <- range(ylim,x$lower,x$upper,na.rm=TRUE)
  }
  npred <- length(pred.mean)
  tsx <- is.ts(pred.mean)
  if(!tsx)
  {
    pred.mean <- ts(pred.mean,start=nx+1,frequency=1)
    type <- "p"
  }
  plot(ts(c(xxx[(nx-include+1):nx], rep(NA, npred)), end=tsp(xx)[2] + (nx-n)/freq + npred/freq, frequency=freq),
    xlab=xlab,ylim=ylim,ylab=ylab,main=main,col=col,type=type, ...)
  if(PI)
  {
    if(is.ts(x$upper)){
      xxx <- time(x$upper)
    }
    else{
      xxx <- tsp(pred.mean)[1] - 1/freq + (1:npred)/freq
    }
    idx <- rev(order(x$level))
    nint <- length(x$level)
    if(is.null(shadecols))
    {
      #require(colorspace)
      if(min(x$level) < 50) # Using very small confidence levels.
        shadecols <- rev(colorspace::sequential_hcl(100)[x$level])
      else # This should happen almost all the time. Colors mapped to levels.
        shadecols <- rev(colorspace::sequential_hcl(52)[x$level-49])
    }
    if(length(shadecols)==1)
    {
      if(shadecols=="oldstyle") # Default behaviour up to v3.25.
        shadecols <- heat.colors(nint+2)[switch(1+(nint>1),2,nint:1)+1]
    }
    for(i in 1:nint)
    {
      if(shadebars)
      {
        for(j in 1:npred)
        {
          polygon(xxx[j] + c(-0.5,0.5,0.5,-0.5)/freq, c(rep(x$lower[j,idx[i]],2),rep(x$upper[j,idx[i]],2)),
            col=shadecols[i], border=FALSE)
        }
      }
      else if(shaded)
      {
        polygon(c(xxx,rev(xxx)), c(x$lower[,idx[i]],rev(x$upper[,idx[i]])),
          col=shadecols[i], border=FALSE)
      }
      else if(npred == 1)
      {
        lines(c(xxx)+c(-0.5,0.5)/freq, rep(x$lower[,idx[i]],2),col=pi.col,lty=pi.lty)
        lines(c(xxx)+c(-0.5,0.5)/freq, rep(x$upper[,idx[i]],2),col=pi.col,lty=pi.lty)
      }
      else
      {
        lines(x$lower[,idx[i]],col=pi.col,lty=pi.lty)
        lines(x$upper[,idx[i]],col=pi.col,lty=pi.lty)
      }
    }
  }
  if(npred > 1 & !shadebars & tsx)
    lines(pred.mean, lty = flty, lwd=flwd, col = fcol)
  else
    points(pred.mean, col=fcol, pch=19)
  if(PI)
    invisible(list(mean=pred.mean,lower=x$lower,upper=x$upper))
  else
    invisible(list(mean=pred.mean))
}

predict.default <- function(object, ...)
{
    forecast(object, ...)
}

hfitted <- function(object, h=1, FUN=NULL, ...)
{
  if(h==1){
    return(fitted(object))
  }
  #Attempt to get model function
  if(is.null(FUN)){
    FUN <- class(object)
    for(i in FUN){
      if(exists(i)){
        if(typeof(eval(parse(text = i)[[1]]))=="closure"){
          FUN <- i
          i <- "Y"
          break
        }
      }
    }
    if(i!="Y"){
      stop("Could not find appropriate function to refit, specify FUN=function")
    }
  }
  x <- getResponse(object)
  tspx <- tsp(x)
  fits <- fitted(object)*NA
  n <- length(fits)
  refitarg <- list(x=NULL, model = object)
  names(refitarg)[1] <- names(formals(FUN))[1]
  fcarg <- list(h=h)
  for(i in 1:(n-h))
  {
    refitarg[[1]] <- ts(x[1:i], start=tspx[1], frequency=tspx[3])
    if(!is.null(object$xreg)){
      refitarg$xreg <- ts(object$xreg[1:i,], start=tspx[1], frequency=tspx[3])
      fcarg$xreg <- ts(object$xreg[(i+1):(i+h),], start=tspx[1]+i/tspx[3], frequency=tspx[3])
    }
    fcarg$object <- try(do.call(FUN, refitarg), silent=TRUE)
    if(!is.element("try-error", class(fcarg$object)))
      fits[i+h] <- suppressWarnings(do.call("forecast", fcarg)$mean[h])
  }
  return(fits)
}

# The following function is for when users don't realise they already have the forecasts.
# e.g., with the dshw(), meanf() or rwf() functions.

forecast.forecast <- function(object, ...)
{
  input_names <- as.list(substitute(list(...)))
  # Read level argument
  if(is.element("level",names(input_names)))
  {
    level <- list(...)[["level"]]
    if(!identical(level,object$level))
      stop("Please set the level argument when the forecasts are first computed")
  }
  # Read h argument
  if(is.element("h",names(input_names)))
  {
    h <- list(...)[["h"]]
    if(h > length(object$mean))
      stop("Please select a longer horizon when the forecasts are first computed")
    tspf <- tsp(object$mean)
    object$mean <- ts(object$mean[1:h], start=tspf[1], frequency=tspf[3])
    if(!is.null(object$upper))
    {
      object$upper <- ts(object$upper[1:h,,drop=FALSE], start=tspf[1], frequency=tspf[3])
      object$lower <- ts(object$lower[1:h,,drop=FALSE], start=tspf[1], frequency=tspf[3])
    }
  }
  return(object)
}

subset.forecast <- function(x, ...){
  tspx <- tsp(x$mean)
  x$mean <- subset(x$mean, ...)
  x$lower <- subset(ts(x$lower, start=tspx[1], frequency=tspx[3]), ...)
  x$upper <- subset(ts(x$upper, start=tspx[1], frequency=tspx[3]), ...)
  return(x)
}

is.forecast <- function(x){
  inherits(x, "forecast")
}

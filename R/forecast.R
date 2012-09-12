## Generic forecast functions
## Part of forecast and demography packages

forecast <- function(object,...) UseMethod("forecast")

forecast.default <- function(object,...) forecast.ts(object,...)

forecast.ts <- function(object, h=ifelse(frequency(object)>1, 2*frequency(object), 10), level=c(80,95), fan=FALSE, ...)
{
  n <- length(object)
  if(n > 3)
  {
    if(frequency(object) < 13)
      forecast(ets(object,...),h=h,level=level,fan=fan)
    else
      stlf(object,h=h,level=level,fan=fan,...)
  }
  else
    meanf(object,h=h,level=level,fan=fan,...)
}

as.data.frame.forecast <- function(x,...)
{
    nconf <- length(x$level)
    out <- matrix(x$mean, ncol=1)
    ists <- is.ts(x$mean)
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
            out <- cbind(out, x$lower[, i], x$upper[, i])
            names <- c(names, paste("Lo", x$level[i]), paste("Hi", x$level[i]))
        }
    }
    colnames(out) <- names
    rownames(out) <- time(x$mean)
    # Rest of function borrowed from print.ts(), but with header() omitted
    if(!ists)
        return(as.data.frame(out))

    x <- as.ts(out)
    fr.x <- frequency(x)
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
                tx <- time(x)
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
            rownames(x) <- format(time(x))
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
    cat("\nIn-sample error measures:\n")
    print(accuracy(object))
    if(is.null(object$mean))
        cat("\n No forecasts\n")
    else
    {
        cat("\nForecasts:\n")
        print(object)
    }
}

plotlmforecast <- function(object, plot.conf, shaded, shadecols, col, fcol, pi.col, pi.lty,
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
  plot(formula(object$model),data=model.frame(object$model),xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,col=col,...)
  abline(object$model)
  nf <- length(object$mean)
  if(plot.conf)
  {
    nint <- length(object$level)
    idx <- rev(order(object$level))
    if(is.null(shadecols))
    {
      require(colorspace)
      if(min(object$level) < 50) # Using very small confidence levels.
        shadecols <- rev(sequential_hcl(100)[object$level])
      else # This should happen almost all the time. Colors mapped to levels.
        shadecols <- rev(sequential_hcl(52)[object$level-49])
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

plot.forecast <- function(x, include, plot.conf=TRUE, shaded=TRUE, shadebars=(length(x$mean)<5),
        shadecols=NULL, col=1, fcol=4, pi.col=1, pi.lty=2, ylim=NULL, main=NULL, ylab="",
        xlab="", type="l",  flty = 1, flwd = 2, ...)
{
  if(is.element("x",names(x))) # Assume stored as x
    data <- x$x
  else
    data=NULL
  if(length(data)==0)
    include <- 0
  else if(missing(include))
    include <- length(data)
  if(is.null(x$lower) | is.null(x$upper) | is.null(x$level))
    plot.conf=FALSE
  if(!shaded)
    shadebars <- FALSE
  if(is.null(main))
    main <- paste("Forecasts from ",x$method,sep="")
  if(plot.conf)
  {
    x$upper <- as.matrix(x$upper)
    x$lower <- as.matrix(x$lower)
  }

  if(is.element("lm",class(x$model)) & !is.element("ts",class(x$mean))) # Non time series linear model
  {
    plotlmforecast(x, plot.conf=plot.conf, shaded=shaded, shadecols=shadecols, col=col, fcol=fcol, pi.col=pi.col, pi.lty=pi.lty,
      ylim=ylim, main=main, xlab=xlab, ylab=ylab, ...)
    if(plot.conf)
      return(invisible(list(mean=x$mean,lower=as.matrix(x$lower),upper=as.matrix(x$upper))))
    else
      return(invisible(list(mean=x$mean)))
  }

  # Otherwise assume x is from a time series forecast
  if(length(data) > 0)
    data <- as.ts(data)
  freq <- frequency(data)
  strt <- start(data)
  n <- length(data)
  pred.mean <- x$mean
  xx <- data

  # Remove final missing values
  nx <- max(which(!is.na(xx)))
  xxx <- xx[1:nx]
  include <- min(include,nx)
  if(is.null(ylim))
  {
    ylim <- range(c(xx[(n-include+1):n],pred.mean),na.rm=TRUE)
    if(plot.conf)
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
  if(plot.conf)
  {
    xxx <- tsp(pred.mean)[1] - 1/freq + (1:npred)/freq
    idx <- rev(order(x$level))
    nint <- length(x$level)
    if(is.null(shadecols))
    {
      require(colorspace)
      if(min(x$level) < 50) # Using very small confidence levels.
        shadecols <- rev(sequential_hcl(100)[x$level])
      else # This should happen almost all the time. Colors mapped to levels.
        shadecols <- rev(sequential_hcl(52)[x$level-49])
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
        lines(xxx+c(-0.5,0.5)/freq,rep(x$lower[,idx[i]],2),col=pi.col,lty=pi.lty)
        lines(xxx+c(-0.5,0.5)/freq,rep(x$upper[,idx[i]],2),col=pi.col,lty=pi.lty)
      }
      else
      {
        lines(xxx,x$lower[,idx[i]],col=pi.col,lty=pi.lty)
        lines(xxx,x$upper[,idx[i]],col=pi.col,lty=pi.lty)
      }
    }
  }
  if(npred > 1 & !shadebars & tsx)
    lines(pred.mean, lty = flty, lwd=flwd, col = fcol)
  else
    points(pred.mean, col=fcol, pch=19)
  if(plot.conf)
    invisible(list(mean=pred.mean,lower=x$lower,upper=x$upper))
  else
    invisible(list(mean=pred.mean))
}

predict.default <- function(object, ...)
{
    forecast(object, ...)
}

tslm <- function(formula,data,lambda=NULL,...)
{
  if(missing(data)) # Grab first variable
  {
    dataname <- as.character(formula)[2]
    x <- get(dataname, envir=parent.frame())
    data <- data.frame(x)
    colnames(data) <- dataname
  }
  else
  {
    dataname <- substitute(data)
    x <- data[,1]
  }
  if(!is.ts(x))
    stop("Not time series data")
  tspx <- tsp(x)

  if(tspx[3]==1) # Nonseasonal data
  {
    f <- as.character(formula)
    if("season" %in% attr(terms(formula), "term.labels"))
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
  }
  if(sum(is.na(x))>0)
    warning("This function may not work correctly when the data contains missing values")
  orig.x <- x
  if(!is.null(lambda))
    x <- data[,1] <- BoxCox(data[,1],lambda)

  # Add trend and seasonal to data frame
  trend <- 1:length(x)
  season <- as.factor(cycle(x))
  data <- data.frame(data,trend,season)
  rownames(data) <- trend
  fit <- lm(formula,data=data,na.action=na.exclude,...)
  j <- is.element(data$trend,names(fit$res))
  if(!is.null(fit$call$subset))
    j <- j & eval(fit$call$subset)
  data <- data[j,]
  # Try to figure out times for subset. Assume they are contiguous.
  timesx <- time(x)[j]
  tspx <- c(min(timesx),max(timesx),tspx[3])
  fit$data <- ts(data)
  fit$x <- ts(orig.x)
  fit$residuals <- ts(residuals(fit))
  fit$fitted.values <- ts(fitted(fit))
  tsp(fit$data) <- tsp(fit$residuals) <- tsp(fit$fitted.values) <- tsp(fit$x) <- tspx
  if(!is.null(dataname))
    fit$call$data <- dataname
  fit$lambda <- lambda
  if(!is.null(lambda))
    fit$fitted.values <- InvBoxCox(fit$fitted.values,lambda)
  return(fit)
}

forecast.lm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...)
{
  if (fan)
    level <- seq(51, 99, by = 3)
  else
  {
    if (min(level) > 0 & max(level) < 1)
      level <- 100 * level
    else if (min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  if(!is.null(object$data))
    origdata <- object$data
  else if(!is.null(object$call$data))
    origdata <- object$data <- eval(object$call$data)
  else
    origdata <- as.data.frame(fitted(object) + residuals(object))

  # Check if the forecasts will be time series
  if(ts & is.element("ts",class(origdata)))
  {
    tspx <- tsp(origdata)
    timesx <- time(origdata)
  }
  else
    tspx <- NULL
  if(!is.null(object$call$subset))
  {
    j <- eval(object$call$subset)
    origdata <- origdata[j,]
    if(!is.null(tspx))
    {
      # Try to figure out times for subset. Assume they are contiguous.
      timesx <- timesx[j]
      tspx <- tsp(origdata) <- c(min(timesx),max(timesx),tspx[3])
    }
  }
  # Add trend and seasonal to data frame
  if(!missing(newdata))
  {
    newdata <- as.data.frame(newdata)
    h <- nrow(newdata)
  }
  if(!is.null(tspx) & is.element("trend",colnames(origdata)))
  {
    x <- ts(1:h, start=tspx[2]+1/tspx[3], frequency=tspx[3])
    trend <- max(origdata[,"trend"]) + (1:h)
    season <- as.factor(cycle(x))
    if(!missing(newdata))
      newdata <- data.frame(as.data.frame(newdata),trend,season)
    else
      newdata <- data.frame(trend,season)
  }
  newdata <- as.data.frame(newdata)
  # If only one column, assume its name.
  if(ncol(newdata)==1 & colnames(newdata)[1]=="newdata")
    colnames(newdata) <- as.character(formula(object$model))[3]

  # Check regressors included in newdata.
  # Not working so removed for now.
  #xreg <- attributes(terms(object$model))$term.labels
  #if(any(!is.element(xreg,colnames(newdata))))
  #  stop("Predictor variables not included")

  object$x <- getResponse(object)
  #responsevar <- as.character(formula(object$model))[2]
  #responsevar <- gsub("`","",responsevar)
  #object$x <- model.frame(object$model)[,responsevar]

  out <- list()
  nl <- length(level)
  for(i in 1:nl)
    out[[i]] <- predict(object, newdata=newdata, se.fit=TRUE, interval="prediction", level=level[i]/100, ...)

  if(nrow(newdata) != length(out[[1]]$fit[,1]))
    stop("Variables not found in newdata")

  fcast <- list(model=object,mean=out[[1]]$fit[,1],lower=out[[1]]$fit[,2],upper=out[[1]]$fit[,3],
    level=level,x=object$x)
  fcast$method <- "Linear regression model"
  fcast$newdata <- newdata
  fcast$residuals <- residuals(object)
  fcast$fitted <- fitted(object)
  if(nrow(origdata) != length(fcast$x)) # Give up on ts attributes as some data are missing
    tspx <- NULL
  if(length(fcast$x) != length(fcast$residuals))
    tspx <- NULL
  if(!is.null(tspx))
  {
    fcast$x <- ts(fcast$x)
    fcast$residuals <- ts(fcast$residuals)
    fcast$fitted <- ts(fcast$fitted)
    tsp(fcast$x) <- tsp(fcast$residuals) <- tsp(fcast$fitted) <- tspx
  }
  if(nl > 1)
  {
    for(i in 2:nl)
    {
      fcast$lower <- cbind(fcast$lower,out[[i]]$fit[,2])
      fcast$upper <- cbind(fcast$upper,out[[i]]$fit[,3])
    }
  }
  if(!is.null(tspx))
  {
    fcast$mean <- ts(fcast$mean, start=tspx[2]+1/tspx[3],frequency=tspx[3])
    fcast$upper <- ts(fcast$upper, start=tspx[2]+1/tspx[3],frequency=tspx[3])
    fcast$lower <- ts(fcast$lower, start=tspx[2]+1/tspx[3],frequency=tspx[3])
  }

  if(!is.null(lambda))
  {
    fcast$x <- InvBoxCox(fcast$x,lambda)
    fcast$mean <- InvBoxCox(fcast$mean,lambda)
    fcast$lower <- InvBoxCox(fcast$lower,lambda)
    fcast$upper <- InvBoxCox(fcast$upper,lambda)
  }

  return(structure(fcast,class="forecast"))
}
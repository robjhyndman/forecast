is.mforecast <- function(x){
  inherits(x, "mforecast")
}

mlmsplit <- function(x, index=NULL){
  if(is.null(index)){
    stop("Must select lm using index=integer(1)")
  }
  mfit <- match(c("coefficients", "residuals", "effects", "fitted.values"), names(x), 0L)
  for (j in mfit){
    x[[j]] <- x[[j]][,index]
  }
  class(x) <- "lm"
  y<-attr(x$terms,"response")
  
  cn <- colnames(x$model)
  x$model[[y]] <- x$model[[y]][,index]
  
  if(!is.null(tsp(x$data[,1]))){
    tspx <- tsp(x$data[,1]) #Consolidate ts attributes for forecast.lm
    x$data <- lapply(x$model, function(x) ts(x, start = tspx[1], end = tspx[2], frequency = tspx[3]))
    class(x$data) <- "data.frame"
    row.names(x$data) <- 1:max(sapply(x$data, NROW))
  }
  
  attr(x$terms,"dataClasses")[y] <- class(x$model[[y]])
  x$terms <- terms(x$model)
  return(x)
}

forecast.mlm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=FALSE, ts=TRUE, ...)
{
  K <- NCOL(object$coefficients)
  y<-attr(object$terms,"response")
  
  # Check if the forecasts will be time series
  if(ts & is.element("ts",class(object$x))){
    tspx <- tsp(object$x)
    timesx <- time(object$x)
  }
  else{
    tspx <- NULL
  }
  out <- list(model=object,level=level)
  if(!is.null(object$x) & !is.list(object$x)){
    out$x <- object$x
  }
  else if(!is.null(object$model)){
    out$x <- object$model[,y]
  }
  else {
    stop("Response not found")
  }
  out$residuals <- residuals(object)
  out$fitted <- fitted(object)
  out$mean <- out$lower <- out$upper <- vector("list",K)
  names(out$mean) <- names(out$lower) <- names(out$upper) <- colnames(object$coefficients)
  out$method <- "Multiple linear regression model"
  for (i in 1:K){
    if(missing(newdata)){
      fcst <- forecast(object = mlmsplit(object,index=i),
                       h=h, level = level, fan = fan, lambda=lambda,
                       biasadj=biasadj, ts = !is.null(tspx), ...)
      newdata <- fcst$newdata
    }
    else{
      fcst <- forecast(object = mlmsplit(object,index=i), newdata=newdata,
                       h=h, level = level, fan = fan, lambda=lambda,
                       biasadj=biasadj, ts = !is.null(tspx), ...)
    }
    out$mean[[i]] <- fcst$mean
    out$lower[[i]] <- fcst$lower
    out$upper[[i]] <- fcst$upper
  }
  out$newdata <- newdata
  return(structure(out,class="mforecast"))
}

forecast.mts <- function(object, h=ifelse(frequency(object)>1, 2*frequency(object), 10), 
                         level=c(80,95), fan=FALSE, robust=FALSE, lambda = NULL, find.frequency = FALSE, 
                         allow.multiplicative.trend=FALSE, ...){
  out <- list(level=level, x=object)
  for(i in 1:NCOL(object)){
    fcast <- forecast.ts(object[,i], h=h, level=level, fan=fan, robust=robust, lambda=lambda, find.frequency=find.frequency,
                allow.multiplicative.trend = allow.multiplicative.trend, ...)
    out$model[[i]] <- fcast$model
    out$mean[[i]] <- fcast$mean
    out$lower[[i]] <- fcast$lower
    out$upper[[i]] <- fcast$upper
    out$method[[i]] <- fcast$method
    if(i==1){
      out$residuals <- residuals(fcast)
      out$fitted <- fitted(fcast)
    }
    else{
      out$residuals <- cbind(out$residuals, residuals(fcast))
      out$fitted <- cbind(out$fitted, fitted(fcast))
    }
  }
  
  names(out$model) <- names(out$mean) <- names(out$lower) <- names(out$upper) <- names(out$method) <- colnames(out$fitted) <- colnames(out$residuals) <- colnames(object)
  return(structure(out,class="mforecast"))
}

print.mforecast <- function(x, ...)
{
  for(i in 1:length(x$mean))
  {
    cat(names(x$mean)[i],"\n")
    fcst <- x
    fcst$mean <- x$mean[[i]]
    fcst$lower <- x$lower[[i]]
    fcst$upper <- x$upper[[i]]
    class(fcst) <- "forecast"
    print(fcst)
    if(i < length(x$mean))
      cat("\n")
  }
}

plot.mforecast <- function(x, main=paste("Forecasts from",x$method),xlab="time",...)
{
  K <- length(x$mean)
  oldpar <- par(mfrow=c(K,1),mar=c(0,5.1,0,2.1),oma=c(6,0,5,0))
  on.exit(par(oldpar))
  for(i in 1:K)
  {
    fcst <- x
    fcst$mean <- x$mean[[i]]
    fcst$lower <- x$lower[[i]]
    fcst$upper <- x$upper[[i]]
    fcst$x <- x$x[,i]
    if("mlm" %in% class(fcst$model)){
      fcst$model <- mlmsplit(fcst$model, index=i)
    }
    class(fcst) <- "forecast"
    plot(fcst,main="",xaxt="n",ylab=names(x$mean)[i],...)
  }
  axis(1)
  mtext(xlab,outer=TRUE,side=1,line=3)
  title(main=main,outer=TRUE)
}

summary.mforecast <- function(object, ...){
  cat(paste("\nForecast method:",object$method))
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
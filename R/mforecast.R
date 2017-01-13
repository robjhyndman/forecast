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

mforecastsplit <- function(x, index=1:length(x$mean)){
  out <- list()
  for(i in index){
    out[[i]] <- structure(list(level = x$level,
                     x = x$x[,i], 
                     model = x$model[[i]],
                     mean = x$mean[[i]],
                     lower = x$lower[[i]],
                     upper = x$upper[[i]],
                     method = x$method[i],
                     residuals = x$residuals[,i],
                     fitted = x$fitted[,i],
                     series = names(x$mean)[i]),
                     class = "forecast")
  }
  if(length(index)==1){
    out <- out[[1]]
  }
  return(out)
}

forecast.mlm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=NULL, ts=TRUE, ...)
{
  out <- list(model=object,forecast=vector("list", NCOL(object$coefficients)))
  
  cl <- match.call()
  cl[[1]] <- quote(forecast.lm)
  cl$object <- quote(mlmsplit(object,index=i))
  for(i in seq_along(out$forecast)){
    out$forecast[[i]] <- eval(cl)
    out$forecast[[i]]$series <- colnames(object$coefficients)[i]
  }
  out$method <- rep("Multiple linear regression model", length(out$forecast))
  names(out$forecast) <- names(out$method) <- colnames(object$coefficients)
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
  lapply(x$forecast, function(x){
    cat(paste0(x$series, "\n"))
    print(x)
    cat("\n")
  })
  return(invisible())
}

plot.mforecast <- function(x, main=paste("Forecasts from",x$method),xlab="time",...)
{
  oldpar <- par(mfrow=c(length(x$forecast),1),mar=c(0,5.1,0,2.1),oma=c(6,0,5,0))
  on.exit(par(oldpar))
  for(fcast in x$forecast)
  {
    plot(fcast,main="",xaxt="n",ylab=fcast$series,...)
  }
  axis(1)
  mtext(xlab,outer=TRUE,side=1,line=3)
  title(main=main,outer=TRUE)
}

summary.mforecast <- function(object, ...){
  cat(paste("\nForecast method:",unique(object$method)))
  cat(paste("\n\nModel Information:\n"))
  print(object$model)
  cat("\nError measures:\n")
  print(accuracy(object))
  if(is.null(object$forecast))
    cat("\n No forecasts\n")
  else
  {
    cat("\nForecasts:\n")
    print(object)
  }
}
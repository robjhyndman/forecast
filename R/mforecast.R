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
  x$model <- as.data.frame(cbind(x$model[,y][,index],x$model[,-y]))
  colnames(x$model) <- cn
  
  if(!is.null(tsp(x$data[,1]))){
    tspx <- tsp(x$data[,1]) #Consolidate ts attributes for forecast.lm
    x$data <- ts(x$model, start = tspx[1], end = tspx[2], frequency = tspx[3])
  }
  
  x$model <- model.frame(formula(x$terms),data=x$model)
  x$terms <- terms(x$model)
  return(x)
}

forecast.mlm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...)
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
  #Add trend and seasonality to data frame
  if(!missing(newdata))
  {
    newdata <- as.data.frame(newdata)
    h <- nrow(newdata)
  }
  if(!is.null(tspx) & is.element("trend",colnames(object$model)))
  {
    x <- ts(1:h, start=tspx[2]+1/tspx[3], frequency=tspx[3])
    trend <- max(object$model[,"trend"]) + (1:h)
    season <- as.factor(cycle(x))
    if(!missing(newdata))
      newdata <- data.frame(as.data.frame(newdata),trend,season)
    else
      newdata <- data.frame(trend,season)
  }
  newdata <- as.data.frame(newdata)
  
  # If only one column, assume its name.
  if(NCOL(newdata)==1 & colnames(newdata)[1]=="newdata")
    colnames(newdata) <- as.character(formula(object$model))[3]
  
  out <- list(model=object,level=level,newdata=newdata)
  if(!is.null(object$x) & !is.list(object$x)){
    out$x <- object$x
  }
  else if(!is.null(object$model)){
    out$x <- object$model[,y]
  }
  else {
    stop("Response not found")
  }
  out$res <- residuals(object)
  out$fitted <- fitted(object)
  out$mean <- out$lower <- out$upper <- vector("list",K)
  names(out$mean) <- names(out$lower) <- names(out$upper) <- colnames(object$coefficients)
  out$method <- "Multiple linear regression model"
  for (i in 1:K){
    fcst <- forecast(object = mlmsplit(object,index=i), newdata=newdata, h=h, level = level, fan = fan, ts = TRUE)
    out$mean[[i]] <- fcst$mean
    out$lower[[i]] <- fcst$lower
    out$upper[[i]] <- fcst$upper
     #forecast(object = model, newdata = newdata, level = level, fan = fan, lambda = lambda, ts = ts, ...)
  }
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
    fcst$model <- mlmsplit(fcst$model, index=i)
    class(fcst) <- "forecast"
    plot(fcst,main="",xaxt="n",ylab=names(x$mean)[i],...)
  }
  axis(1)
  mtext(xlab,outer=TRUE,side=1,line=3)
  title(main=main,outer=TRUE)
}
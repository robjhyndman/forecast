tslm <- function(formula, data, subset, lambda=NULL, biasadj=FALSE, ...){
  cl <- match.call()
  if(!("formula" %in% class(formula))){
    formula <- stats::as.formula(formula)
  }
  mt <- terms(formula)

  vars <- attr(mt,"variables")
  #Check for time series variables
  tsvar <- match(c("trend", "season"), as.character(vars), 0L)
  #Check for functions (which should be evaluated later, in lm)
  fnvar <- NULL
  for(i in 2:length(vars)){
    term <- vars[[i]]
    if(!is.symbol(term)){
      if(typeof(eval(term[[1]]))=="closure"){#If this term is a function (alike fourier)
        #Renaming variable code
        # This is to be done by datamat(functions=TRUE)
#         floc <- match(deparse(term),colnames(data))
#         colnames(data)[floc] <- paste("\U0192",term[[1]],"(\U2026)",sep="")
        #formula[[3]][[i+1]]
        attr(mt,"variables")[[i]] <- as.symbol(paste("FN.",term[[1]],"_",sep=""))
        attr(mt,"term.labels") <- gsub(deparse(term), paste("FN.",term[[1]],"_",sep=""), attr(mt,"term.labels"), fixed = TRUE)
        #fnvar <- c(fnvar, i)
      }
    }
  }
  formula <- stats::reformulate(attr(mt,"term.labels"), response = vars[[attr(mt,"response")+1]],
                         intercept = attr(mt,"intercept"))
  if(sum(c(tsvar, fnvar))>0){
    #Remove variables not needed in data (trend+season+functions)
    vars <- vars[-c(tsvar, fnvar)]
  }

  if(!missing(data)){
    #Check for any missing variables in data
    vars <- vars[c(TRUE, !as.character(vars[-1])%in%colnames(data))]
    dataname <- substitute(data)
  }

  if(length(vars)>1){
    # Grab variables missing from data
    vars[[1]] <- quote(forecast:::datamat)
    if(!missing(data)){
      vars[[length(vars)+1]] <- dataname
    }
    data <- eval.parent(vars)
  }
  else{
    data <- datamat(data)
  }

  # Check to see if data is univariate time series
  if(is.null(dim(data)) & length(data)!=0){
    #cn <- as.character(vars)[2:length(vars)]
    cn <- as.character(vars)[2]
  } else{
    cn <- colnames(data)
  }
  if(is.null(tsp(data))){
    if(is.null(tsp(data[,1]))){#Check for complex ts data.frame
      if((attr(mt,"intercept")+1)%in%fnvar){#Check unevaluated response variable
        tspx <- tsp(eval(attr(mt,"variables")[[attr(mt,"intercept")+1]]))
      }
    }
    else{
      tspx <- tsp(data[,1])
    }
  }
  else{
    tspx <- tsp(data)
  }
  if(!exists("tspx")){
    stop("Not time series data, use lm()")
  }
  tsdat <- match(c("trend", "season"), cn, 0L)
  #Create trend and season if missing
  if(tsdat[1]==0&tsvar[1]!=0){#If "trend" is not in data, but is in formula
    trend <- 1:NROW(data)
    cn <- c(cn,"trend")
    data <- cbind(data,trend)
  }
  if(tsdat[2]==0&tsvar[2]!=0){#If "season" is not in data, but is in formula
    if(tspx[3]==1){ # Nonseasonal data
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    season <- as.factor(cycle(data[,1]))
    cn <- c(cn,"season")
    data <- cbind(data,season)
  }
  colnames(data) <- cn
  if(!missing(subset)){
    if(is.logical(subset)){
      subsetTF <- subset
    }
    else{
      subsetTF <- eval.parent(substitute(subset)[[2]])%in%subset
    }
    if(NCOL(subset)!=1){
      subsetTF <- rowSums(matrix(data=subsetTF, ncol=2))==NCOL(subset) #TODO
    }
    if(NROW(subsetTF)!=NROW(data)){
      stop("Subset must specify the rows to keep in the dataset")
    }
    if(!is.null(tsp(subset)) & NROW(subset)!=NROW(data)){
      tspx <- tsp(subset)
    }
    else{
      warning("Subset has been assumed contiguous")
      timesx <- time(data[,1])[subsetTF]
      tspx <- recoverTSP(timesx)
    }
    if(tspx[3]==1 & tsdat[2]==0 & tsvar[2]!=0){
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    data <- data[subsetTF,]#model.frame(formula,as.data.frame(data[subsetTF,]))
  }
  if(!is.null(lambda)){
    data[,1] <- BoxCox(data[,1],lambda)
  }
  if(tsdat[2]==0&tsvar[2]!=0){
    data$season <- factor(data$season) #fix for lost factor information, may not be needed?
  }
  fit <- lm(formula,data=data,na.action=na.exclude,...)
  fit$residuals <- ts(residuals(fit))
  fit$fitted.values <- ts(fitted(fit))
  tsp(fit$residuals) <- tsp(fit$fitted.values) <- tsp(data[,1]) <- tspx
  fit$data <- data # This unfortunately needs to be a mf, to be able to separate multivariate response
  fit$x <- data[,1] ## Do we want to include subsetting here?
  fit$call <- cl
  if(NCOL(data[,1])>1){ #Univariate response
    fit$data <- data[,1]
  }
  if(exists("dataname")){
    fit$call$data <- dataname
  }
  if(!is.null(lambda)){
    fit$lambda <- lambda
    fit$fitted.values <- InvBoxCox(fit$fitted.values,lambda)
    if(biasadj){
      fit$fitted.values <- InvBoxCoxf(fit$fitted.values, fvar = var(fit$residuals), lambda = lambda)
    }
  }
  return(fit)
}

forecast.lm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=FALSE, ts=TRUE, ...)
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
  else if(!is.null(object$call$data)){
    origdata <- try(object$data <- eval(object$call$data), silent = TRUE)
    if (is.element("try-error", class(origdata)))
      stop("Could not find data. Try training your model using tslm() or attach data directly to the object via object$data<-modeldata for some object<-lm(formula,modeldata).")
  }
  else
    origdata <- as.data.frame(fitted(object) + residuals(object))
  if(!is.element("data.frame", class(origdata)))
  {
    origdata <- as.data.frame(origdata)
    if(!is.element("data.frame", class(origdata)))
      stop("Could not find data.  Try training your model using tslm() or attach data directly to the object via object$data<-modeldata for some object<-lm(formula,modeldata).")
  }

  # Check if the forecasts will be time series
  if(ts & is.element("ts",class(origdata))){
    tspx <- tsp(origdata)
    timesx <- time(origdata)
  }
  else if(ts & is.element("ts",class(origdata[,1]))){
    tspx <- tsp(origdata[,1])
    timesx <- time(origdata[,1])
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
    reqvars <- as.character(attr(object$terms,"variables")[-1])[-attr(object$terms,"response")]
    newvars <- make.names(colnames(newdata))
    misvar <- match(make.names(reqvars), newvars, 0L)
    if (any(misvar != 0)){
      tmpdata <- datamat(newdata[reqvars[misvar!=0]])
      rm1 <- FALSE
    }
    else{
      #Prefill the datamat
      tmpdata <- datamat(1:NROW(newdata))
      rm1 <- TRUE
    }
    tsvar <- match(c("trend", "season"), reqvars, 0L)
    if(sum(tsvar)>0){
      reqvars <- reqvars[-tsvar]
      misvar <- match(make.names(reqvars), newvars, 0L)
    }
    if (any(misvar == 0)){
      reqvars <- reqvars[misvar == 0]
      for (i in reqvars){
        subvars <- grep(i, names(object$coefficients), value=TRUE)
        subvars <- substr(subvars, nchar(i)+1, 999L)
        fsub <- match(make.names(subvars), newvars, 0L)
        if (any(fsub == 0)){
          #Check for misnamed columns
          fsub <- grep(paste(make.names(subvars),collapse="|"), newvars)
        }
        if (all(fsub != 0)){
          imat <- as.matrix(newdata[,fsub], ncol = length(fsub))
          colnames(imat) <- subvars
          tmpdata[[length(tmpdata)+1]] <- imat
          names(tmpdata)[length(tmpdata)] <- i
        }
        else{
          stop(paste("Could not find \"", i, "\" in newdata", sep=""))
        }
      }
    }
    if(rm1){
      tmpdata[[1]] <- NULL
    }
    newdata <- tmpdata
    h <- nrow(newdata)
  }
  if(!is.null(tspx) & any(is.element(c("trend","season"),colnames(origdata))))
  {
    if(is.element("trend",colnames(origdata))){
      trend <- max(origdata[,"trend"]) + (1:h)
      if(!missing(newdata)){
        newdata <- cbind(newdata, trend)
      }
      else{
        newdata <- datamat(trend)
      }
    }
    if(is.element("season",colnames(origdata))){
      x <- ts(1:h, start=tspx[2]+1/tspx[3], frequency=tspx[3])
      season <- as.factor(cycle(x))
      if(!missing(newdata)){
        newdata <- cbind(newdata, season)
      }
      else{
        newdata <- datamat(season)
      }
    }
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
    if(biasadj){
      fcast$mean <- InvBoxCoxf(fcast, lambda = lambda)
    }
    fcast$lower <- InvBoxCox(fcast$lower,lambda)
    fcast$upper <- InvBoxCox(fcast$upper,lambda)
  }

  return(structure(fcast,class="forecast"))
}

# Compute cross-validation and information criteria from a linear model
CV <- function(obj)
{
  if(!is.element("lm", class(obj)))
    stop("This function is for objects of class lm")
  n <- length(obj$residuals)
  k <- extractAIC(obj)[1]-1 # number of predictors (constant removed)
  aic <- extractAIC(obj)[2]+2 # add 2 for the variance estimate
  aicc <- aic + 2*(k+2)*(k+3)/(n-k-3)
  bic <- aic + (k+2)*(log(n)-2)
  cv <- mean((residuals(obj)/(1-hatvalues(obj)))^2, na.rm=TRUE)
  adjr2 <- summary(obj)$adj
  out <- c(cv,aic,aicc,bic,adjr2)
  names(out) <- c("CV","AIC","AICc","BIC","AdjR2")
  return(out)
}
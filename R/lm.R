tslm <- function(formula, data, subset, lambda=NULL, biasadj=FALSE, ...){
  cl <- match.call()
  if(!("formula" %in% class(formula))){
    formula <- stats::as.formula(formula)
  }
  if(missing(data)){
    mt <- try(terms(formula))
    if(is.element("try-error", class(mt))){
      stop("Cannot extract terms from formula, please provide data argument.")
    }
  }
  else{
    mt <- terms(formula, data=data)
  }

  ## Categorise formula variables into time-series, functions, and data.
  vars <- attr(mt,"variables")
  #Check for time series variables
  tsvar <- match(c("trend", "season"), as.character(vars), 0L)
  #Check for functions (which should be evaluated later, in lm)
  fnvar <- NULL
  for(i in 2:length(vars)){
    term <- vars[[i]]
    if(!is.symbol(term)){
      if(typeof(eval(term[[1]]))=="closure"){#If this term is a function (alike fourier)
        fnvar <- c(fnvar, i)
      }
    }
  }

  ## Fix formula's environment for correct `...` scoping.
  attr(formula, ".Environment") <- environment()

  if(sum(c(tsvar, fnvar))>0){
    #Remove variables not needed in data (trend+season+functions)
    rmvar <- c(tsvar, fnvar)
    rmvar <- rmvar[rmvar!=attr(mt,"response")+1] #Never remove the reponse variable
    if(any(rmvar!=0)){
      vars <- vars[-rmvar]
    }
  }

  ## Grab any variables missing from data
  if(!missing(data)){
    #Check for any missing variables in data
    vars <- vars[c(TRUE, !as.character(vars[-1])%in%colnames(data))]
    dataname <- substitute(data)
  }
  if(!missing(data)){
    data <- datamat(do.call(datamat, as.list(vars[-1]), envir = parent.frame()),data)
  }
  else{
    data <- do.call(datamat, as.list(vars[-1]), envir = parent.frame())
  }

  ## Set column name of univariate dataset
  if(is.null(dim(data)) & length(data)!=0){
    cn <- as.character(vars)[2]
  } else{
    cn <- colnames(data)
  }

  ## Get time series attributes from the data
  if(is.null(tsp(data))){
    if((attr(mt,"response")+1)%in%fnvar){#Check unevaluated response variable
      tspx <- tsp(eval(attr(mt,"variables")[[attr(mt,"response")+1]]))
    }
    tspx <- tsp(data[,1])#Check for complex ts data.frame
  }
  else{
    tspx <- tsp(data)
  }
  if(is.null(tspx)){
    stop("Not time series data, use lm()")
  }
  tsdat <- match(c("trend", "season"), cn, 0L)

  ## Create trend and season if missing from the data
  if(tsdat[1]==0){#&tsvar[1]!=0){#If "trend" is not in data, but is in formula
    trend <- 1:NROW(data)
    cn <- c(cn,"trend")
    data <- cbind(data,trend)
  }
  if(tsdat[2]==0){#&tsvar[2]!=0){#If "season" is not in data, but is in formula
    if(tsvar[2]!=0 & tspx[3]==1){ # Nonseasonal data, and season requested
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    season <- as.factor(cycle(data[,1]))
    cn <- c(cn,"season")
    data <- cbind(data,season)
  }
  colnames(data) <- cn

  ## Subset the data according to subset argument
  if(!missing(subset)){
    if(!is.logical(subset))
      stop("subset must be logical")
    else if(NCOL(subset) > 1)
      stop("subset must be a logical vector")
    else if(NROW(subset) != NROW(data))
      stop("Subset must be the same length as the number of rows in the dataset")
    warning("Subset has been assumed contiguous")
    timesx <- time(data[,1])[subset]
    tspx <- recoverTSP(timesx)
    if(tspx[3]==1 & tsdat[2]==0 & tsvar[2]!=0){
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    data <- data[subset,]#model.frame(formula,as.data.frame(data[subsetTF,]))
  }
  if(!is.null(lambda)){
    data[,1] <- BoxCox(data[,1],lambda)
  }
  if(tsdat[2]==0&tsvar[2]!=0){
    data$season <- factor(data$season) #fix for lost factor information, may not be needed?
  }

  ## Fit the model and prepare model structure
  fit <- lm(formula,data=data,na.action=na.exclude,...)
  fit$residuals <- ts(residuals(fit))
  fit$fitted.values <- ts(fitted(fit))
  tsp(fit$residuals) <- tsp(fit$fitted.values) <- tsp(data[,1]) <- tspx
  fit$call <- cl
  fit$method <- "Linear regression model"
  if(exists("dataname")){
    fit$call$data <- dataname
  }
  if(!is.null(lambda)){
    attr(lambda, "biasadj") <- biasadj
    fit$lambda <- lambda
    fit$fitted.values <- InvBoxCox(fit$fitted.values, lambda, biasadj, var(fit$residuals))
  }
  return(fit)
}

forecast.lm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, biasadj=NULL, ts=TRUE, ...)
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
    origdata <- object$data #no longer exists
  else if(!is.null(object$model)){
    origdata <- object$model
  }
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
  else if(ts & is.element("ts",class(fitted(object)))){
    tspx <- tsp(fitted(object))
    timesx <- time(fitted(object))
  }
  else
    tspx <- NULL
  # if(!is.null(object$call$subset))
  # {
  #   j <- eval(object$call$subset)
  #   origdata <- origdata[j,]
  #   if(!is.null(tspx))
  #   {
  #     # Try to figure out times for subset. Assume they are contiguous.
  #     timesx <- timesx[j]
  #     tspx <- tsp(origdata) <- c(min(timesx),max(timesx),tspx[3])
  #   }
  # }
  # Add trend and seasonal to data frame
  oldterms <- terms(object)
  #Adjust terms for function variables and rename datamat colnames to match.
  if(!missing(newdata))
  {
    reqvars <- as.character(attr(object$terms,"variables")[-1])[-attr(object$terms,"response")]
    #Search for time series variables
    tsvar <- match(c("trend", "season"), reqvars, 0L)
    #Check if required variables are functions
    fnvar <- sapply(reqvars, function(x) !(is.symbol(parse(text=x)[[1]]) || typeof(eval(parse(text=x)[[1]][[1]]))!="closure"))
    if(!is.data.frame(newdata)){
      newdata <- datamat(newdata)
      colnames(newdata)[1] <- ifelse(sum(tsvar>0),reqvars[-tsvar][1],reqvars[1])
      warning("newdata column names not specified, defaulting to first variable required.")
    }
    oldnewdata <- newdata
    newvars <- make.names(colnames(newdata))
    #Check if variables are missing
    misvar <- match(make.names(reqvars), newvars, 0L)==0L
    if (any(!misvar & !fnvar)){ #If any variables are not missing/functions, add them to data
      tmpdata <- datamat(newdata[reqvars[!misvar]])
      rm1 <- FALSE
    }
    else{
      #Prefill the datamat
      tmpdata <- datamat(1:NROW(newdata))
      rm1 <- TRUE
    }
    #Remove trend and seasonality from required variables
    if(sum(tsvar)>0){
      reqvars <- reqvars[-tsvar]
      fnvar <- fnvar[-tsvar]
      misvar <- match(make.names(reqvars), newvars, 0L)==0L
    }
    if (any(misvar | fnvar)){ #If any variables are missing/functions
      reqvars <- reqvars[misvar | fnvar] #They are required
      fnvar <- fnvar[misvar | fnvar] #Update required function variables
      for (i in reqvars){
        found <- FALSE
        subvars <- NULL
        for(j in 1:length(object$coefficients)){
          subvars[j] <- pmatch(i,names(object$coefficients)[j])
        }
        subvars <- !is.na(subvars)
        subvars <- names(object$coefficients)[subvars]
        #Detect if subvars if multivariate
        if (length(subvars)>1){
          #Extract prefix only
          subvars <- substr(subvars, nchar(i)+1, 999L)
          fsub <- match(make.names(subvars), newvars, 0L)
          if (any(fsub == 0)){
            #Check for misnamed columns
            fsub <- grep(paste(make.names(subvars),collapse="|"), newvars)
          }
          if (all(fsub != 0) & length(fsub) == length(subvars)){
            imat <- as.matrix(newdata[,fsub], ncol = length(fsub))
            colnames(imat) <- subvars
            tmpdata[[length(tmpdata)+1]] <- imat
            found <- TRUE
          }
          else{
            #Attempt to evaluate it as a function
            subvars <- i
          }
        }
        if(length(subvars)==1){ #Check if it is a function
          if(fnvar[match(i, reqvars)]){#Pre-evaluate function from data
            tmpdata[[length(tmpdata)+1]] <- eval(parse(text=subvars)[[1]], newdata)
            found <- TRUE
          }
        }
        if(found){
          names(tmpdata)[length(tmpdata)] <- paste0("solvedFN___",match(i, reqvars))
          subvarloc <- match(i,lapply(attr(object$terms,"predvars"),deparse))
          attr(object$terms,"predvars")[[subvarloc]] <- attr(object$terms,"variables")[[subvarloc]] <- parse(text=paste0("solvedFN___",match(i, reqvars)))[[1]]
        }
        else{
          warning(paste0("Could not find required variable ", i, " in newdata. Specify newdata as a named data.frame"))
        }
      }
    }
    if(rm1){
      tmpdata[[1]] <- NULL
    }
    newdata <- tmpdata
    h <- nrow(newdata)
  }
  if(!is.null(tspx))
  {
    # Always generate trend series
    trend <- ifelse(is.null(origdata$trend), NCOL(origdata), max(origdata$trend)) + (1:h)
    if(!missing(newdata)){
      newdata <- cbind(newdata, trend)
    }
    else{
      newdata <- datamat(trend)
    }
    # Always generate season series
    x <- ts(1:h, start=tspx[2]+1/tspx[3], frequency=tspx[3])
    season <- as.factor(cycle(x))
    newdata <- cbind(newdata, season)
  }
  newdata <- as.data.frame(newdata)
  if(!exists("oldnewdata")){
    oldnewdata <- newdata
  }
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

  object$terms <- oldterms
  if(is.null(object$series)){ # Model produced via lm(), add series attribute
    object$series <- deparse(attr(oldterms, "variables")[[1 + attr(oldterms, "response")]])
  }
  fcast <- list(model=object,mean=out[[1]]$fit[,1],lower=out[[1]]$fit[,2],upper=out[[1]]$fit[,3],
                level=level,x=object$x,series=object$series)
  fcast$method <- "Linear regression model"
  fcast$newdata <- oldnewdata
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
    fcast$mean <- InvBoxCox(fcast$mean,lambda, biasadj, fcast)
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

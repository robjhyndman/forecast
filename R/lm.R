tslm <- function(formula,data,lambda=NULL,...)
{
  return(tswp(formula,data,lambda=NULL,model="lm",...))
}

forecast.lm <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...)
{
  return(forecast.wp(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...))
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



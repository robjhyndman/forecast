tsgam <- function(formula,data,lambda=NULL,...)
{
  return(tswp(formula,data,lambda=NULL,model="gam",...))
}

forecast.gam <- function(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...)
{
  return(forecast.wp(object, newdata, h=10, level=c(80,95), fan=FALSE, lambda=object$lambda, ts=TRUE, ...))
}




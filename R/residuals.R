residuals.bats <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$errors
  else
    getResponse(object) - fitted(object)
}

residuals.ets <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.Arima <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.ar <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$resid
  else
    getResponse(object) - fitted(object)
}

residuals.stlm <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.fracdiff <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
  {
    if(!is.null(object$residuals))   # Object produced by arfima()
      return(object$residuals)
    else                             # Object produced by fracdiff()
    {
      if (is.element("x", names(object)))
        x <- object$x
      else
        x <- eval.parent(parse(text=as.character(object$call)[2]))
      if(!is.null(object$lambda))
        x <- BoxCox(x,object$lambda)
      y <- fracdiff::diffseries(x - mean(x), d=object$d)
      fit <- arima(y, order=c(length(object$ar),0,length(object$ma)), include.mean=FALSE, fixed=c(object$ar,object$ma))
      return(residuals(fit, type="innovation"))
    }
  }
  else
    getResponse(object) - fitted(object)
}

residuals.forecast <- function(object, type=c("innovation","response"),...) 
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.geom_forecast <- function(object, type=c("innovation","response"),...) {
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}


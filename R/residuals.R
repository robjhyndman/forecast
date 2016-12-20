residuals.ar <- function(object, type=c("innovation","response"),...)
{
  type <- match.arg(type)
  # innovation and response residuals are the same for AR models
  object$resid
}

residuals.Arima <- function(object, type=c("innovation","response","regression"), h=1, ...)
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else if(type=="response")
    getResponse(object) - fitted(object, h=h)
  else
  {
    x <- getResponse(object)
    if(!is.null(object$lambda))
      x <- BoxCox(x,object$lambda)
    xreg <- getxreg(object)
    # Remove intercept
    if(is.element("intercept",names(object$coef)))
      xreg <- cbind(rep(1,length(x)),xreg)
    # Return errors
    if(is.null(xreg))
      return(x)
    else
    {
      norder <- sum(object$arma[1:4])
      return(ts(c(x - xreg %*% as.matrix(object$coef[(norder+1):length(object$coef)])),
        frequency=frequency(x),start=start(x)))
    }
  }
}

residuals.bats <- function(object, type=c("innovation","response"), h=1, ...)
{
  type <- match.arg(type)
  if(type=="innovation")
    object$errors
  else
    getResponse(object) - fitted(object, h=h)
}

residuals.ets <- function(object, type=c("innovation","response"), h=1, ...)
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object, h=h)
}

residuals.forecast <- function(object, type=c("innovation","response"), ...)
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.fracdiff <- function(object, type=c("innovation","response"), ...)
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
      fit <- arima(y, order=c(length(object$ar),0,length(object$ma)), include.mean=FALSE, fixed=c(object$ar,-object$ma))
      return(residuals(fit, type="innovation"))
    }
  }
  else
    getResponse(object) - fitted(object)
}

residuals.geom_forecast <- function(object, type=c("innovation","response"), ...) {
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

residuals.nnetar <- function(object, type=c("innovation","response"), h=1, ...)
{
  type <- match.arg(type)
  if(type=="innovation" & !is.null(object$lambda))
  {
    res <- matrix(unlist(lapply(object$model, residuals)), ncol=length(object$model))
    if(!is.null(object$scalex$scale))
      res <- res * object$scalex$scale
  }
  else
    res <- getResponse(object) - fitted(object, h=h)

  tspx <- tsp(getResponse(object))
  res <- ts(res, frequency=tspx[3L], end=tspx[2L])

  return(res)
}

residuals.stlm <- function(object, type=c("innovation","response"), ...)
{
  type <- match.arg(type)
  if(type=="innovation")
    object$residuals
  else
    getResponse(object) - fitted(object)
}

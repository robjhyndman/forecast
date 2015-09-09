# Functions to return the response variable for different models.
# If a Box-Cox transformation is used, the series returned here should
# be on the original scale, not the Box-Cox transformed scale.


getResponse <- function(object,...) UseMethod("getResponse")

getResponse.default <- function(object,...){
	if(is.list(object))
		return(object$x)
	else
		return(NULL)
}

getResponse.lm <- function(object,...) {
	responsevar <- as.character(formula(object))[2]
	ans <- model.frame(object$model)[,responsevar]
	return(ans)
}

getResponse.Arima <- function(object,...) {
	if (is.element("x", names(object))) 
    x <- object$x
  else
  {
  	series.name <- object$series
  	if(is.null(series.name))
  		stop("missing component series in Arima model")
  	else
  	{
    	x <- try(eval.parent(parse(text = series.name)),silent=TRUE)
    	if(is.element("try-error",class(x))) # Try one level further up the chain
    		x <- try(eval.parent(parse(text = series.name),2),silent=TRUE)
      if(is.element("try-error",class(x))) # Give up
        return(NULL)
  	}
  }
  return(as.ts(x))
}

getResponse.fracdiff <- function(object, ...) {
	if (is.element("x", names(object))) 
        x <- object$x
    else
 		x <- eval.parent(parse(text=as.character(object$call)[2]))
	if(is.null(tsp(x)))
		x <- ts(x,frequency=1,start=1)
	return(x)
}

getResponse.ar <- function(object, ...) {
  getResponse.Arima(object)
}

getResponse.tbats <- function(object,..){
  if(is.element("y", names(object))){
    y <- object$y
  } else {
    return(NULL)
  }
  return(as.ts(y))
}

getResponse.bats <- function(object,...){
  return(getResponse.tbats(object,...))
}

getResponse <- function(object,...) UseMethod("getResponse")

getResponse.default <- function(object,...){object$x}

getResponse.lm <- function(object,...) {
	responsevar <- as.character(formula(object$model))[2]
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
	    	x <- eval.parent(parse(text = series.name))
	}
	if(is.null(tsp(x)))
		x <- ts(x,frequency=1,start=1)
	return(x)
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
	if (is.element("x", names(object))) 
        x <- object$x
    else
		x <- eval.parent(parse(text=object$series))
	if(is.null(tsp(x)))
		x <- ts(x,frequency=1,start=1)
	return(x)
}

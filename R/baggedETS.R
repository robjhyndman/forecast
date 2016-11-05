##

baggedETS <- function(x, bootstrapped_series=bld.mbb.bootstrap(x, 100), ...)
{

  xname <- deparse(substitute(x))
  
  mod_boot <- lapply(bootstrapped_series, function(x) { 
        mod <- ets(x, ...) 
      })

  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$bootstrapped_series <- bootstrapped_series
  out$models <- mod_boot
    
  out$etsargs <- list(...)
  
  fitted_boot <- lapply(out$models, fitted)
  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
  out$fitted <- apply(fitted_boot, 1, mean)
    
#  out$fitted <- ts(rep(NA_real_, length(out$x)))
#  out$fitted[c(rep(TRUE, maxlag), j)] <- fits

  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted

  out$series <- xname
  out$method <- "baggedETS"

  out$call <- match.call()
  return(structure(out,class=c("baggedETS")))
}


forecast.baggedETS <- function(object, h=ifelse(frequency(object$x)>1, 2*frequency(object$x), 10), ...) {

  out <- list(model=object,x=object$x)
  #out <- object
  tspx <- tsp(out$x)
  
  forecasts_boot <- lapply(out$model$models, function(mod) { 
        forecast(mod, PI=FALSE, h=h)$mean
      })
  
  forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
  colnames(forecasts_boot) <- NULL
  
  if(!is.null(tspx))
    start.f <- tspx[2] + 1/frequency(out$x)
  else
    start.f <- length(out$x)+1

  #out <- list()
  out$forecasts_boot <- forecasts_boot
  
  out$mean <- ts(apply(forecasts_boot, 1, mean),frequency=frequency(out$x),start=start.f)
  out$median <- ts(apply(forecasts_boot, 1, median))
  out$lower <- ts(apply(forecasts_boot, 1, min))
  out$upper <- ts(apply(forecasts_boot, 1, max))
  out$level <- 100

  tsp(out$median) <- tsp(out$lower) <- tsp(out$upper) <- tsp(out$mean)
  
  class(out) <- "forecast"
  out
  
}


#fitted.baggedETS <- function(object, h=1, accum_func=mean, ...){
#  
#  fitted_boot <- lapply(object$models, fitted, h)
#  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
#  fitted_boot <- apply(fitted_boot, 2, accum_func)
#  fitted_boot
#}

#residuals.baggedETS <- function(object, h=1, ...){
#  
#  residuals_boot <- lapply(object$models, residuals, h)
#  residuals_boot <- as.matrix(as.data.frame(residuals_boot))
#  residuals_boot
#
#  #Alternative implementation:
#  #object$x - fitted(object, h)
#}

print.baggedETS <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  cat("Call:   ")
  print(x$call)
  
  #print(x$model)
  #cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits), "\n", sep = "")
  
  invisible(x)
}

is.baggedETS <- function(x){
  inherits(x, "baggedETS")
}



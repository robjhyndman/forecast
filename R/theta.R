#Implement standard Theta method of Assimakopoulos and Nikolopoulos (2000)
#More general methods are available in the forecTheta package

#Author: RJH



#' Theta method forecast
#' 
#' Returns forecasts and prediction intervals for a theta method forecast.
#' 
#' The theta method of Assimakopoulos and Nikolopoulos (2000) is equivalent to
#' simple exponential smoothing with drift. This is demonstrated in Hyndman and
#' Billah (2003).
#' 
#' The series is tested for seasonality using the test outlined in A&N. If
#' deemed seasonal, the series is seasonally adjusted using a classical
#' multiplicative decomposition before applying the theta method. The resulting
#' forecasts are then reseasonalized.
#' 
#' Prediction intervals are computed using the underlying state space model.
#' 
#' More general theta methods are available in the
#' \code{\link[forecTheta]{forecTheta}} package.
#' 
#' @param y a numeric vector or time series of class \code{ts}
#' @param h Number of periods for forecasting
#' @param level Confidence levels for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param x Deprecated. Included for backwards compatibility.
#' @return An object of class "\code{forecast}".
#' 
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#' 
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{rwf}.
#' 
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
#' That is x minus fitted values.} \item{fitted}{Fitted values (one-step
#' forecasts)}
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{arima}}, \code{\link{meanf}}, \code{\link{rwf}},
#' \code{\link{ses}}
#' @references Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model:
#' a decomposition approach to forecasting. \emph{International Journal of
#' Forecasting} \bold{16}, 521-530.
#' 
#' Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method.
#' \emph{International J. Forecasting}, \bold{19}, 287-290.
#' @keywords ts
#' @examples
#' nile.fcast <- thetaf(Nile)
#' plot(nile.fcast)
#' 
#' @export
thetaf <- function(y, h=ifelse(frequency(y)>1, 2*frequency(y), 10),
  level=c(80,95), fan=FALSE, x=y)
{
  # Check inputs
  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }

  # Check seasonality
  n <- length(x)
  x <- as.ts(x)
  m <- frequency(x)
  if(m > 1)
  {
    r <- as.numeric(acf(x, lag.max=m, plot=FALSE)$acf)[-1]
    stat <- sqrt((1 + 2*sum(r[-m]^2)) / n)
    seasonal <- (abs(r[m]) / stat > qnorm(0.95))
  }
  else
    seasonal <- FALSE

  # Seasonal decomposition
  origx <- x
  if(seasonal)
  {
    decomp <- decompose(x, type="multiplicative")
    x <- seasadj(decomp)
  }

  # Find theta lines
  fcast <- ses(x,h=h)
  tmp2 <- lsfit(0:(n-1),x)$coef[2]/2
  alpha <- fcast$model$par["alpha"]
  fcast$mean <- fcast$mean + tmp2*(0:(h-1) + (1-(1-alpha)^n)/alpha)

  # Reseasonalize
  if(seasonal)
    fcast$mean <- fcast$mean * rep(tail(decomp$seasonal, m), trunc(1+h/m))[1:h]

  # Find prediction intervals
  fcast.se <- sqrt(fcast$model$sigma) * sqrt((0:(h-1))*alpha^2+1)
  nconf <- length(level)
  fcast$lower <- fcast$upper <- ts(matrix(NA,nrow=h,ncol=nconf))
  tsp(fcast$lower) <- tsp(fcast$upper) <- tsp(fcast$mean)
  for(i in 1:nconf)
  {
    zt <- -qnorm( 0.5 - level[i]/200)
    fcast$lower[,i] <- fcast$mean - zt*fcast.se
    fcast$upper[,i] <- fcast$mean + zt*fcast.se
  }

  # Return results
  fcast$x <- origx
  fcast$level <- level
  fcast$method <- "Theta"
  fcast$model <- list(alpha=alpha,drift=tmp2,sigma=fcast$model$sigma)
  fcast$model$call <- match.call()
  return(fcast)
}


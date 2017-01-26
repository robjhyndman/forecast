#Implement standard Theta method of Assimakopoulos and Nikolopoulos (2000)
#More general methods are available in the forecTheta package

#Author: RJH

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


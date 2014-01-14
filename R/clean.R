# Functions to remove outliers and fill missing values in a time series
# Nothing for multiple seasonality yet.

# na.interp fills in missing values
# Uses linear interpolation for non-seasonal series
# Adds seasonality based on a periodic stl decomposition with seasonal series
# Argument lambda allows for Box-cox transformation

na.interp <- function(x, lambda = NULL)
{
  missng <- is.na(x) 
  # Do nothing if no missing values
  if(sum(missng)==0)
    return(x)

  if(is.null(tsp(x)))
    x <- ts(x)
  freq <- frequency(x)
  n <- length(x)
  tt <- 1:n

  idx <- tt[!missng]
  if(freq <= 1 | n <= 2*freq) # Non-seasonal -- use linear interpolation
  {
  	if(!is.null(lambda)) {
  	  x[idx] <- BoxCox(x[idx], lambda = lambda)
  	  xx <- as.ts(approx(idx,x[idx],1:n, rule=2)$y)
  	  xx <- InvBoxCox(xx, lambda = lambda)  # back-transformed
  	} 
    else
  	  xx <- as.ts(approx(idx,x[idx],1:n, rule=2)$y)
    tsp(xx) <- tsp(x)
    return(xx)
  }
  # Otherwise a seasonal series
  # Estimate seasonal component robustly
  # Then add to linear interpolation of seasonally adjusted series
  else
  {
    # Fit Fourier series for seasonality and a cubic polynomial for the trend, 
    #just to get something reasonable to start with
  	if(!is.null(lambda)) {
  	  x <- BoxCox(x, lambda = lambda)
  	}
    X <- cbind(fourier(x,3),poly(tt,degree=3))
    fit <- lm(x ~ X, na.action=na.exclude)
    pred <- predict(fit, newdata =data.frame(X))
    x[missng] <- pred[missng]
    # Now re-do it with stl to get better results
    fit <- stl(x,s.window=11,robust=TRUE)
    # Interpolate seasonally adjusted values
    sa <- seasadj(fit)
    sa <- approx(idx,sa[idx],1:n, rule=2)$y
    # Replace original missing values
    x[missng] <- sa[missng] + fit$time.series[missng,"seasonal"]
  	if(!is.null(lambda))
  	{
  	  x <- InvBoxCox(x, lambda = lambda)
  	}
    return(x)
  }
}

# Function to identify outliers and replace them with better values
# Missing values replaced as well if replace.missing=TRUE

tsclean <- function(x, replace.missing=TRUE, lambda = NULL)
{
  outliers <- tsoutliers(x, lambda = lambda)
  x[outliers$index] <- outliers$replacements
  if(replace.missing)
    x <- na.interp(x, lambda = lambda)
  return(x)
}

# Function to identify time series outlieres
tsoutliers <- function(x, iterate=2, lambda = NULL)
{
  # Identify missing values
  missng <- is.na(x)
  n <- length(x)

  # Seasonal data
  if(frequency(x)>1)
  {
	xx <- na.interp(x, lambda = lambda)
    if(!is.null(lambda))
	{
	  xx <- BoxCox(xx, lambda = lambda)
	}
    fit <- stl(xx, s.window="periodic", robust=TRUE)
    resid <- fit$time.series[,"remainder"]
    # Make sure missing values are not interpeted as outliers
    resid[missng] <- NA
  }
  else # Non-seasonal data
  {
    tt <- 1:n
    if(!is.null(lambda))
	{
	  xx <- BoxCox(x, lambda = lambda)
	} else
	  xx <- x
    mod <- loess(xx ~ tt, na.action=na.exclude, family="symmetric", degree=1, span=min(20/n, 0.75))
    resid <- xx-fitted(mod)
  }

  # Limits of acceptable residuals
  resid.q <- quantile(resid, prob=c(0.1,0.9), na.rm=TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + 2*iqr*c(-1,1)

  # Find residuals outside limits
  outliers <- which((resid < limits[1]) | (resid > limits[2]))

  # Replace all missing values including outliers
  x[outliers] <- NA
  x <- na.interp(x, lambda = lambda)

  # Iterate only for non-seasonal data as stl includes iteration
  # Do no more than 2 iterations regardless of the value of iterate
  if(iterate > 1 & frequency(x)<=1)
  {
    tmp <- tsoutliers(x, iterate=1, lambda = lambda)
    if(length(tmp$index) > 0) # Found some more
    {
      outliers <- sort(c(outliers,tmp$index))
      x[outliers] <- NA
      x <- na.interp(x, lambda = lambda)
    }
  }

  # Return outlier indexes and replacements
  return(list(index=outliers, replacements=x[outliers]))
}

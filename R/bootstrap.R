#Bootstrap functions

# Trend estimation like STL without seasonality.
# Non-robust version
tl <- function(x, ...)
{
  x <- as.ts(x)
  tspx <- tsp(x)
  n <- length(x)
  tt <- 1:n
  fit <- supsmu(tt, x)
  out <- ts(cbind(trend=fit$y, remainder=x-fit$y))
  tsp(out) <- tsp(x)

  out <- structure(list(time.series=out),class="stl")
  return(out)
}

# Function to return some bootstrap samples of x
# based on LPB
lpb <- function(x, nsim=100)
{
  n <- length(x)
  meanx <- mean(x)
  y <- x - meanx
  gamma <- wacf(y, lag.max=n)$acf[,,1]
  s <- length(gamma)
  Gamma <- matrix(1, s, s)
  d <- row(Gamma) - col(Gamma)
  for(i in 1:(s-1))
    Gamma[d==i | d==(-i)] <- gamma[i+1]
  L <- t(chol(Gamma))
  W <- solve(L) %*% matrix(y,ncol=1)
  out <- ts(L %*% matrix(sample(W, n*nsim, replace=TRUE), nrow=n, ncol=nsim) + meanx)
  tsp(out) <- tsp(x)
  return(out)
}

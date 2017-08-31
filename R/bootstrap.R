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



# Bootstrapping time series (based on Bergmeir et al., 2016, IJF paper)
# Author: Fotios Petropoulos

MBB <- function(x, window_size) {

  bx <- array(0, (floor(length(x)/window_size)+2)*window_size)
  for (i in 1:(floor(length(x)/window_size)+2)){
    c <- sample(1:(length(x)-window_size+1),1)
    bx[((i-1)*window_size+1):(i*window_size)] <- x[c:(c+window_size-1)]
  }
  start_from <- sample(0:(window_size-1),1) + 1
  bx[start_from:(start_from+length(x)-1)]
}




#' Box-Cox and Loess-based decomposition bootstrap.
#'
#' Generates bootstrapped versions of a time series using the Box-Cox and
#' Loess-based decomposition bootstrap.
#'
#' The procedure is described in Bergmeir et al. Box-Cox decomposition is
#' applied, together with STL or Loess (for non-seasonal time series), and the
#' remainder is bootstrapped using a moving block bootstrap.
#'
#' @param x Original time series.
#' @param num Number of bootstrapped versions to generate.
#' @param block_size Block size for the moving block bootstrap.
#' @return A list with bootstrapped versions of the series. The first series in
#' the list is the original series.
#' @author Christoph Bergmeir, Fotios Petropoulos
#' @seealso \code{\link{baggedETS}}.
#' @references Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
#' Exponential Smoothing Methods using STL Decomposition and Box-Cox
#' Transformation. International Journal of Forecasting 32, 303-312.
#' @keywords ts
#' @examples
#' bootstrapped_series <- bld.mbb.bootstrap(WWWusage, 100)
#'
#' @export
bld.mbb.bootstrap <- function(x, num, block_size=NULL)
{

  freq <- frequency(x)
  if(is.null(block_size))
  {
    block_size <- ifelse(freq > 1, 2*freq, min(8, floor(length(x)/ 2)))
  }

  xs <- list()
  xs[[1]] <- x # the first series is the original one

  if (num>1){
    # Box-Cox transformation
    lambda <- BoxCox.lambda(x, lower=0, upper=1)
    x.bc <- BoxCox(x, lambda)

    if (freq>1){
      # STL decomposition
      x.stl <- stl(ts(x.bc, frequency=freq), "per")$time.series
      seasonal <- x.stl[,1]
      trend <- x.stl[,2]
      remainder <- x.stl[,3]
    } else {
      # Loess
      trend <- 1:length(x)
      suppressWarnings(x.loess <- loess(x.bc ~ trend, span=6/length(x), degree=1))
      seasonal <- rep(0, length(x))
      trend <- x.loess$fitted
      remainder <- x.loess$residuals
    }

    # Bootstrap some series, using MBB
    for (i in 2:num){
      xs[[i]] <- InvBoxCox(trend + seasonal + MBB(remainder, block_size), lambda)
    }
  }

  xs
}

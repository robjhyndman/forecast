## A function determining the appropriate period, if the data is of unknown period
## Written by Rob Hyndman

#' Find dominant frequency of a time series
#'
#' \code{findfrequency} returns the period of the dominant frequency of a time
#' series. For seasonal data, it will return the seasonal period. For cyclic
#' data, it will return the average cycle length.
#'
#' The dominant frequency is determined from a spectral analysis of the time
#' series. First, a linear trend is removed, then the spectral density function
#' is estimated from the best fitting autoregressive model (based on the AIC).
#' If there is a large (possibly local) maximum in the spectral density
#' function at frequency \eqn{f}, then the function will return the period
#' \eqn{1/f} (rounded to the nearest integer). If no such dominant frequency
#' can be found, the function will return 1.
#'
#' @param x a numeric vector or time series of class \code{ts}
#' @return an integer value
#' @author Rob J Hyndman
#' @keywords ts
#' @examples
#'
#' findfrequency(USAccDeaths) # Monthly data
#' findfrequency(taylor) # Half-hourly data
#' findfrequency(lynx) # Annual data
#'
#' @export
findfrequency <- function(x) {
  n <- length(x)
  x <- as.ts(x)
  # Remove trend from data
  x <- residuals(tslm(x ~ trend))
  # Compute spectrum by fitting ar model to largest section of x
  n.freq <- 500
  spec <- spec.ar(c(na.contiguous(x)), plot = FALSE, n.freq = n.freq)
  if (max(spec$spec) > 10) # Arbitrary threshold chosen by trial and error.
  {
    period <- floor(1 / spec$freq[which.max(spec$spec)] + 0.5)
    if (period == Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec) > 0)
      if (length(j) > 0) {
        nextmax <- j[1] + which.max(spec$spec[(j[1] + 1):n.freq])
        if (nextmax < length(spec$freq)) {
          period <- floor(1 / spec$freq[nextmax] + 0.5)
        } else {
          period <- 1L
        }
      }
      else {
        period <- 1L
      }
    }
  }
  else {
    period <- 1L
  }

  return(as.integer(period))
}

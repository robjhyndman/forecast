#' Multi-Seasonal Time Series
#'
#' msts is an S3 class for multi seasonal time series objects, intended to be
#' used for models that support multiple seasonal periods. The msts class
#' inherits from the ts class and has an additional "msts" attribute which
#' contains the vector of seasonal periods. All methods that work on a ts
#' class, should also work on a msts class.
#'
#' @aliases print.msts window.msts `[.msts`
#'
#' @param data A numeric vector, ts object, matrix or data frame. It is
#' intended that the time series data is univariate, otherwise treated the same
#' as ts().
#' @param seasonal.periods A vector of the seasonal periods of the msts.
#' @param ts.frequency The seasonal period that should be used as frequency of
#' the underlying ts object. The default value is `max(seasonal.periods)`.
#' @param ... Arguments to be passed to the underlying call to `ts()`. For
#' example `start=c(1987, 5)`.
#' @return An object of class `c("msts", "ts")`. If there is only one
#' seasonal period (i.e., `length(seasonal.periods) == 1`), then the object
#' is of class `ts`.
#' @author Slava Razbash and Rob J Hyndman
#' @keywords ts
#' @examples
#'
#' x <- msts(taylor, seasonal.periods = c(2 * 24, 2 * 24 * 7, 2 * 24 * 365), start = 2000 + 22 / 52)
#' y <- msts(USAccDeaths, seasonal.periods = 12, start = 1949)
#'
#' @export
msts <- function(
  data,
  seasonal.periods,
  ts.frequency = floor(max(seasonal.periods)),
  ...
) {
  # if(!is.element(ts.frequency, round(seasonal.periods-0.5+1e-12)))
  #  stop("ts.frequency should be one of the seasonal periods")

  if (is.ts(data) && frequency(data) == ts.frequency && ...length() == 0) {
    object <- data
  } else {
    object <- ts(data = data, frequency = ts.frequency, ...)
  }
  if (length(seasonal.periods) > 1L) {
    class(object) <- c("msts", "ts")
    attr(object, "msts") <- sort(seasonal.periods)
  }
  object
}

#' @export
print.msts <- function(x, ...) {
  cat("Multi-Seasonal Time Series:\n")
  cat("Start: ")
  cat(start(x))
  # cat("\nEnd: ")
  # cat(x$end)
  cat("\nSeasonal Periods: ")
  cat(attr(x, "msts"))
  cat("\nData:\n")
  xx <- unclass(x) # handles both univariate and multivariate ts
  attr(xx, "tsp") <- attr(xx, "msts") <- NULL
  print(xx)
  # print(matrix(x, ncol=length(x)), nrow=1)
  cat("\n")
}

#' @export
window.msts <- function(x, ...) {
  seasonal.periods <- attr(x, "msts")
  class(x) <- "ts"
  x <- window(x, ...)
  class(x) <- c("msts", "ts")
  attr(x, "msts") <- seasonal.periods
  x
}

#' @export
`[.msts` <- function(x, i, j, drop = TRUE) {
  y <- NextMethod("[")
  if (!is.ts(y)) {
    return(y)
  }
  class(y) <- c("msts", class(y))
  attr(y, "msts") <- attr(x, "msts")
  y
}

# Copy msts attributes from x to y
copy_msts <- function(x, y) {
  if (NROW(x) > NROW(y)) {
    # Pad y with initial NAs
    if (NCOL(y) == 1) {
      y <- c(rep(NA, NROW(x) - NROW(y)), y)
    } else {
      y <- rbind(matrix(NA, ncol = NCOL(y), nrow = NROW(x) - NROW(y)), y)
    }
  } else if (NROW(x) != NROW(y)) {
    stop("x and y should have the same number of observations")
  }
  if (NCOL(y) > 1) {
    class(y) <- c("mts", "ts", "matrix")
  } else {
    class(y) <- "ts"
  }
  if (inherits(x, "msts")) {
    class(y) <- c("msts", class(y))
  }
  attr <- attributes(x)
  attributes(y)$tsp <- attr$tsp
  attributes(y)$msts <- attr$msts
  y
}

# Copy msts attributes from x to y shifted to forecast period
future_msts <- function(x, y) {
  if (NCOL(y) > 1) {
    class(y) <- c("mts", "ts", "matrix")
  } else {
    class(y) <- "ts"
  }
  if (inherits(x, "msts")) {
    class(y) <- c("msts", class(y))
  }
  attr <- attributes(x)
  attr$tsp[1:2] <- attr$tsp[2] + c(1, NROW(y)) / attr$tsp[3]
  attributes(y)$tsp <- attr$tsp
  attributes(y)$msts <- attr$msts
  y
}

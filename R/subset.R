#' Subsetting a time series
#'
#' Various types of subsetting of a time series. Allows subsetting by index
#' values (unlike \code{\link[stats]{window}}). Also allows extraction of the
#' values of a specific season or subset of seasons in each year. For example,
#' to extract all values for the month of May from a time series.
#'
#' If character values for months are used, either upper or lower case may be
#' used, and partial unambiguous names are acceptable. Possible character
#' values for quarters are \code{"Q1"}, \code{"Q2"}, \code{"Q3"}, and
#' \code{"Q4"}.
#'
#' @param x a univariate time series to be subsetted
#' @param subset optional logical expression indicating elements to keep;
#' missing values are taken as false. \code{subset} must be the same length as
#' \code{x}.
#' @param month Numeric or character vector of months to retain. Partial
#' matching on month names used.
#' @param quarter Numeric or character vector of quarters to retain.
#' @param season Numeric vector of seasons to retain.
#' @param start Index of start of contiguous subset.
#' @param end Index of end of contiguous subset.
#' @param ... Other arguments, unused.
#' @return If \code{subset} is used, a numeric vector is returned with no ts
#' attributes. If \code{start} and/or \code{end} are used, a ts object is
#' returned consisting of x[start:end], with the appropriate time series
#' attributes retained. Otherwise, a ts object is returned with frequency equal
#' to the length of \code{month}, \code{quarter} or \code{season}.
#' @author Rob J Hyndman
#' @seealso \code{\link[base]{subset}}, \code{\link[stats]{window}}
#' @keywords ts
#' @examples
#' plot(subset(gas,month="November"))
#' subset(woolyrnq,quarter=3)
#' subset(USAccDeaths, start=49)
#'
#' @export
subset.ts <- function(x, subset=NULL, month=NULL, quarter=NULL, season=NULL,
                      start=NULL, end=NULL, ...) {
  if (!is.null(subset)) {
    if (NROW(subset) != NROW(x)) {
      stop("subset must be the same length as x")
    }
    if (NCOL(subset) != 1) {
      stop("subset must be a vector of rows to keep")
    }
    if ("mts" %in% class(x)) {
      return(subset.matrix(x, subset))
    }
    else {
      return(subset.default(x, subset))
    }
  }
  else if (!is.null(start) | !is.null(end)) {
    if (is.null(start)) {
      start <- 1
    }
    if (is.null(end)) {
      end <- NROW(x)
    }
    if ("mts" %in% class(x)) {
      xsub <- x[start:end, , drop=FALSE]
    } else {
      xsub <- x[start:end]
    }
    tspx <- tsp(x)
    return(ts(xsub, frequency = tspx[3], start = tspx[1L] + (start - 1) / tspx[3L]))
  }
  else if (frequency(x) <= 1) {
    stop("Data must be seasonal")
  }
  if (!is.null(month)) {
    if (frequency(x) != 12) {
      stop("Data is not monthly")
    }
    if (is.character(month)) {
      season <- pmatch(tolower(month), tolower(month.name), duplicates.ok = TRUE)
    } else {
      season <- month
    }
    season <- na.omit(season)
    if (length(season) == 0L) {
      stop("No recognizable months")
    }
    if (min(season) < 1L | max(season) > 12L) {
      stop("Months must be between 1 and 12")
    }
  }
  else if (!is.null(quarter)) {
    if (frequency(x) != 4) {
      stop("Data is not quarterly")
    }
    if (is.character(quarter)) {
      season <- pmatch(tolower(quarter), paste("q", 1:4, sep = ""), duplicates.ok = TRUE)
    } else {
      season <- quarter
    }
    season <- na.omit(season)
    if (length(season) == 0L) {
      stop("No recognizable quarters")
    }
    if (min(season) < 1L | max(season) > 4L) {
      stop("Quarters must be between 1 and 4")
    }
  }
  else if (is.null(season)) {
    stop("No subset specified")
  } else
  if (min(season) < 1L | max(season) > frequency(x)) {
    stop(paste("Seasons must be between 1 and", frequency(x)))
  }

  start <- utils::head(time(x)[is.element(cycle(x), season)], 1)
  if ("mts" %in% class(x)) {
    x <- subset.matrix(x, is.element(cycle(x), season))
  }
  else {
    x <- subset.default(x, is.element(cycle(x), season))
  }
  return(ts(x, frequency = length(season), start = start))
}

# head.ts and tail.ts only defined/exported for R < 4.5.0
# due to new base R functions.

#' @importFrom utils head.matrix
#' @importFrom utils tail.matrix
#' @rawNamespace if (getRversion() < "4.5.0") S3method(head, ts)
#' @rawNamespace if (getRversion() < "4.5.0") S3method(tail, ts)

if(getRversion() < "4.5.0") {
  head.ts <- function(x, n=6L, ...) {
    attr_x <- attributes(x)
    attr_x$names <- NULL
    if (NCOL(x) > 1) {
      hx <- head.matrix(as.matrix(x), n = n, ...)
    } else if ((length(x) + n) > 0) {
      hx <- head(c(x), n = n, ...)
    } else {
      return(numeric(0))
    }
    attr_x$tsp[2] <- attr_x$tsp[1] + (NROW(hx) - 1) / attr_x$tsp[3]
    if (!is.null(dim(x))) {
      attr_x$dim[1] <- NROW(hx)
    }
    attributes(hx) <- attr_x
    return(hx)
  }
  tail.ts <- function(x, n=6L, ...) {
    attr_x <- attributes(x)
    attr_x$names <- NULL
    if (NCOL(x) > 1) {
      hx <- tail.matrix(as.matrix(x), n = n, ...)
    } else if ((length(x) + n) > 0) {
      hx <- tail(c(x), n = n, ...)
    } else {
      return(numeric(0))
    }
    attr_x$tsp[1] <- attr_x$tsp[2] - (NROW(hx) - 1) / attr_x$tsp[3]
    if (!is.null(dim(x))) {
      attr_x$dim[1] <- NROW(hx)
    }
    attributes(hx) <- attr_x
    return(hx)
  }
}

#' @rdname subset.ts
#' @export
subset.msts <- function(x, subset=NULL, start=NULL, end=NULL, ...) {
  out <- subset.ts(x, start = start, end = end, ...)
  tspx <- tsp(out)
  msts(
    out, seasonal.periods = attr(x, "msts"),
    start = tspx[1], ts.frequency = tspx[3]
  )
}

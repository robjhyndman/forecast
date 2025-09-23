# Functions to extract components from time series decomposition
# These should match corresponding functions in the seasonal package
# providing similar functional for stl, decomposed.ts and tbats objects

#' Extract components from a time series decomposition
#'
#' Returns a univariate time series equal to either a seasonal component,
#' trend-cycle component or remainder component from a time series
#' decomposition.
#'
#' @param object Object created by [stats::decompose()],
#' [stats::stl()] or [tbats()].
#' @return Univariate time series.
#' @author Rob J Hyndman
#' @seealso [stats::stl()], [stats::decompose()], [tbats()], [seasadj()].
#' @keywords ts
#' @examples
#' plot(USAccDeaths)
#' fit <- stl(USAccDeaths, s.window = "periodic")
#' lines(trendcycle(fit), col = "red")
#'
#' library(ggplot2)
#' autoplot(
#'   cbind(
#'     Data = USAccDeaths,
#'     Seasonal = seasonal(fit),
#'     Trend = trendcycle(fit),
#'     Remainder = remainder(fit)
#'   ),
#'   facets = TRUE
#' ) +
#'   labs(x = "Year", y = "")
#'
#' @export
seasonal <- function(object) {
  if (inherits(object, "mstl")) {
    cols <- grep("Season", colnames(object), fixed = TRUE)
    return(object[, cols])
  } else if (inherits(object, "stl")) {
    return(object$time.series[, "seasonal"])
  } else if (inherits(object, "decomposed.ts")) {
    return(object$seasonal)
  } else if (inherits(object, "tbats")) {
    comp <- tbats.components(object)
    scols <- grep("season", colnames(comp), fixed = TRUE)
    season <- ts(rowSums(comp[, scols, drop = FALSE]))
    if (!is.null(object$lambda)) {
      season <- InvBoxCox(season, object$lambda)
    }
    tsp(season) <- tsp(comp)
    return(season)
  } else if (inherits(object, "seas")) {
    return(object$data[, "seasonal"])
  } else {
    stop("Unknown object type")
  }
}

#' @rdname seasonal
#' @export
trendcycle <- function(object) {
  if (inherits(object, "mstl")) {
    return(object[, "Trend"])
  } else if (inherits(object, "stl")) {
    return(object$time.series[, "trend"])
  } else if (inherits(object, "decomposed.ts")) {
    # 	else if("tbats" %in% class(object))
    return(object$trend)
  } else if (inherits(object, "seas")) {
    # 	{
    #     trnd <- tbats.components(object)[,"level"]
    #     if (!is.null(object$lambda))
    #       trnd <- InvBoxCox(trnd, object$lambda)
    #     return(trnd)
    #   }
    return(seasextract_w_na_action(object, "trend"))
  } else {
    stop("Unknown object type")
  }
}

#' @rdname seasonal
#' @export
remainder <- function(object) {
  if (inherits(object, "mstl")) {
    return(object[, "Remainder"])
  } else if (inherits(object, "stl")) {
    return(object$time.series[, "remainder"])
  } else if (inherits(object, "decomposed.ts")) {
    # 	else if("tbats" %in% class(object))
    return(object$random)
  } else if (inherits(object, "seas")) {
    # 	{
    # 		comp <- tbats.components(object)
    # 		trnd <- comp[,"level"]
    # 		scols <- grep("season",colnames(comp))
    #     season <- rowSums(comp[,scols,drop=FALSE])
    #     irreg <- ts(comp[,'observed'] - trnd - season)
    #     tsp(irreg) <- tsp(comp)
    #     return(irreg)
    #   }
    return(seasextract_w_na_action(object, "irregular"))
  } else {
    stop("Unknown object type")
  }
}

## Copied from seasonal:::extract_w_na_action
## Importing is problematic due to issues with ARM processors

seasextract_w_na_action <- function(x, name) {
  if (is.null(x$data)) {
    return(NULL)
  }
  z <- na.omit(x$data[, name])
  if (!is.null(x$na.action)) {
    if (attr(x$na.action, "class") == "exclude") {
      z <- ts(stats::napredict(x$na.action, z))
      tsp(z) <- tsp(x$x)
    }
  }
  z
}

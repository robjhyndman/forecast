## Generic seasadj functions

#' Seasonal adjustment
#'
#' Returns seasonally adjusted data constructed by removing the seasonal
#' component.
#'
#'
#' @param object Object created by [stats::decompose()], [stats::stl()] or
#' [tbats()].
#' @param ... Other arguments not currently used.
#' @return Univariate time series.
#' @author Rob J Hyndman
#' @seealso [stats::stl()], [stats::decompose()], [tbats()].
#' @keywords ts
#' @examples
#' plot(AirPassengers)
#' lines(seasadj(decompose(AirPassengers, "multiplicative")), col = 4)
#'
#' @export
seasadj <- function(object, ...) UseMethod("seasadj")

#' @rdname seasadj
#' @export
seasadj.stl <- function(object, ...) {
  trendcycle(object) + remainder(object)
}

#' @rdname seasadj
#' @export
seasadj.mstl <- function(object, ...) {
  trendcycle(object) + remainder(object)
}

#' @rdname seasadj
#' @export
seasadj.decomposed.ts <- function(object, ...) {
  if (object$type == "additive") {
    object$x - object$seasonal
  } else {
    object$x / object$seasonal
  }
}

#' @rdname seasadj
#' @export
seasadj.tbats <- function(object, ...) {
  object$y - seasonal(object)
  # comp <- tbats.components(object)
  # scols <- grep("season",colnames(comp))
  # sa <- comp[,"observed"] - rowSums(comp[,scols,drop=FALSE])
  # # Back transform if necessary
  # if (!is.null(object$lambda))
  #   sa <- InvBoxCox(sa, object$lambda)
  # return(sa)
}

#' @rdname seasadj
#' @export
seasadj.seas <- function(object, ...) {
  seasextract_w_na_action(object, "final")
}

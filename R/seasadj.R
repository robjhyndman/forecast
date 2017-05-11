## Generic seasadj functions



#' Seasonal adjustment
#' 
#' Returns seasonally adjusted data constructed by removing the seasonal
#' component.
#' 
#' 
#' @param object Object created by \code{\link[stats]{decompose}},
#' \code{\link[stats]{stl}} or \code{\link{tbats}}.
#' @param ... Other arguments not currently used.
#' @return Univariate time series.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{stl}}, \code{\link[stats]{decompose}},
#' \code{\link{tbats}}.
#' @keywords ts
#' @examples
#' plot(AirPassengers)
#' lines(seasadj(decompose(AirPassengers,"multiplicative")),col=4)
#' 
#' @export
seasadj <- function(object,...) UseMethod("seasadj")

#' @rdname seasadj
#' @export
seasadj.stl <- function(object, ...)
{
   return(trendcycle(object) + remainder(object))
}

#' @rdname seasadj
#' @export
seasadj.decomposed.ts <- function(object, ...)
{
  if(object$type=="additive")
    return(object$x - object$seasonal)
  else
    return(object$x / object$seasonal)
}

#' @rdname seasadj
#' @export
seasadj.tbats <- function(object, ...)
{
  return(object$y - seasonal(object))
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
seasadj.seas <- function(object, ...)
{
  return(seasextract_w_na_action(object, "final"))
}

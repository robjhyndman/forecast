## Generic seasadj functions

seasadj <- function(object,...) UseMethod("seasadj")

seasadj.stl <- function(object, ...)
{
   return(object$time.series[,2] + object$time.series[,3])
}

seasadj.decomposed.ts <- function(object, ...)
{
  if(object$type=="additive")
    return(object$x - object$seasonal)
  else
    return(object$x / object$seasonal)
}

seasadj.tbats <- function(object, ...)
{
  comp <- tbats.components(object)
  scols <- grep("season",colnames(comp))
  sa <- comp[,"observed"] - rowSums(comp[,scols,drop=FALSE])
  # Back transform if necessary
  if (!is.null(object$lambda))
    sa <- InvBoxCox(sa, object$lambda)
  return(sa)
}

# Functions to extract components from time series decomposition
# These should match corresponding functions in the seasonal package
# providing similar functional for stl, decomposed.ts and tbats objects

seasonal <- function(object)
{
	if("stl" %in% class(object))
		return(object$time.series[,"seasonal"])
	else if("decomposed.ts" %in% class(object))
		return(object$seasonal)
	else if("tbats" %in% class(object))
	{
    comp <- tbats.components(object)
    scols <- grep("season",colnames(comp))
    season <- ts(rowSums(comp[,scols,drop=FALSE]))
    if (!is.null(object$lambda))
      season <- InvBoxCox(season, object$lambda)
    tsp(season) <- tsp(comp)
    return(season)
	}
  else if("seas" %in% class(object))
    stop("No idea")
  else
    stop("Unknown object type")
}

trendcycle <- function(object)
{
	if("stl" %in% class(object))
		return(object$time.series[,"trend"])
	else if("decomposed.ts" %in% class(object))
		return(object$trend)
# 	else if("tbats" %in% class(object))
# 	{
#     trnd <- tbats.components(object)[,"level"]
#     if (!is.null(object$lambda))
#       trnd <- InvBoxCox(trnd, object$lambda)
#     return(trnd)
#   }
  else if("seas" %in% class(object))
    return(seasonal::trend(object))
	else
	  stop("Unknown object type")
}

remainder <- function(object)
{
	if("stl" %in% class(object))
		return(object$time.series[,"remainder"])
	else if("decomposed.ts" %in% class(object))
		return(object$random)
# 	else if("tbats" %in% class(object))
# 	{
# 		comp <- tbats.components(object)
# 		trnd <- comp[,"level"]
# 		scols <- grep("season",colnames(comp))
#     season <- rowSums(comp[,scols,drop=FALSE])
#     irreg <- ts(comp[,'observed'] - trnd - season)
#     tsp(irreg) <- tsp(comp)
#     return(irreg)
#   }
  else if("seas" %in% class(object))
    return(seasonal::irregular(object))
	else
	  stop("Unknown object type")
}


subset.ts <- function(x, subset=NULL, month=NULL, quarter=NULL, season=NULL, ...)
{
  if(!is.null(subset))
    return(subset.default(x,subset))
  else if(frequency(x) <= 1)
    stop("Data must be seasonal")
  if(!is.null(month))
    season <- pmatch(month,month.name,duplicates.ok=TRUE)
  else if(!is.null(quarter))
    season <- quarter
  else if(is.null(season))
    stop("No subset specified")
  x.time <- subset(time(x),cycle(x)==season)
  if(length(season)==1) # one value per year
    return(ts(subset(x,cycle(x)==season),frequency=1,start=x.time[1]))
  else # ignore ts attributes
    return(subset(x,cycle(x)==season))
}

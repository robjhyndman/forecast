subset.ts <- function(x, subset=NULL, month=NULL, quarter=NULL, season=NULL, ...)
{
  if(!is.null(subset))
  {
    if(length(subset) != length(x))
      stop("subset must be the same length as x")
    return(subset.default(x,subset))
  }
  else if(frequency(x) <= 1)
    stop("Data must be seasonal")
  if(!is.null(month))
  {
    if(frequency(x) != 12)
      stop("Data is not monthly")
    if(is.character(month))
      season <- pmatch(tolower(month),tolower(month.name),duplicates.ok=TRUE)
    else
      season <- month
    season <- na.omit(season)
    if(length(season) == 0L)
      stop("No recognizable months")
    if(min(season) < 1L | max(season) > 12L)
      stop("Months must be between 1 and 12")
  }
  else if(!is.null(quarter))
  {
    if(frequency(x) != 4)
      stop("Data is not quarterly")
    if(is.character(quarter))
      season <- pmatch(tolower(quarter),paste("q",1:4,sep=""),duplicates.ok=TRUE)
    else
      season <- quarter
    season <- na.omit(season)
    if(length(season) == 0L)
      stop("No recognizable quarters")
    if(min(season) < 1 | max(season) > 4)
      stop("Quarters must be between 1 and 4")
  }
  else if(is.null(season))
    stop("No subset specified")

  start <- head(time(x)[is.element(cycle(x), season)],1)
  x <- subset.default(x, is.element(cycle(x), season))
    return(ts(x, frequency=length(season), start=start))
}

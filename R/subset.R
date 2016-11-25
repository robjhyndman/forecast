subset.ts <- function(x, subset=NULL, month=NULL, quarter=NULL, season=NULL, 
  start=NULL, end=NULL, ...)
{
  if(!is.null(subset))
  {
    if(NROW(subset) != NROW(x))
      stop("subset must be the same length as x")
    if(NCOL(subset) != 1)
      stop("subset must be a vector of rows to keep")
    if("mts" %in% class(x)){
      return(subset.matrix(x,subset))
    }
    else{
      return(subset.default(x,subset))
    }
  }
  else if(!is.null(start) | !is.null(end))
  {
    if(is.null(start))
      start <- 1
    if(is.null(end))
      end <- NROW(x)
    if("mts" %in% class(x))
      xsub <- x[start:end,]
    else
      xsub <- x[start:end]
    tspx <- tsp(x)
    return(ts(xsub, frequency=tspx[3], start=tspx[1L] + (start-1)/tspx[3L]))
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
    if(min(season) < 1L | max(season) > 4L)
      stop("Quarters must be between 1 and 4")
  }
  else if(is.null(season))
    stop("No subset specified")
  else
    if(min(season) < 1L | max(season) > frequency(x))
      stop(paste("Seasons must be between 1 and", frequency(x)))

  start <- utils::head(time(x)[is.element(cycle(x), season)],1)
  if("mts" %in% class(x)){
    x <- subset.matrix(x, is.element(cycle(x), season))
  }
  else{
    x <- subset.default(x, is.element(cycle(x), season))
  }
    return(ts(x, frequency=length(season), start=start))
}

head.ts <- function(x, n=6L, ...)
{
  tspx <- tsp(x)
  if(NCOL(x) > 1)
    hx <- ts(utils::head.matrix(as.matrix(x), n=n, ...),
      start=tspx[1], frequency=tspx[3])
  else if((length(x) + n) > 0)
    hx <- ts(head(c(x), n=n, ...),
             start=tspx[1], frequency=tspx[3])
  else
    hx <- numeric(0)
  return(hx)
}


tail.ts <- function(x, n=6L, ...)
{
  tspx <- tsp(x)
  if(NCOL(x) > 1)
    hx <- ts(utils::tail.matrix(as.matrix(x), n=n, ...),
             end=tspx[2], frequency=tspx[3])
  else if((length(x) + n) > 0)
    hx <- ts(tail(c(x), n=n, ...),
             end=tspx[2], frequency=tspx[3])
  else
    hx <- numeric(0)
  return(hx)
}

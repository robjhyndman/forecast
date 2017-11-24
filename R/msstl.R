
#' Multiple seasonal  decomposition
#'
#' Decompose a multiple seasonal time series into seasonal, trend and remainder
#' components. Seasonal components are estimated iteratively using STL. The trend
#' component is computed for the last iteration of STL. Non-seasonal time series
#' are decomposed into trend and remainder only. In this case, \code{\link[stats]{supsmu}} 
#' is used to estimate the trend.
#' Optionally, the time series may be Box-Cox transformed before decomposition.
#' @param x Univariate time series of class \code{msts} or \code{ts}.
#' @param lambda Box-Cox decomposition parameter. If \code{NULL}, no transformation
#' is used. If \code{lambda="auto"}, a transformation is automatically selected. If
#' lambda takes a numerical value, it is used as the parameter of the Box-Cox transformation.
#' @param iterate Number of iterations to use to refine the seasonal component.
#' @param s.window Seasonal windows to be used in the  decompositions. If scalar,
#' the same value is used for all seasonal components. Otherwise, it should be a vector
#' of the same length as the number of seasonal components.
#' @param ... Other arguments are passed to \code{\link[stats]{stl}}.
#' @seealso \code{\link[stats]{stl}}, \code{link[stats]{supsmu}}
#' @examples
#' library(ggplot2)
#' msstl(taylor) %>% autoplot(facet=TRUE)
#' msstl(AirPassengers, lambda='auto') %>% autoplot(facet=TRUE)
#' @export
msstl <- function(x, lambda=NULL, iterate=2, s.window=21, ...)
{
  # What is x?
  n <- length(x)
  if("msts" %in% class(x))
  {
    msts <- attributes(x)$msts
    if(any(msts >= n/2))
    {
      warning("Dropping seasonal components with fewer than two full periods.")
      msts <- msts[msts < n/2]
      x <- forecast::msts(x, seasonal.periods = msts)
    }
    msts <- sort(msts, decreasing = FALSE)
  }
  else if("ts" %in% class(x))
  {
    msts <- frequency(x)
    iterate <- 1L
  }
  else
    msts <- 1L

  # Transform if necessary
  if(!is.null(lambda))
  {
    if(lambda=="auto")
      lambda <- forecast::BoxCox.lambda(x, ...)
    x <- forecast::BoxCox(x, lambda=lambda)
  }
  tt <- seq_len(n)

  # Replace missing values if necessary
  origx <- x
  if(anyNA(x))
    x <- na.interp(x, lambda=lambda)
  
  
  # Now fit stl models with only one type of seasonality at a time
  if(msts[1L] > 1)
  {
    stlfits <- list()
    seas <- as.list(rep(0,length(msts)))
    deseas <- x
    if(length(s.window)==1L)
      s.window <- rep(s.window, length(msts))
    iterate <- pmax(1L, iterate)
    for(j in seq_len(iterate))
    {
      for(i in seq_along(msts))
      {
        deseas <- deseas + seas[[i]]
        fit <- stl(ts(deseas, frequency=msts[i]), s.window=s.window[i], ...)
        seas[[i]] <- msts(fit$time.series[,"seasonal"], seasonal.periods=msts)
        attributes(seas[[i]]) <- attributes(x)
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- msts(fit$time.series[,'trend'], seasonal.periods=msts)
  }
  else
  {
    msts <- NULL
    deseas <- x
    trend <- ts(stats::supsmu(seq_len(n), x)$y)
  }
  attributes(trend) <- attributes(x)

  # Estimate remainder
  remainder <- deseas - trend

    # Package into matrix
  output <- cbind(origx, trend)
  if(!is.null(msts))
  {
    for(i in seq_along(msts))
      output <- cbind(output, seas[[i]])
  }
  output <- cbind(output, remainder)
  colnames(output)[1L:2L] <- c("Data","Trend")
  if(!is.null(msts))
    colnames(output)[2L+seq_along(msts)] <- paste0("Seasonal",round(msts,2))
  colnames(output)[NCOL(output)] <- "Remainder"

  return(structure(output, class=c("msstl","mts")))
}

#' @rdname autoplot.seas
#' @export
autoplot.msstl <- function(object, ...)
{
  autoplot.mts(object,facets=TRUE,...)
}

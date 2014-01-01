bizdays <- function(x, FinCenter) {
  # Return the number of trading days corresponding to the input ts
  #
  # Args:
  #   x: a ts object
  #   FinCenter: inherents "FinCenter" from "timeDate" package
  #
  # Returns:
  #   A matrix contains the number of trading days
  if (is.null(tsp(x))) {
    stop("We cannot handle a time series without time attributes.")
  }
  if (missing(FinCenter)) {
    FinCenter <- "GMT"
  }
  # Convert tsp to date
  freq <- frequency(x)
  if (freq == 12L) {  # monthly data
    date <- zoo::as.Date(time(x))
    start <- date[1L]
    end <- seq(date[length(date)], length = 2L, by = "month")[2L] - 1L
    days.len <- as.timeDate(seq(start, end, by = "days"), FinCenter = FinCenter)
    # Grab business days
    biz <- days.len[isBizday(days.len, 
                             holidays = unique(format(days.len, "%Y")))]
    bizdays <- format(biz, format = "%Y %b")
  } else if (freq == 4L) {  # Quarterly data
    date <- zoo::as.Date(time(x))
    start <- date[1L]
    end <- seq(date[length(date)], length = 2L, by = "3 month")[2L] - 1L
    days.len <- as.timeDate(seq(start, end, by = "days"), FinCenter = FinCenter)
    biz <- days.len[isBizday(days.len, 
                             holidays = unique(format(days.len, "%Y")))]
    bizdays <- format(as.yearqtr(biz), format = "%Y Qtr%q")
    
  } # else if (freq == 52L) {  # Weekly data
  #   start <- paste0(start(x)[1L], "-01-01")
  #   start <- as.Date(start) + start(x)[2L] * 7L
  #   end <- start + length(time(x)) * 7L
  #   days.len <- as.timeDate(seq(start, end, by = "days"), FinCenter = FinCenter)
  #   biz <- days.len[isBizday(days.len, 
  #                            holidays = unique(format(days.len, "%Y")))]
  #   bizdays <- format(biz, format = "%Y Wk%W")
  # }
  num.days <- table(bizdays)
  out <- ts(num.days, start = tsp(x)[1L], frequency = freq)
  return(out)
}

easter <- function(x, easter.mon = FALSE) {
  # Return a vector of 0's and 1's for easter holidays
  #
  # Args:
  #   x: monthly, quarterly or weekly data
  #   easter.mon: An option including easter.mon
  #
  # ToDo:
  #   Allow fractional results, if Easter spans Mar & Apr
  if (is.null(tsp(x))) {
    stop("We cannot handle a time series without time attributes.")
  }
  freq <- frequency(x)
  start.yr <- start(x)[1L]
  end.yr <- end(x)[1L]
  yr.span <- seq(start.yr, end.yr)
  if (freq == 12L) {
    fmat <- "%Y-%m"
    yr.mon <- format(zoo::as.Date(time(x)), format = fmat)
    gd.fri <- format(Easter(yr.span, shift = -2L), format = fmat)  # good fri
    if (easter.mon) {
      easter <- format(Easter(yr.span, shift = 1L), format = fmat) # easter mon
    } else {
      easter <- format(Easter(yr.span), format = fmat)
    }
  } else if (freq == 4L) {
    fmat <- "%Y-%q"
    yr.mon <- format(as.yearqtr(zoo::as.Date(time(x))), format = fmat) # yr.qtr
    gd.fri <- format(as.yearqtr(Easter(yr.span, shift = -2L)), foramt = fmat)
    if (easter.mon) {
      easter <- format(as.yearqtr(Easter(yr.span, shift = 1L)), format = fmat)
    } else {
      easter <- format(as.yearqtr(Easter(yr.span)), format = fmat)
    }
  }
  span <- cbind(gd.fri, easter)  # the span of easter holidays
  hdays <- unlist(apply(span, 1, unique))
  dummies <- ifelse(yr.mon %in% hdays, 1L, 0L)
  out <- ts(dummies, start = tsp(x)[1L], frequency = freq)
  return(out)
}

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
    
  } else if (freq == 52L) {  # Weekly data
    start <- paste0(start(x)[1L], "-01-01")
    start <- as.Date(start) + start(x)[2L] * 7L
    end <- start + length(time(x)) * 7L
    days.len <- as.timeDate(seq(start, end, by = "days"), FinCenter = FinCenter)
    biz <- days.len[isBizday(days.len, 
                             holidays = unique(format(days.len, "%Y")))]
    bizdays <- format(biz, format = "%Y Wk%W")
  }
  num.days <- table(bizdays)
  out <- ts(num.days, start = tsp(x)[1L], frequency = freq)
  return(out)
}

bizdays <- function(x, FinCenter) {
  # Return the number of trading days corresponding to the input ts
  #
  # Args:
  #   x: a ts object
  #   FinCenter: inherits "FinCenter" from "timeDate" package
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
  if (is.null(tsp(x))) {
    stop("We cannot handle a time series without time attributes.")
  }
  freq <- frequency(x)
  date <- zoo::as.Date(time(x))
  start.yr <- start(x)[1L]
  end.yr <- end(x)[1L]
  yr.span <- seq(start.yr, end.yr)
  gd.fri0 <- Easter(yr.span, -2L)
  if (easter.mon) {
    easter0 <- Easter(yr.span, 1L)
  } else {
    easter0 <- Easter(yr.span)
  }
  if (freq == 12L) {
    fmat <- "%Y-%m"
    yr.mon <- format(date, format = fmat)
    gd.fri <- format(gd.fri0, format = fmat)  # good fri
    easter <- format(easter0, format = fmat) # easter mon
  } else if (freq == 4L) {
    fmat <- "%Y-%q"
    yr.mon <- format(as.yearqtr(date), format = fmat) # yr.qtr
    gd.fri <- format(as.yearqtr(gd.fri0), format = fmat)
    easter <- format(as.yearqtr(easter0), format = fmat)
  }
  span <- cbind(gd.fri, easter)  # the span of easter holidays
  hdays <- unlist(apply(span, 1, unique))
  dummies <- ifelse(yr.mon %in% hdays, 1L, 0L)
  # Allow fractional results
  denominator <- (easter0 - gd.fri0 + 1L)[1L]
  last.mar <- as.timeDate(paste0(yr.span, "-03-31"))
  dif <- difftimeDate(last.mar, gd.fri0, units = "days") + 1L
  # Remove easter out of date range
  if (date[1L] > as.character(last.mar[1L])) {
    dif <- dif[-1L]
  }
  if (date[length(yr.mon)] < as.character(last.mar[length(last.mar)])) {
    dif <- dif[-length(dif)]
  }
  replace <- dif > denominator | dif <= 0L
  dif[replace] <- denominator  # Easter in the same month
  # Start to insert the remaining part falling in Apr
  index <- which(dif != denominator)
  if (length(index) != 0L) {
    values <- denominator - dif[index]
    new.index <- index[1L]
    for (i in 1L:length(index)) {
      dif <- append(dif, values = values[i], new.index)
      new.index <- index[i + 1L] + i
    }
    dummies[dummies == 1L] <- round(dif/unclass(denominator), digits = 2)
  }
  out <- ts(dummies, start = tsp(x)[1L], frequency = freq)
  return(out)
}

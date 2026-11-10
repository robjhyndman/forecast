## Add as.Date.timeDate to S3 method table
#' @export
as.Date.timeDate <- timeDate::as.Date.timeDate


#' Number of trading days in each season
#'
#' Returns number of trading days in each month or quarter of the observed time
#' period in a major financial center.
#'
#' Useful for trading days length adjustments. For more on how to define
#' business days, see [timeDate::isBizday()].
#'
#' @param x Monthly or quarterly time series.
#' @param FinCenter Major financial center.
#' @return Time series
#' @author Earo Wang
#' @seealso [monthdays()]
#' @keywords ts
#' @examples
#'
#' x <- ts(rnorm(30), start = c(2013, 2), frequency = 12)
#' bizdays(x, FinCenter = "New York")
#' @export
bizdays <- function(
  x,
  FinCenter = c(
    "New York",
    "London",
    "NERC",
    "Toronto",
    "Zurich"
  )
) {
  # Return the number of trading days corresponding to the input ts
  #
  # Args:
  #   x: a ts object
  #   FinCenter: inherits holiday calendar from "timeDate" package
  #
  # Returns:
  #   A matrix contains the number of trading days
  if (is.null(tsp(x))) {
    stop("We cannot handle a time series without time attributes.")
  }
  # Convert tsp to date
  freq <- frequency(x)
  years <- start(x)[1L]:end(x)[1L]
  # Grab the holidays from years and financial center
  FinCenter <- match.arg(FinCenter)
  holidays <- switch(
    FinCenter,
    "New York" = timeDate::holidayNYSE(years),
    "London" = timeDate::holidayLONDON(years),
    "NERC" = timeDate::holidayNERC(years),
    "Toronto" = timeDate::holidayTSX(years),
    "Zurich" = timeDate::holidayZURICH(years)
  )
  if (freq == 12L) {
    # monthly data
    date <- zoo::as.Date(time(x))
    start <- date[1L]
    end <- seq(date[length(date)], length.out = 2L, by = "month")[2L] - 1L
    days.len <- timeDate::timeSequence(from = start, to = end)
    # Grab business days
    biz <- days.len[timeDate::isBizday(days.len, holidays = holidays)]
    bizdays <- format(biz, format = "%Y-%m")
  } else if (freq == 4L) {
    # Quarterly data
    date <- zoo::as.Date(time(x))
    start <- date[1L]
    end <- seq(date[length(date)], length.out = 2L, by = "3 month")[2L] - 1L
    days.len <- timeDate::timeSequence(from = start, to = end)
    # Grab business days
    biz <- days.len[timeDate::isBizday(days.len, holidays = holidays)]
    bizdays <- format(zoo::as.yearqtr(biz), format = "%Y Qtr%q")
  }
  num.days <- table(bizdays)
  out <- ts(num.days, start = tsp(x)[1L], frequency = freq)
  out
}


#' Easter holidays in each season
#'
#' Returns a vector of 0's and 1's or fractional results if Easter spans March
#' and April in the observed time period. Easter is defined as the days from
#' Good Friday to Easter Sunday inclusively, plus optionally Easter Monday if
#' `easter.mon = TRUE`.
#'
#' Useful for adjusting calendar effects.
#'
#' @param x Monthly or quarterly time series.
#' @param easter.mon If `TRUE`, the length of Easter holidays includes
#' Easter Monday.
#' @return Time series
#' @author Earo Wang
#' @keywords ts
#' @examples
#'
#' easter(wineind, easter.mon = TRUE)
#' @export
easter <- function(x, easter.mon = FALSE) {
  freq <- frequency(x)
  if (freq == 12) {
    period <- function(date) format(date, "%Y-%m")
  } else if (freq == 4) {
    period <- function(date) format(zoo::as.yearqtr(date), "%Y-%q")
  } else {
    stop("`x` must be a monthly or quarterly time series.")
  }
  # Every day of the Easter holidays in the years covered by x
  n_days <- if (easter.mon) 4L else 3L
  good_friday <- as.Date(Easter(seq(start(x)[1L], end(x)[1L]), -2L))
  holidays <- rep(good_friday, each = n_days) + seq_len(n_days) - 1L
  # Fraction of each year's holidays falling in each period of x
  periods <- period(zoo::as.Date(time(x)))
  n_holidays <- tabulate(match(period(holidays), periods), length(periods))
  ts(round(n_holidays / n_days, 2), start = tsp(x)[1L], frequency = freq)
}

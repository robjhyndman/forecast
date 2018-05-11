
forecast2plotdf <- function(model, data=as.data.frame(model), PI=TRUE, showgap=TRUE, ...) {
  # Time series forecasts
  if (is.element("ts", class(model$mean))) {
    xVals <- as.numeric(time(model$mean)) # x axis is time
  }
  # Cross-sectional forecasts
  else if (!is.null(model[["newdata"]])) {
    xVals <- as.numeric(model[["newdata"]][, 1]) # Only display the first column of newdata, should be generalised.
    if (NCOL(model[["newdata"]]) > 1) {
      message("Note: only extracting first column of data")
    }
  }
  else {
    stop("Could not find forecast x axis")
  }
  Hiloc <- grep("Hi ", names(data))
  Loloc <- grep("Lo ", names(data))
  if (PI && !is.null(model$level)) { # PI
    if (length(Hiloc) == length(Loloc)) {
      if (length(Hiloc) > 0) {
        out <- data.frame(
          x = rep(xVals, length(Hiloc) + 1),
          y = c(rep(NA, NROW(data) * (length(Hiloc))), data[, 1]),
          level = c(as.numeric(rep(gsub("Hi ", "", names(data)[Hiloc]), each = NROW(data))), rep(NA, NROW(data))),
          ymax = c(unlist(data[, Hiloc]), rep(NA, NROW(data))), ymin = c(unlist(data[, Loloc]), rep(NA, NROW(data)))
        )
        numInterval <- length(model$level)
      }
    }
    else {
      warning("missing intervals detected, plotting point predictions only")
      PI <- FALSE
    }
  }
  if (!PI) { # No PI
    out <- data.frame(x = xVals, y = as.numeric(model$mean), level = rep(NA, NROW(model$mean)), ymax = rep(NA, NROW(model$mean)), ymin = rep(NA, NROW(model$mean)))
    numInterval <- 0
  }
  if (!showgap) {
    if (is.null(model$x)) {
      warning("Removing the gap requires historical data, provide this via model$x. Defaulting showgap to TRUE.")
    }
    else {
      intervalGap <- data.frame(
        x = rep(time(model$x)[length(model$x)], numInterval + 1),
        y = c(model$x[length(model$x)], rep(NA, numInterval)),
        level = c(NA, model$level)[seq_along(1:(numInterval + 1))],
        ymax = c(NA, rep(model$x[length(model$x)], numInterval)),
        ymin = c(NA, rep(model$x[length(model$x)], numInterval))
      )
      out <- rbind(intervalGap, out)
    }
  }
  return(out)
}


#' @rdname geom_forecast
#' @export
StatForecast <- ggplot2::ggproto(
  "StatForecast", ggplot2::Stat,
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, params, PI=TRUE, showgap=TRUE, series=NULL,
                           model=ets(y), fc.args = list(), ...) {
    tspx <- recoverTSP(data$x)
    y <- ts(data = data$y, start = tspx[1], frequency = tspx[3])

    fit <- eval(substitute(model))
    fcast <- do.call("forecast", append(list(fit), fc.args))
    fcast <- forecast2plotdf(fcast, PI = PI, showgap = showgap)
    if (!is.null(series)) {
      if (data$group[1] > length(series)) {
        message("Recycling series argument, please provide a series name for each time series")
      }
      fcast <- transform(fcast, series = series[(abs(data$group[1]) - 1) %% length(series) + 1])
    }
    fcast
  }
)

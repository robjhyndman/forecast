#' @rdname is.forecast
#' @export
is.mforecast <- function(x) {
  inherits(x, "mforecast")
}

mlmsplit <- function(x, index = NULL) {
  if (is.null(index)) {
    stop("Must select lm using index=integer(1)")
  }
  mfit <- match(
    c("coefficients", "residuals", "effects", "fitted.values"),
    names(x),
    0L
  )
  for (j in mfit) {
    x[[j]] <- x[[j]][, index]
  }
  class(x) <- "lm"
  y <- attr(x$terms, "response")

  yName <- make.names(colnames(x$model[[y]])[index])
  x$model[[y]] <- x$model[[y]][, index]
  colnames(x$model)[y] <- yName
  attr(x$model, "terms") <- terms(
    reformulate(attr(x$terms, "term.labels"), response = yName),
    data = x$model
  )

  if (!is.null(tsp(x$data[, 1]))) {
    tspx <- tsp(x$data[, 1]) # Consolidate ts attributes for forecast.lm
    x$data <- lapply(x$model, function(x) {
      ts(x, start = tspx[1], end = tspx[2], frequency = tspx[3])
    })
    class(x$data) <- "data.frame"
    row.names(x$data) <- 1:max(vapply(x$data, NROW, integer(1)))
  }

  x$terms <- terms(x$model)
  x
}


#' Forecast a multiple linear model with possible time series components
#'
#' `forecast.mlm` is used to predict multiple linear models, especially
#' those involving trend and seasonality components.
#'
#' `forecast.mlm` is largely a wrapper for [forecast.lm()] except that it
#' allows forecasts to be generated on multiple series. Also, the output is
#' reformatted into a `mforecast` object.
#'
#' @inheritParams forecast.lm
#' @param object Object of class "mlm", usually the result of a call to
#' [stats::lm()] or [tslm()].
#' @param ... Other arguments passed to [forecast.lm()].
#' @return An object of class `mforecast`.
#'
#' The function `summary` is used to obtain and print a summary of the
#' results, while the function `plot` produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions `fitted.values` and `residuals`
#' extract useful features of the value returned by `forecast.lm`.
#'
#' An object of class `mforecast` is a list containing at least the
#' following elements:
#' \item{model}{A list containing information about the fitted model}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a multivariate time series}
#' \item{lower}{Lower limits for prediction intervals of each series}
#' \item{upper}{Upper limits for prediction intervals of each series}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{x}{The historical data for the response variable.}
#' \item{residuals}{Residuals from the fitted model. That is x minus fitted values.}
#' \item{fitted}{Fitted values}
#' @author Mitchell O'Hara-Wild
#' @seealso [tslm()], [forecast.lm()], [stats::lm()].
#' @examples
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' fit <- tslm(lungDeaths ~ trend + season)
#' fcast <- forecast(fit, h = 10)
#'
#' carPower <- as.matrix(mtcars[, c("qsec", "hp")])
#' carmpg <- mtcars[, "mpg"]
#' fit <- lm(carPower ~ carmpg)
#' fcast <- forecast(fit, newdata = data.frame(carmpg = 30))
#'
#' @export
forecast.mlm <- function(
  object,
  newdata,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(object$lambda, "biasadj"),
  ts = TRUE,
  ...
) {
  out <- list(
    model = object,
    forecast = vector("list", NCOL(object$coefficients))
  )

  cl <- match.call()
  cl[[1]] <- quote(forecast.lm)
  cl$object <- quote(mlmsplit(object, index = i))
  for (i in seq_along(out$forecast)) {
    out$forecast[[i]] <- eval(cl)
    out$forecast[[i]]$series <- colnames(object$coefficients)[i]
  }
  out$method <- rep("Multiple linear regression model", length(out$forecast))
  names(out$forecast) <- names(out$method) <- colnames(object$coefficients)
  structure(out, class = "mforecast")
}

#' Forecasting time series
#'
#' `mforecast` is a class of objects for forecasting from multivariate
#' time series or multivariate time series models. The function invokes
#' particular \emph{methods} which depend on the class of the first argument.
#'
#' For example, the function [forecast.mlm()] makes multivariate
#' forecasts based on the results produced by [tslm()].
#'
#' @aliases mforecast print.mforecast summary.mforecast as.data.frame.mforecast
#'
#' @inheritParams forecast.ts
#' @param object a multivariate time series or multivariate time series model
#' for which forecasts are required
#' @param robust If `TRUE`, the function is robust to missing values and outliers
#' in `object`. This argument is only valid when `object` is of class `mts`.
#' @param find.frequency If `TRUE`, the function determines the appropriate
#' period, if the data is of unknown period.
#' @param allow.multiplicative.trend If `TRUE`, then ETS models with
#' multiplicative trends are allowed. Otherwise, only additive or no trend ETS
#' models are permitted.
#' @param ... Additional arguments affecting the forecasts produced.
#' @return An object of class `mforecast`.
#'
#' The function `summary` is used to obtain and print a summary of the
#' results, while the function `plot` produces a plot of the multivariate
#' forecasts and prediction intervals.
#'
#' The generic accessors functions `fitted.values` and `residuals`
#' extract various useful features of the value returned by `forecast$model`.
#'
#' An object of class `mforecast` is a list usually containing at least
#' the following elements:
#' \item{model}{A list containing information about the fitted model}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals}
#' \item{upper}{Upper limits for prediction intervals}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{x}{The original time series (either `object` itself or the time series
#'          used to create the model stored as `object`).}
#' \item{residuals}{Residuals from the fitted model. For models with additive
#'       errors, the residuals will be x minus the fitted values.}
#' \item{fitted}{Fitted values (one-step forecasts)}
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso Other functions which return objects of class `mforecast`
#' are [forecast.mlm()], `forecast.varest()`.
#'
#' @export
forecast.mts <- function(
  object,
  h = if (frequency(object) > 1) 2 * frequency(object) else 10,
  level = c(80, 95),
  fan = FALSE,
  robust = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  find.frequency = FALSE,
  allow.multiplicative.trend = FALSE,
  ...
) {
  out <- list(forecast = vector("list", NCOL(object)))
  cl <- match.call()
  cl[[1]] <- quote(forecast.ts)
  cl$object <- quote(object[, i])
  for (i in seq_len(NCOL(object))) {
    out$forecast[[i]] <- eval(cl)
    out$forecast[[i]]$series <- colnames(object)[i]
  }
  out$method <- vapply(out$forecast, function(x) x$method, character(1))
  names(out$forecast) <- names(out$method) <- colnames(object)
  structure(out, class = "mforecast")
}

#' @export
print.mforecast <- function(x, ...) {
  lapply(x$forecast, function(x) {
    cat(paste0(x$series, "\n"))
    print(x)
    cat("\n")
  })
  invisible()
}


#' Multivariate forecast plot
#'
#' Plots historical data with multivariate forecasts and prediction intervals.
#'
#' `autoplot` will produce an equivalent plot as a ggplot object.
#'
#' @param x Multivariate forecast object of class `mforecast`.
#' @param object Multivariate forecast object of class `mforecast`. Used
#' for ggplot graphics (S3 method consistency).
#' @param main Main title. Default is the forecast method. For autoplot,
#' specify a vector of titles for each plot.
#' @param xlab X-axis label. For autoplot, specify a vector of labels for each
#' plot.
#' @param PI If `FALSE`, confidence intervals will not be plotted, giving
#' only the forecast line.
#' @param facets If `TRUE`, multiple time series will be faceted. If
#' `FALSE`, each series will be assigned a colour.
#' @param colour If `TRUE`, the time series will be assigned a colour aesthetic
#' @param series Matches an unidentified forecast layer with a coloured object
#' on the plot.
#' @param ... additional arguments to each individual `plot`.
#' @author Mitchell O'Hara-Wild
#' @seealso [plot.forecast()], [stats::plot.ts()]
#' @references Hyndman and Athanasopoulos (2018) \emph{Forecasting: principles
#' and practice}, 2nd edition, OTexts: Melbourne, Australia.
#' \url{https://otexts.com/fpp2/}
#' @keywords ts
#' @examples
#' library(ggplot2)
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' fit <- tslm(lungDeaths ~ trend + season)
#' fcast <- forecast(fit, h = 10)
#' plot(fcast)
#' autoplot(fcast)
#'
#' carPower <- as.matrix(mtcars[, c("qsec", "hp")])
#' carmpg <- mtcars[, "mpg"]
#' fit <- lm(carPower ~ carmpg)
#' fcast <- forecast(fit, newdata = data.frame(carmpg = 30))
#' plot(fcast, xlab = "Year")
#' autoplot(fcast, xlab = rep("Year", 2))
#'
#' @export
plot.mforecast <- function(
  x,
  main = paste("Forecasts from", unique(x$method)),
  xlab = "time",
  ...
) {
  oldpar <- par(
    mfrow = c(length(x$forecast), 1),
    mar = c(0, 5.1, 0, 2.1),
    oma = c(6, 0, 5, 0)
  )
  on.exit(par(oldpar))
  for (fcast in x$forecast) {
    plot(fcast, main = "", xaxt = "n", ylab = fcast$series, ...)
  }
  axis(1)
  mtext(xlab, outer = TRUE, side = 1, line = 3)
  title(main = main, outer = TRUE)
}

#' @export
summary.mforecast <- function(object, ...) {
  class(object) <- c("summary.mforecast", class(object))
  object
}

#' @export
print.summary.mforecast <- function(x, ...) {
  cat(paste("\nForecast method:", unique(x$method)))
  cat(paste("\n\nModel Information:\n"))
  print(x$model)
  cat("\nError measures:\n")
  print(accuracy(x))
  if (is.null(x$forecast)) {
    cat("\n No forecasts\n")
  } else {
    cat("\nForecasts:\n")
    NextMethod()
  }
}

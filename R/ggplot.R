#' Create a ggplot layer appropriate to a particular data type
#'
#' \code{autolayer} uses ggplot2 to draw a particular layer for an object of a
#' particular class in a single command. This defines the S3 generic that other
#' classes and packages can extend.
#'
#'
#' @param object an object, whose class will determine the behaviour of
#' autoplot
#' @param ... other arguments passed to specific methods
#' @return a ggplot layer
#' @seealso \code{\link[ggplot2]{autoplot}}, \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggplot2]{fortify}}
#'
#' @export
autolayer <- function(object, ...) {
  UseMethod("autolayer")
}

ggAddExtras <- function(xlab=NA, ylab=NA, main=NA) {
  dots <- eval.parent(quote(list(...)))
  extras <- list()
  if ("xlab" %in% names(dots) || is.null(xlab) || !is.na(xlab)) {
    if ("xlab" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::xlab(dots$xlab)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::xlab(xlab)
    }
  }
  if ("ylab" %in% names(dots) || is.null(ylab) || !is.na(ylab)) {
    if ("ylab" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::ylab(dots$ylab)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::ylab(ylab)
    }
  }
  if ("main" %in% names(dots) || is.null(main) || !is.na(main)) {
    if ("main" %in% names(dots)) {
      extras[[length(extras) + 1]] <- ggplot2::ggtitle(dots$main)
    }
    else {
      extras[[length(extras) + 1]] <- ggplot2::ggtitle(main)
    }
  }
  if ("xlim" %in% names(dots)) {
    extras[[length(extras) + 1]] <- ggplot2::xlim(dots$xlim)
  }
  if ("ylim" %in% names(dots)) {
    extras[[length(extras) + 1]] <- ggplot2::ylim(dots$ylim)
  }
  return(extras)
}

ggtsbreaks <- function(x) {
  # Make x axis contain only whole numbers (e.g., years)
  return(unique(round(pretty(floor(x[1]):ceiling(x[2])))))
}



#' ggplot (Partial) Autocorrelation and Cross-Correlation Function Estimation
#' and Plotting
#'
#' Produces a ggplot object of their equivalent Acf, Pacf, Ccf, taperedacf and
#' taperedpacf functions.
#'
#' If \code{autoplot} is given an \code{acf} or \code{mpacf} object, then an
#' appropriate ggplot object will be created.
#'
#' ggtaperedpacf
#' @param object Object of class \dQuote{\code{acf}}.
#' @param x a univariate or multivariate (not Ccf) numeric time series object
#' or a numeric vector or matrix.
#' @param y a univariate numeric time series object or a numeric vector.
#' @param ci coverage probability for confidence interval. Plotting of the
#' confidence interval is suppressed if ci is zero or negative.
#' @param lag.max maximum lag at which to calculate the acf.
#' @param type character string giving the type of acf to be computed. Allowed
#' values are "\code{correlation}" (the default), \dQuote{\code{covariance}} or
#' \dQuote{\code{partial}}.
#' @param plot logical. If \code{TRUE} (the default) the resulting ACF, PACF or
#' CCF is plotted.
#' @param na.action function to handle missing values. Default is
#' \code{\link[stats]{na.contiguous}}.  Useful alternatives are
#' \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.
#' @param demean Should covariances be about the sample means?
#' @param calc.ci If \code{TRUE}, confidence intervals for the ACF/PACF
#' estimates are calculated.
#' @param level Percentage level used for the confidence intervals.
#' @param nsim The number of bootstrap samples used in estimating the
#' confidence intervals.
#' @param ... Other plotting parameters to affect the plot.
#' @return A ggplot object.
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link[stats]{plot.acf}}, \code{\link{Acf}},
#' \code{\link[stats]{acf}}, \code{\link{taperedacf}}
#' @examples
#'
#' library(ggplot2)
#' ggAcf(wineind)
#' wineind %>% Acf(plot=FALSE) %>% autoplot
#' \dontrun{
#' wineind %>% taperedacf(plot=FALSE) %>% autoplot
#' ggtaperedacf(wineind)
#' ggtaperedpacf(wineind)}
#' ggCcf(mdeaths, fdeaths)
#'
#' @export
autoplot.acf <- function(object, ci=0.95, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "acf")) {
      stop("autoplot.acf requires a acf object, use object=object")
    }

    data <- data.frame(Lag = object$lag, ACF = object$acf)
    if (data$Lag[1] == 0 && object$type == "correlation") {
      data <- data[-1, ]
    }

    # Initialise ggplot object
    p <- ggplot2::ggplot(
      ggplot2::aes_(x = ~Lag, xend = ~Lag, y = 0, yend = ~ACF),
      data = data
    )
    p <- p + ggplot2::geom_hline(yintercept = 0)

    # Add data
    p <- p + ggplot2::geom_segment(lineend = "butt")

    # Add ci lines (assuming white noise input)
    ci <- qnorm((1 + ci) / 2) / sqrt(object$n.used)
    p <- p + ggplot2::geom_hline(yintercept = c(-ci, ci), colour = "blue", linetype = "dashed")

    # Prepare graph labels
    if (!is.null(object$ccf)) {
      ylab <- "CCF"
      ticktype <- "ccf"
      main <- paste("Series:", object$snames)
      nlags <- round(length(data$Lag) / 2)
    }
    else if (object$type == "partial") {
      ylab <- "PACF"
      ticktype <- "acf"
      main <- paste("Series:", object$series)
      nlags <- length(data$Lag)
    }
    else if (object$type == "correlation") {
      ylab <- "ACF"
      ticktype <- "acf"
      main <- paste("Series:", object$series)
      nlags <- length(data$Lag)
    }
    else {
      ylab <- NULL
    }

    # Add seasonal x-axis
    # Change ticks to be seasonal and prepare default title
    if (!is.null(object$tsp)) {
      freq <- object$tsp[3]
    } else {
      freq <- 1
    }
    if (!is.null(object$periods)) {
      periods <- object$periods
      periods <- periods[periods != freq]
      minorbreaks <- periods * seq(-20:20)
    }
    else {
      minorbreaks <- NULL
    }
    p <- p + ggplot2::scale_x_continuous(breaks = seasonalaxis(
      freq,
      nlags, type = ticktype, plot = FALSE
    ), minor_breaks = minorbreaks)
    p <- p + ggAddExtras(ylab = ylab, xlab = "Lag", main = main)
    return(p)
  }
}

#' @rdname autoplot.acf
#' @export
ggAcf <- function(x, lag.max = NULL,
                  type = c("correlation", "covariance", "partial"),
                  plot = TRUE, na.action = na.contiguous, demean=TRUE, ...) {
  cl <- match.call()
  if (plot) {
    cl$plot <- FALSE
  }
  cl[[1]] <- quote(Acf)
  object <- eval.parent(cl)
  object$tsp <- tsp(x)
  object$periods <- attributes(x)$msts
  if (plot) {
    return(autoplot(object, ...))
  }
  else {
    return(object)
  }
}

#' @rdname autoplot.acf
#' @export
ggPacf <- function(x, lag.max = NULL,
                   plot = TRUE, na.action = na.contiguous, demean=TRUE, ...) {
  object <- Acf(x, lag.max = lag.max, type = "partial", na.action = na.action, demean = demean, plot = FALSE)
  object$series <- deparse(substitute(x))
  if (plot) {
    return(autoplot(object, ...))
  } else {
    return(object)
  }
}

#' @rdname autoplot.acf
#' @export
ggCcf <- function(x, y, lag.max=NULL, type=c("correlation", "covariance"),
                  plot=TRUE, na.action=na.contiguous, ...) {
  cl <- match.call()
  if (plot) {
    cl$plot <- FALSE
  }
  cl[[1]] <- quote(Ccf)
  object <- eval.parent(cl)
  object$snames <- paste(substitute(x), "&", substitute(y))
  object$ccf <- TRUE
  if (plot) {
    return(autoplot(object, ...))
  }
  else {
    return(object)
  }
}

#' @rdname autoplot.acf
#' @export
autoplot.mpacf <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "mpacf")) {
      stop("autoplot.mpacf requires a mpacf object, use object=object")
    }
    if (!is.null(object$lower)) {
      data <- data.frame(Lag = 1:object$lag, z = object$z, sig = (object$lower < 0 & object$upper > 0))
      cidata <- data.frame(Lag = rep(1:object$lag, each = 2) + c(-0.5, 0.5), z = rep(object$z, each = 2), upper = rep(object$upper, each = 2), lower = rep(object$lower, each = 2))
      plotpi <- TRUE
    }
    else {
      data <- data.frame(Lag = 1:object$lag, z = object$z)
      plotpi <- FALSE
    }
    # Initialise ggplot object
    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = 0), size = 0.2)

    # Add data
    if (plotpi) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes_(x = ~Lag, ymin = ~lower, ymax = ~upper), data = cidata, fill = "grey50")
    }
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~Lag, y = ~z), data = data)
    if (plotpi) {
      p <- p + ggplot2::geom_point(ggplot2::aes_(x = ~Lag, y = ~z, colour = ~sig), data = data)
    }

    # Change ticks to be seasonal
    freq <- frequency(object$x)
    msts <- is.element("msts", class(object$x))

    # Add seasonal x-axis
    if (msts) {
      periods <- attributes(object$x)$msts
      periods <- periods[periods != freq]
      minorbreaks <- periods * seq(-20:20)
    }
    else {
      minorbreaks <- NULL
    }

    p <- p + ggplot2::scale_x_continuous(
      breaks = seasonalaxis(frequency(object$x), length(data$Lag), type = "acf", plot = FALSE),
      minor_breaks = minorbreaks
    )

    if (object$type == "partial") {
      ylab <- "PACF"
    }
    else if (object$type == "correlation") {
      ylab <- "ACF"
    }

    p <- p + ggAddExtras(ylab = ylab)

    return(p)
  }
}

#' @rdname autoplot.acf
#' @export
ggtaperedacf <- function(x, lag.max=NULL, type=c("correlation", "partial"),
                         plot=TRUE, calc.ci=TRUE, level=95, nsim=100, ...) {
  cl <- match.call()
  if (plot) {
    cl$plot <- FALSE
  }
  cl[[1]] <- quote(taperedacf)
  object <- eval.parent(cl)
  if (plot) {
    return(autoplot(object, ...))
  }
  else {
    return(object)
  }
}

#' @rdname autoplot.acf
#' @export
ggtaperedpacf <- function(x, ...) {
  ggtaperedacf(x, type = "partial", ...)
}

#' @rdname plot.Arima
#' @export
autoplot.Arima <- function(object, type = c("both", "ar", "ma"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (is.Arima(object)) {
      # Detect type
      type <- match.arg(type)
      q <- p <- 0
      if (length(object$model$phi) > 0) {
        test <- abs(object$model$phi) > 1e-09
        if (any(test)) {
          p <- max(which(test))
        }
      }
      if (length(object$model$theta) > 0) {
        test <- abs(object$model$theta) > 1e-09
        if (any(test)) {
          q <- max(which(test))
        }
      }

      if (type == "both") {
        type <- c("ar", "ma")
      }
    }
    else if (inherits(object, "ar")) {
      type <- "ar"
      p <- length(arroots(object)$roots)
      q <- 0
    }
    else {
      stop("autoplot.Arima requires an Arima object")
    }

    # Remove NULL type
    type <- intersect(type, c("ar", "ma")[c(p > 0, q > 0)])

    # Prepare data
    arData <- maData <- NULL
    allRoots <- data.frame(roots = numeric(0), type = character(0))
    if ("ar" %in% type && p > 0) {
      arData <- arroots(object)
      allRoots <- rbind(allRoots, data.frame(roots = arData$roots, type = arData$type))
    }
    if ("ma" %in% type && q > 0) {
      maData <- maroots(object)
      allRoots <- rbind(allRoots, data.frame(roots = maData$roots, type = maData$type))
    }
    allRoots$Real <- Re(1 / allRoots$roots)
    allRoots$Imaginary <- Im(1 / allRoots$roots)
    allRoots$UnitCircle <- factor(ifelse((abs(allRoots$roots) > 1), "Within", "Outside"))

    # Initialise general ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~Real, y = ~Imaginary, colour = ~UnitCircle), data = allRoots)
    p <- p + ggplot2::coord_fixed(ratio = 1)
    p <- p + ggplot2::annotate(
      "path", x = cos(seq(0, 2 * pi, length.out = 100)),
      y = sin(seq(0, 2 * pi, length.out = 100))
    )
    p <- p + ggplot2::geom_vline(xintercept = 0)
    p <- p + ggplot2::geom_hline(yintercept = 0)
    p <- p + ggAddExtras(xlab = "Real", ylab = "Imaginary")

    if (NROW(allRoots) == 0) {
      return(p + ggAddExtras(main = "No AR or MA roots"))
    }

    p <- p + ggplot2::geom_point(size = 3)

    if (length(type) == 1) {
      p <- p + ggAddExtras(main = paste("Inverse", toupper(type), "roots"))
    }
    else {
      p <- p + ggplot2::facet_wrap(~ type, labeller = function(labels) lapply(labels, function(x) paste("Inverse", as.character(x), "roots")))
    }
  }
  return(p)
}

#' @rdname plot.Arima
#' @export
autoplot.ar <- function(object, ...) {
  autoplot.Arima(object, ...)
}

#' @rdname autoplot.seas
#' @export
autoplot.decomposed.ts <- function(object, labels=NULL, range.bars = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "decomposed.ts")) {
      stop("autoplot.decomposed.ts requires a decomposed.ts object")
    }

    if (is.null(labels)) {
      labels <- c("seasonal", "trend", "remainder")
    }

    cn <- c("data", labels)

    data <- data.frame(
      datetime = rep(time(object$x), 4),
      y = c(object$x, object$seasonal, object$trend, object$random),
      parts = factor(rep(cn, each = NROW(object$x)), levels = cn)
    )

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data)

    # Add data
    int <- as.numeric(object$type == "multiplicative")
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~y), data = subset(data, data$parts != cn[4]), na.rm = TRUE)
    p <- p + ggplot2::geom_segment(
      ggplot2::aes_(x = ~datetime, xend = ~datetime, y = int, yend = ~y),
      data = subset(data, data$parts == cn[4]), lineend = "butt", na.rm = TRUE
    )
    p <- p + ggplot2::facet_grid("parts ~ .", scales = "free_y", switch = "y")
    p <- p + ggplot2::geom_hline(ggplot2::aes_(yintercept = ~y), data = data.frame(y = int, parts = cn[4]))

    if (is.null(range.bars)) {
      range.bars <- object$type == "additive"
    }
    if (range.bars) {
      yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
      xranges <- range(data$datetime)
      barmid <- apply(yranges, 2, mean)
      barlength <- min(apply(yranges, 2, diff))
      barwidth <- (1 / 64) * diff(xranges)
      barpos <- data.frame(
        left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
        top = barmid + barlength / 2, bottom = barmid - barlength / 2,
        parts = colnames(yranges), datetime = xranges[2], y = barmid
      )
      p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
    }

    # Add axis labels
    p <- p + ggAddExtras(
      main = paste("Decomposition of", object$type, "time series"), xlab = "Time",
      ylab = ""
    )

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks = unique(round(pretty(data$datetime))))

    return(p)
  }
}

#' @rdname plot.ets
#' @export
autoplot.ets <- function(object, range.bars = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!is.ets(object)) {
      stop("autoplot.ets requires an ets object, use object=object")
    }

    names <- c(y = "observed", l = "level", b = "slope", s1 = "season")
    data <- cbind(object$x, object$states[, colnames(object$states) %in% names(names)])
    cn <- c("y", c(colnames(object$states)))
    colnames(data) <- cn <- names[stats::na.exclude(match(cn, names(names)))]

    # Convert to longform
    data <- data.frame(
      datetime = rep(time(data), NCOL(data)), y = c(data),
      parts = factor(rep(cn, each = NROW(data)), levels = cn)
    )

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data, ylab = "")

    # Add data
    p <- p + ggplot2::geom_line(na.rm = TRUE)
    p <- p + ggplot2::facet_grid(parts ~ ., scales = "free_y", switch = "y")
    if (is.null(range.bars)) {
      range.bars <- is.null(object$lambda)
    }
    if (range.bars) {
      yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
      xranges <- range(data$datetime)
      barmid <- apply(yranges, 2, mean)
      barlength <- min(apply(yranges, 2, diff))
      barwidth <- (1 / 64) * diff(xranges)
      barpos <- data.frame(
        left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
        top = barmid + barlength / 2, bottom = barmid - barlength / 2,
        parts = colnames(yranges), datetime = xranges[2], y = barmid
      )
      p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
    }

    p <- p + ggAddExtras(xlab = NULL, ylab = "", main = paste("Components of", object$method, "method"))
    return(p)
  }
}

#' @rdname plot.bats
#' @export
autoplot.tbats <- function(object, range.bars = FALSE, ...) {
  cl <- match.call()
  cl[[1]] <- quote(autoplot.bats)
  eval.parent(cl)
}

#' @rdname plot.bats
#' @export
autoplot.bats <- function(object, range.bars = FALSE, ...) {
  data <- tbats.components(object)

  cn <- colnames(data)
  # Convert to longform
  data <- data.frame(
    datetime = rep(time(data), NCOL(data)), y = c(data),
    parts = factor(rep(cn, each = NROW(data)), levels = cn)
  )

  # Initialise ggplot object
  p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data, ylab = "")

  # Add data
  p <- p + ggplot2::geom_line(na.rm = TRUE)
  p <- p + ggplot2::facet_grid(parts ~ ., scales = "free_y", switch = "y")

  if (range.bars) {
    yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
    xranges <- range(data$datetime)
    barmid <- apply(yranges, 2, mean)
    barlength <- min(apply(yranges, 2, diff))
    barwidth <- (1 / 64) * diff(xranges)
    barpos <- data.frame(
      left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
      top = barmid + barlength / 2, bottom = barmid - barlength / 2,
      parts = colnames(yranges), datetime = xranges[2], y = barmid
    )
    p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
  }

  p <- p + ggAddExtras(xlab = NULL, ylab = "", main = paste("Components of", object$method, "method"))
  return(p)
}

#' @rdname plot.forecast
#' @export
autoplot.forecast <- function(object, include, PI=TRUE, shadecols=c("#596DD5", "#D5DBFF"), fcol="#0000AA", flwd=0.5, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!is.forecast(object)) {
      stop("autoplot.forecast requires a forecast object, use object=object")
    }
    if (is.null(object$lower) || is.null(object$upper) || is.null(object$level)) {
      PI <- FALSE
    }
    else if (!is.finite(max(object$upper))) {
      PI <- FALSE
    }

    if (!is.null(object$model$terms) && !is.null(object$model$model)) {
      # Initialise original dataset
      mt <- object$model$terms
      if (!is.null(object$series)) {
        yvar <- object$series
      } else {
        yvar <- deparse(mt[[2]])
      } # Perhaps a better way to do this
      xvar <- attr(mt, "term.labels")
      vars <- c(yvar = yvar, xvar = xvar)
      data <- object$model$model
      colnames(data) <- names(vars)[match(colnames(data), vars)]
      if (!is.null(object$model$lambda)) {
        data$yvar <- InvBoxCox(data$yvar, object$model$lambda)
      }
    }
    else {
      if (!is.null(object$x)) {
        data <- data.frame(yvar = c(object$x))
      }
      else if (!is.null(object$residuals) && !is.null(object$fitted)) {
        data <- data.frame(yvar = c(object$residuals + object$fitted))
      }
      else {
        stop("Could not find data")
      }
      if (!is.null(object$series)) {
        vars <- c(yvar = object$series)
      }
      else if (!is.null(object$model$call)) {
        vars <- c(yvar = deparse(object$model$call$y))
        if (vars == "object") {
          vars <- c(yvar = "y")
        }
      }
      else {
        vars <- c(yvar = "y")
      }
    }

    # Initialise ggplot object
    p <- ggplot2::ggplot()

    # Cross sectional forecasts
    if (!is.element("ts", class(object$mean))) {
      if (length(xvar) > 1) {
        stop("Forecast plot for regression models only available for a single predictor")
      }
      if (NCOL(object$newdata) == 1) { # Make sure column has correct name
        colnames(object$newdata) <- xvar
      }
      flwd <- 2 * flwd # Scale for points

      # Data points
      p <- p + ggplot2::geom_point(ggplot2::aes_(x = ~xvar, y = ~yvar), data = data)
      p <- p + ggplot2::labs(y = vars["yvar"], x = vars["xvar"])

      # Forecasted intervals
      if (PI) {
        levels <- NROW(object$level)
        interval <- data.frame(xpred = rep(object$newdata[[1]], levels), lower = c(object$lower), upper = c(object$upper), level = rep(object$level, each = NROW(object$newdata[[1]])))
        interval <- interval[order(interval$level, decreasing = TRUE), ] # Must be ordered for gg z-index
        p <- p + ggplot2::geom_linerange(ggplot2::aes_(x = ~xpred, ymin = ~lower, ymax = ~upper, colour = ~level), data = interval, size = flwd)
        if (length(object$level) <= 5) {
          p <- p + ggplot2::scale_colour_gradientn(breaks = object$level, colours = shadecols, guide = "legend")
        }
        else {
          p <- p + ggplot2::scale_colour_gradientn(colours = shadecols, guide = "colourbar")
        }
      }

      # Forecasted points
      predicted <- data.frame(object$newdata, object$mean)
      colnames(predicted) <- c("xpred", "ypred")
      p <- p + ggplot2::geom_point(ggplot2::aes_(x = ~xpred, y = ~ypred), data = predicted, color = fcol, size = flwd)

      # Line of best fit
      coef <- data.frame(int = 0, m = 0)
      i <- match("(Intercept)", names(object$model$coefficients))
      if (i != 0) {
        coef$int <- object$model$coefficients[i]
        if (NROW(object$model$coefficients) == 2) {
          coef$m <- object$model$coefficients[-i]
        }
      }
      else {
        if (NROW(object$model$coefficients) == 1) {
          coef$m <- object$model$coefficients
        }
      }
      p <- p + ggplot2::geom_abline(intercept = coef$int, slope = coef$m)
    }
    else {
      # Time series objects (assumed)

      # Data points
      if (!is.null(time(object$x))) {
        timex <- time(object$x)
      }
      else if (!is.null(time(object$model$residuals))) {
        timex <- time(object$model$residuals)
      }
      data <- data.frame(yvar = as.numeric(data$yvar), datetime = as.numeric(timex))
      if (!missing(include)) {
        data <- tail(data, include)
      }
      p <- p + ggplot2::scale_x_continuous()
      p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~yvar), data = data) +
        ggplot2::labs(y = vars["yvar"], x = "Time")

      # Forecasted intervals
      predicted <- data.frame(xvar = time(object$mean), yvar = object$mean)
      colnames(predicted) <- c("datetime", "ypred")
      if (PI) {
        levels <- NROW(object$level)
        interval <- data.frame(datetime = rep(predicted$datetime, levels), lower = c(object$lower), upper = c(object$upper), level = rep(object$level, each = NROW(object$mean)))
        interval <- interval[order(interval$level, decreasing = TRUE), ] # Must be ordered for gg z-index
        p <- p + ggplot2::geom_ribbon(ggplot2::aes_(x = ~datetime, ymin = ~lower, ymax = ~upper, group = ~-level, fill = ~level), data = interval)
        if (min(object$level) < 50) {
          scalelimit <- c(1, 99)
        }
        else {
          scalelimit <- c(50, 99)
        }
        if (length(object$level) <= 5) {
          p <- p + ggplot2::scale_fill_gradientn(breaks = object$level, colours = shadecols, limit = scalelimit, guide = "legend")
        }
        else {
          p <- p + ggplot2::scale_fill_gradientn(colours = shadecols, limit = scalelimit)
        }
        # Negative group is a work around for missing z-index
      }

      # Forecasted points
      p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~ypred), data = predicted, color = fcol, size = flwd)
    }

    p <- p + ggAddExtras(main = paste("Forecasts from ", object$method, sep = ""))
    return(p)
  }
}

#' @rdname plot.mforecast
#' @export
autoplot.mforecast <- function(object, PI = TRUE, facets = TRUE, colour = FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!is.mforecast(object)) {
      stop("autoplot.mforecast requires a mforecast object, use object=object")
    }
    if (is.ts(object$forecast[[1]]$mean)) {
      # ts forecasts
      p <- autoplot(getResponse(object), facets = facets, colour = colour) + autolayer(object, ...)
      if (facets) {
        p <- p + ggplot2::facet_wrap(
          ~ series,
          labeller = function(labels) {
            if (!is.null(object$method)) {
              lapply(labels, function(x) paste0(as.character(x), "\n", object$method[as.character(x)]))
            }
            else {
              lapply(labels, function(x) paste0(as.character(x)))
            }
          },
          ncol = 1,
          scales = "free_y"
        )
      }
      p <- p + ggAddExtras(ylab = NULL)
      return(p)
    }
    else {
      # lm forecasts
      if (!requireNamespace("grid")) {
        stop("grid is needed for this function to work. Install it via install.packages(\"grid\")", call. = FALSE)
      }

      K <- length(object$forecast)
      if (K < 2) {
        warning("Expected at least two plots but forecast required less.")
      }

      # Set up vector arguments
      if (missing(PI)) {
        PI <- rep(TRUE, K)
      }

      # Set up grid
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      gridlayout <- matrix(seq(1, K), ncol = 1, nrow = K)

      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))

      for (i in 1:K) {
        partialfcast <- object$forecast[[i]]
        partialfcast$model <- mlmsplit(object$model, index = i)
        matchidx <- as.data.frame(which(gridlayout == i, arr.ind = TRUE))
        print(
          autoplot(
            structure(partialfcast, class = "forecast"),
            PI = PI[i], ...
          ) + ggAddExtras(ylab = names(object$forecast)[i]),
          vp = grid::viewport(
            layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col
          )
        )
      }
    }
  }
}

#' @rdname tsdisplay
#'
#' @examples
#' library(ggplot2)
#' ggtsdisplay(USAccDeaths, plot.type="scatter", theme=theme_bw())
#'
#' @export
ggtsdisplay <- function(x, plot.type=c("partial", "histogram", "scatter", "spectrum"),
                        points=TRUE, smooth=FALSE,
                        lag.max, na.action=na.contiguous, theme=NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else if (!requireNamespace("grid", quietly = TRUE)) {
    stop("grid is needed for this function to work. Install it via install.packages(\"grid\")", call. = FALSE)
  }
  else {
    if (NCOL(x) > 1) {
      stop("ggtsdisplay is only for univariate time series")
    }
    plot.type <- match.arg(plot.type)
    main <- deparse(substitute(x))

    if (!is.ts(x)) {
      x <- ts(x)
    }
    if (missing(lag.max)) {
      lag.max <- round(min(max(10 * log10(length(x)), 3 * frequency(x)), length(x) / 3))
    }

    dots <- list(...)
    if (is.null(dots$xlab)) {
      dots$xlab <- ""
    }
    if (is.null(dots$ylab)) {
      dots$ylab <- ""
    }
    labs <- match(c("xlab", "ylab", "main"), names(dots), nomatch = 0)

    # Set up grid for plots
    gridlayout <- matrix(c(1, 2, 1, 3), nrow = 2)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))

    # Add ts plot with points
    matchidx <- as.data.frame(which(gridlayout == 1, arr.ind = TRUE))
    tsplot <- do.call(ggplot2::autoplot, c(object = quote(x), dots[labs]))
    if (points) {
      tsplot <- tsplot + ggplot2::geom_point(size = 0.5)
    }
    if (smooth) {
      tsplot <- tsplot + ggplot2::geom_smooth(method = "loess", se = FALSE)
    }
    if (is.null(tsplot$labels$title)) { # Add title if missing
      tsplot <- tsplot + ggplot2::ggtitle(main)
    }
    if (!is.null(theme)) {
      tsplot <- tsplot + theme
    }
    print(
      tsplot,
      vp = grid::viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      )
    )

    # Prepare Acf plot
    acfplot <- do.call(ggAcf, c(x = quote(x), lag.max = lag.max, na.action = na.action, dots[-labs])) + ggplot2::ggtitle(NULL)
    if (!is.null(theme)) {
      acfplot <- acfplot + theme
    }

    # Prepare last plot (variable)
    if (plot.type == "partial") {
      lastplot <- ggPacf(x, lag.max = lag.max, na.action = na.action) + ggplot2::ggtitle(NULL)
      # Match y-axis
      acfplotrange <- ggplot2::layer_scales(acfplot)$y$range$range
      pacfplotrange <- ggplot2::layer_scales(lastplot)$y$range$range
      yrange <- range(c(acfplotrange, pacfplotrange))
      acfplot <- acfplot + ggplot2::ylim(yrange)
      lastplot <- lastplot + ggplot2::ylim(yrange)
    }
    else if (plot.type == "histogram") {
      lastplot <- gghistogram(x, add.normal = TRUE, add.rug = TRUE) + ggplot2::xlab(main)
    }
    else if (plot.type == "scatter") {
      scatterData <- data.frame(y = x[2:NROW(x)], x = x[1:NROW(x) - 1])
      lastplot <- ggplot2::ggplot(ggplot2::aes_(y = ~y, x = ~x), data = scatterData) +
        ggplot2::geom_point() + ggplot2::labs(x = expression(Y[t - 1]), y = expression(Y[t]))
    }
    else if (plot.type == "spectrum") {
      specData <- spec.ar(x, plot = FALSE)
      specData <- data.frame(spectrum = specData$spec, frequency = specData$freq)
      lastplot <- ggplot2::ggplot(ggplot2::aes_(y = ~spectrum, x = ~frequency), data = specData) +
        ggplot2::geom_line() + ggplot2::scale_y_log10()
    }
    if (!is.null(theme)) {
      lastplot <- lastplot + theme
    }

    # Add ACF plot
    matchidx <- as.data.frame(which(gridlayout == 2, arr.ind = TRUE))
    print(
      acfplot,
      vp = grid::viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      )
    )

    # Add last plot
    matchidx <- as.data.frame(which(gridlayout == 3, arr.ind = TRUE))
    print(
      lastplot,
      vp = grid::viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      )
    )
  }
}



#' Time series lag ggplots
#'
#' Plots a lag plot using ggplot.
#'
#' \dQuote{gglagplot} will plot time series against lagged versions of
#' themselves. Helps visualising 'auto-dependence' even when auto-correlations
#' vanish.
#'
#' \dQuote{gglagchull} will layer convex hulls of the lags, layered on a single
#' plot. This helps visualise the change in 'auto-dependence' as lags increase.
#'
#' @param x a time series object (type \code{ts}).
#' @param lags number of lag plots desired, see arg set.lags.
#' @param set.lags vector of positive integers specifying which lags to use.
#' @param diag logical indicating if the x=y diagonal should be drawn.
#' @param diag.col color to be used for the diagonal if(diag).
#' @param do.lines if TRUE, lines will be drawn, otherwise points will be
#' drawn.
#' @param colour logical indicating if lines should be coloured.
#' @param continuous Should the colour scheme for years be continuous or
#' discrete?
#' @param labels logical indicating if labels should be used.
#' @param seasonal Should the line colour be based on seasonal characteristics
#' (TRUE), or sequential (FALSE).
#' @param \dots Not used (for consistency with lag.plot)
#' @return None.
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link[stats]{lag.plot}}
#' @examples
#'
#' gglagplot(woolyrnq)
#' gglagplot(woolyrnq,seasonal=FALSE)
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' gglagplot(lungDeaths, lags=2)
#' gglagchull(lungDeaths, lags=6)
#'
#' @export
gglagplot <- function(x, lags=ifelse(frequency(x) > 9, 16, 9),
                      set.lags = 1:lags, diag=TRUE, diag.col="gray", do.lines = TRUE, colour = TRUE,
                      continuous = frequency(x) > 12, labels = FALSE, seasonal = TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    freq <- frequency(x)
    if (freq > 1) {
      linecol <- cycle(x)
      if (freq > 24) {
        continuous <- TRUE
      }
    }
    else {
      seasonal <- FALSE
      continuous <- TRUE
    }
    if (!seasonal) {
      continuous <- TRUE
    }

    # Make sure lags is evaluated
    tmp <- lags
    x <- as.matrix(x)

    # Prepare data for plotting
    n <- NROW(x)
    data <- data.frame()
    for (i in 1:NCOL(x)) {
      for (lagi in set.lags) {
        sname <- colnames(x)[i]
        if (is.null(sname)) {
          sname <- deparse(match.call()$x)
        }
        data <- rbind(
          data,
          data.frame(
            lagnum = 1:(n - lagi),
            freqcur = ifelse(rep(seasonal, n - lagi), linecol[1:(n - lagi)], 1:(n - lagi)),
            orig = x[1:(n - lagi), i],
            lagged = x[(lagi + 1):n, i],
            lagVal = rep(lagi, n - lagi),
            series = factor(rep(sname, n - lagi))
          )
        )
      }
    }
    if (!continuous) {
      data$freqcur <- factor(data$freqcur)
    }

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~lagged, y = ~orig), data = data)

    if (diag) {
      p <- p + ggplot2::geom_abline(colour = diag.col, linetype = "dashed")
    }
    if (labels) {
      linesize <- 0.25 * (2 - do.lines)
    }
    else {
      linesize <- 0.5 * (2 - do.lines)
    }
    plottype <- if (do.lines) {
      ggplot2::geom_path
    }
    else {
      ggplot2::geom_point
    }
    if (colour) {
      p <- p + plottype(ggplot2::aes_(colour = ~freqcur), size = linesize)
    }
    else {
      p <- p + plottype(size = linesize)
    }

    if (labels) {
      p <- p + ggplot2::geom_text(ggplot2::aes_(label = ~lagnum))
    }
    # Ensure all facets are of size size (if extreme values are excluded in lag specification)
    if (max(set.lags) > NROW(x) / 2) {
      axissize <- rbind(aggregate(orig ~ series, data = data, min), aggregate(orig~ series, data = data, max))
      axissize <- data.frame(series = rep(axissize$series, length(set.lags)), orig = rep(axissize$orig, length(set.lags)), lagVal = rep(set.lags, each = NCOL(x)))
      p <- p + ggplot2::geom_blank(ggplot2::aes_(x = ~orig, y = ~orig), data = axissize)
    }

    # Facet
    labellerFn <- function(labels) {
      if (!is.null(labels$series)) {
        # Multivariate labels
        labels$series <- as.character(labels$series)
      }
      labels$lagVal <- paste("lag", labels$lagVal)
      return(labels)
    }
    if (NCOL(x) > 1) {
      p <- p + ggplot2::facet_wrap(~series + lagVal, scales = "free", labeller = labellerFn)
    }
    else {
      p <- p + ggplot2::facet_wrap(~lagVal, labeller = labellerFn)
    }
    p <- p + ggplot2::theme(aspect.ratio = 1)
    if (colour) {
      if (seasonal) {
        if (freq == 4L) {
          title <- "Quarter"
        } else if (freq == 12L) {
          title <- "Month"
        } else if (freq == 7L) {
          title <- "Day"
        } else if (freq == 24L) {
          title <- "Hour"
        } else {
          title <- "Season"
        }
      }
      else {
        title <- "Time"
      }
      if (continuous) {
        p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = title))
      }
      else {
        p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = title))
      }
    }

    p <- p + ggAddExtras(ylab = NULL, xlab = NULL)

    return(p)
  }
}

#' @rdname gglagplot
#'
#' @examples
#' gglagchull(woolyrnq)
#'
#' @export
gglagchull <- function(x,
                       lags=ifelse(frequency(x) > 1, min(12, frequency(x)), 4),
                       set.lags = 1:lags, diag=TRUE, diag.col="gray", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    # Make sure lags is evaluated
    tmp <- lags
    x <- as.matrix(x)

    # Prepare data for plotting
    n <- NROW(x)
    data <- data.frame()
    for (i in 1:NCOL(x)) {
      for (lag in set.lags) {
        sname <- colnames(x)[i]
        if (is.null(sname)) {
          sname <- substitute(x)
        }
        data <- rbind(data, data.frame(orig = x[(lag + 1):n, i], lagged = x[1:(n - lag), i], lag = rep(lag, n - lag), series = rep(sname, n - lag))[grDevices::chull(x[(lag + 1):n, i], x[1:(n - lag), i]), ])
      }
    }

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~orig, y = ~lagged), data = data)
    if (diag) {
      p <- p + ggplot2::geom_abline(colour = diag.col, linetype = "dashed")
    }
    p <- p + ggplot2::geom_polygon(ggplot2::aes_(group = ~lag, colour = ~lag, fill = ~lag), alpha = 1 / length(set.lags))
    p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = "lag"))
    p <- p + ggplot2::theme(aspect.ratio = 1)

    # Facet
    if (NCOL(x) > 1) {
      p <- p + ggplot2::facet_wrap(~series, scales = "free")
    }

    p <- p + ggAddExtras(ylab = "lagged", xlab = "original")

    return(p)
  }
}



#' Create a seasonal subseries ggplot
#'
#' Plots a subseries plot using ggplot. Each season is plotted as a separate
#' mini time series. The blue lines represent the mean of the observations
#' within each season.
#'
#' The \code{ggmonthplot} function is simply a wrapper for
#' \code{ggsubseriesplot} as a convenience for users familiar with
#' \code{\link[stats]{monthplot}}.
#'
#' @param x a time series object (type \code{ts}).
#' @param labels A vector of labels to use for each 'season'
#' @param times A vector of times for each observation
#' @param phase A vector of seasonal components
#' @param \dots Not used (for consistency with monthplot)
#' @return Returns an object of class \code{ggplot}.
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link[stats]{monthplot}}
#' @examples
#'
#' ggsubseriesplot(AirPassengers)
#' ggsubseriesplot(woolyrnq)
#'
#' @export
ggmonthplot <- function(x, labels = NULL, times = time(x), phase = cycle(x), ...) {
  ggsubseriesplot(x, labels, times, phase, ...)
}

#' @rdname ggmonthplot
#' @export
ggsubseriesplot <- function(x, labels = NULL, times = time(x), phase = cycle(x), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(x, "ts")) {
      stop("ggsubseriesplot requires a ts object, use x=object")
    }

    if (round(frequency(x)) <= 1) {
      stop("Data are not seasonal")
    }
    
    if("1" %in% dimnames(table(table(phase)))[[1]]){
      stop(paste("Each season requires at least 2 observations.",
                  ifelse(frequency(x)%%1 == 0,
                         "Your series length may be too short for this graphic.",
                         "This may be caused from specifying a time-series with non-integer frequency.")
                  )
           )
    }

    data <- data.frame(y = as.numeric(x), year = trunc(time(x)), season = as.numeric(phase))
    seasonwidth <- (max(data$year) - min(data$year)) * 1.05
    data$time <- data$season + 0.025 + (data$year - min(data$year)) / seasonwidth
    avgLines <- stats::aggregate(data$y, by = list(data$season), FUN = mean)
    colnames(avgLines) <- c("season", "avg")
    data <- merge(data, avgLines, by = "season")

    # Initialise ggplot object
    # p <- ggplot2::ggplot(ggplot2::aes_(x=~interaction(year, season), y=~y, group=~season), data=data, na.rm=TRUE)
    p <- ggplot2::ggplot(
      ggplot2::aes_(x = ~time, y = ~y, group = ~season),
      data = data, na.rm = TRUE
    )

    # Remove vertical break lines
    p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    # Add data
    p <- p + ggplot2::geom_line()

    # Add average lines
    p <- p + ggplot2::geom_line(ggplot2::aes_(y = ~avg), col = "#0000AA")

    # Create x-axis labels
    xfreq <- frequency(x)
    if (xfreq == 4) {
      xbreaks <- c("Q1", "Q2", "Q3", "Q4")
      xlab <- "Quarter"
    }
    else if (xfreq == 7) {
      xbreaks <- c(
        "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
        "Friday", "Saturday"
      )
      xlab <- "Day"
    }
    else if (xfreq == 12) {
      xbreaks <- month.abb
      xlab <- "Month"
    }
    else {
      xbreaks <- 1:frequency(x)
      xlab <- "Season"
    }

    # X-axis
    p <- p + ggplot2::scale_x_continuous(breaks = 0.5 + (1:xfreq), labels = xbreaks)

    # Graph labels
    p <- p + ggAddExtras(ylab = deparse(substitute(x)), xlab = xlab)
    return(p)
  }
}

#' @rdname seasonplot
#'
#' @examples
#' ggseasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)
#' ggseasonplot(AirPassengers, year.labels=TRUE, continuous=TRUE)
#'
#' @export
ggseasonplot <- function(x, season.labels=NULL, year.labels=FALSE, year.labels.left=FALSE, type=NULL, col=NULL, continuous=FALSE, polar=FALSE, labelgap=0.04, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  if (!inherits(x, "ts")) {
    stop("autoplot.seasonplot requires a ts object, use x=object")
  }
  if (!is.null(type)) {
    message("Plot types are not yet supported for seasonplot()")
  }

  # Check data are seasonal and convert to integer seasonality
  s <- round(frequency(x))
  if (s <= 1) {
    stop("Data are not seasonal")
  }

  # Grab name for plot title
  xname <- deparse(substitute(x))

  tspx <- tsp(x)
  x <- ts(x, start = tspx[1], frequency = s)

  data <- data.frame(
    y = as.numeric(x),
    year = trunc(time(x)),
    cycle = as.numeric(cycle(x)),
    time = as.numeric((cycle(x) - 1) / s)
  )
  data$year <- if (continuous) {
    as.numeric(data$year)
  }
  else {
    as.factor(data$year)
  }
  if (polar) {
    startValues <- data[data$cycle == 1, ]
    if (data$cycle[1] == 1) {
      startValues <- startValues[-1, ]
    }
    startValues$time <- 1 - .Machine$double.eps
    levels(startValues$year) <- as.numeric(levels(startValues$year)) - 1
    data <- rbind(data, startValues)
  }
  # Initialise ggplot object
  p <- ggplot2::ggplot(ggplot2::aes_(x = ~time, y = ~y, group = ~year, colour = ~year), data = data, na.rm = TRUE)
  # p <- p + ggplot2::scale_x_continuous()

  # Add data
  p <- p + ggplot2::geom_line()

  if (!is.null(col)) {
    if (continuous) {
      p <- p + ggplot2::scale_color_gradientn(colours = col)
    }
    else {
      ncol <- length(unique(data$year))
      if (length(col) == 1) {
        p <- p + ggplot2::scale_color_manual(guide = "none", values = rep(col, ncol))
      }
      else {
        p <- p + ggplot2::scale_color_manual(values = rep(col, ceiling(ncol / length(col)))[1:ncol])
      }
    }
  }

  if (year.labels) {
    yrlab <- stats::aggregate(time ~ year, data = data, FUN = max)
    yrlab <- cbind(yrlab, offset = labelgap)
  }
  if (year.labels.left) {
    yrlabL <- stats::aggregate(time ~ year, data = data, FUN = min)
    yrlabL <- cbind(yrlabL, offset = -labelgap)
    if (year.labels) {
      yrlab <- rbind(yrlab, yrlabL)
    }
  }
  if (year.labels || year.labels.left) {
    yrlab <- merge(yrlab, data)
    yrlab$time <- yrlab$time + yrlab$offset
    p <- p + ggplot2::guides(colour = FALSE)
    p <- p + ggplot2::geom_text(ggplot2::aes_(x = ~time, y = ~y, label = ~year), data = yrlab)
  }

  # Add seasonal labels
  if (s == 12) {
    labs <- month.abb
    xLab <- "Month"
  }
  else if (s == 4) {
    labs <- paste("Q", 1:4, sep = "")
    xLab <- "Quarter"
  }
  else if (s == 7) {
    labs <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    xLab <- "Day"
  }
  else if (s == 52) {
    labs <- 1:s
    xLab <- "Week"
  }
  else if (s == 24) {
    labs <- 0:(s - 1)
    xLab <- "Hour"
  }
  else if (s == 48) {
    labs <- seq(0, 23.5, by = 0.5)
    xLab <- "Half-hour"
  }
  else {
    labs <- 1:s
    xLab <- "Season"
  }

  if (!is.null(season.labels)) {
    if (length(season.labels) != length(labs)) {
      warning(paste0("Provided season.labels have length ", length(season.labels), ", but ", length(labs), " are required. Ignoring season.labels."))
    }
    else {
      labs <- season.labels
    }
  }

  breaks <- sort(unique(data$time))
  if (polar) {
    breaks <- head(breaks, -1)
    p <- p + ggplot2::coord_polar()
  }

  p <- p + ggplot2::scale_x_continuous(breaks = breaks, minor_breaks = NULL, labels = labs)

  # Graph title and axes
  p <- p + ggAddExtras(main = paste("Seasonal plot:", xname), xlab = xLab, ylab = NULL)
  return(p)
}

#' @rdname plot.forecast
#' @export
autoplot.splineforecast <- function(object, PI=TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    p <- autoplot(object$x) + autolayer(object)
    p <- p + ggplot2::geom_point(size = 2)
    fit <- data.frame(datetime = as.numeric(time(object$fitted)), y = as.numeric(object$fitted))
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~y), colour = "red", data = fit)
    p <- p + ggAddExtras(ylab = deparse(object$model$call$x))
    if (!is.null(object$series)) {
      p <- p + ggplot2::ylab(object$series)
    }
    return(p)
  }
}

#' @rdname autoplot.seas
#' @export
autoplot.stl <- function(object, labels = NULL, range.bars = TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "stl")) {
      stop("autoplot.stl requires a stl object, use x=object")
    }
    # Re-order series as trend, seasonal, remainder
    object$time.series <- object$time.series[, c("trend", "seasonal", "remainder")]
    if (is.null(labels)) {
      labels <- colnames(object$time.series)
    }

    data <- object$time.series
    cn <- c("data", labels)
    data <- data.frame(
      datetime = rep(time(data), NCOL(data) + 1), y = c(rowSums(data), data),
      parts = factor(rep(cn, each = NROW(data)), levels = cn)
    )

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data)

    # Add data
    # Timeseries lines
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~y), data = subset(data, data$parts != cn[4]), na.rm = TRUE)
    p <- p + ggplot2::geom_segment(
      ggplot2::aes_(x = ~datetime, xend = ~datetime, y = 0, yend = ~y),
      data = subset(data, data$parts == cn[4]), lineend = "butt"
    )

    # Rangebars
    if (range.bars) {
      yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
      xranges <- range(data$datetime)
      barmid <- apply(yranges, 2, mean)
      barlength <- min(apply(yranges, 2, diff))
      barwidth <- (1 / 64) * diff(xranges)
      barpos <- data.frame(
        left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
        top = barmid + barlength / 2, bottom = barmid - barlength / 2,
        parts = colnames(yranges), datetime = xranges[2], y = barmid
      )
      p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
    }

    # Remainder
    p <- p + ggplot2::facet_grid("parts ~ .", scales = "free_y", switch = "y")
    p <- p + ggplot2::geom_hline(ggplot2::aes_(yintercept = ~y), data = data.frame(y = 0, parts = cn[4]))

    # Add axis labels
    p <- p + ggAddExtras(xlab = "Time", ylab = "")

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks = unique(round(pretty(data$datetime))))
    # ^^ Remove rightmost x axis gap with `expand=c(0.05, 0, 0, 0)` argument when assymetric `expand` feature is supported
    # issue: tidyverse/ggplot2#1669

    return(p)
  }
}

#' @rdname autoplot.seas
#' @export
autoplot.StructTS <- function(object, labels = NULL, range.bars = TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "StructTS")) {
      stop("autoplot.StructTS requires a StructTS object.")
    }

    if (is.null(labels)) {
      labels <- colnames(object$fitted)
    }

    data <- object$fitted
    cn <- c("data", labels)
    data <- data.frame(
      datetime = rep(time(data), NCOL(data) + 1), y = c(object$data, data),
      parts = factor(rep(cn, each = NROW(data)), levels = cn)
    )

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data)

    # Add data
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~y), na.rm = TRUE)
    p <- p + ggplot2::facet_grid("parts ~ .", scales = "free_y", switch = "y")

    if (range.bars) {
      yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
      xranges <- range(data$datetime)
      barmid <- apply(yranges, 2, mean)
      barlength <- min(apply(yranges, 2, diff))
      barwidth <- (1 / 64) * diff(xranges)
      barpos <- data.frame(
        left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
        top = barmid + barlength / 2, bottom = barmid - barlength / 2,
        parts = colnames(yranges), datetime = xranges[2], y = barmid
      )
      p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
    }

    # Add axis labels
    p <- p + ggAddExtras(xlab = "Time", ylab = "")

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks = unique(round(pretty(data$datetime))))

    return(p)
  }
}



#' Plot time series decomposition components using ggplot
#'
#' Produces a ggplot object of seasonally decomposed time series for objects of
#' class \dQuote{\code{stl}} (created with \code{\link[stats]{stl}}), class
#' \dQuote{\code{seas}} (created with \code{\link[seasonal]{seas}}), or class
#' \dQuote{\code{decomposed.ts}} (created with \code{\link[stats]{decompose}}).
#'
#' @param object Object of class \dQuote{\code{seas}}, \dQuote{\code{stl}}, or
#' \dQuote{\code{decomposed.ts}}.
#' @param labels Labels to replace \dQuote{seasonal}, \dQuote{trend}, and
#' \dQuote{remainder}.
#' @param range.bars Logical indicating if each plot should have a bar at its
#' right side representing relative size. If \code{NULL}, automatic selection
#' takes place.
#' @param ... Other plotting parameters to affect the plot.
#' @return Returns an object of class \code{ggplot}.
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link[seasonal]{seas}}, \code{\link[stats]{stl}},
#' \code{\link[stats]{decompose}}, \code{\link[stats]{StructTS}},
#' \code{\link[stats]{plot.stl}}.
#' @examples
#'
#' library(ggplot2)
#' co2 %>% decompose %>% autoplot
#' nottem %>% stl(s.window='periodic') %>% autoplot
#'
#' \dontrun{
#' library(seasonal)
#' seas(USAccDeaths) %>% autoplot
#' }
#'
#' @export
autoplot.seas <- function(object, labels = NULL, range.bars = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!inherits(object, "seas")) {
      stop("autoplot.seas requires a seas object")
    }
    if (is.null(labels)) {
      labels <- c("seasonal", "trend", "remainder")
    }

    data <- cbind(object$x, object$data[, c("seasonal", "trend", "irregular")])
    cn <- c("data", labels)
    data <- data.frame(
      datetime = rep(time(data), NCOL(data)), y = c(data),
      parts = factor(rep(cn, each = NROW(data)), levels = cn)
    )

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~datetime, y = ~y), data = data)

    # Add data
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~y), data = subset(data, data$parts != cn[4]), na.rm = TRUE)
    p <- p + ggplot2::geom_segment(
      ggplot2::aes_(x = ~datetime, xend = ~datetime, y = 1, yend = ~y),
      data = subset(data, data$parts == cn[4]), lineend = "butt"
    )
    p <- p + ggplot2::facet_grid("parts ~ .", scales = "free_y", switch = "y")
    p <- p + ggplot2::geom_hline(ggplot2::aes_(yintercept = ~y), data = data.frame(y = 1, parts = cn[4]))

    # Rangebars
    if (is.null(range.bars)) {
      range.bars <- object$spc$transform$`function` == "none"
    }
    if (range.bars) {
      yranges <- vapply(split(data$y, data$parts), function(x) range(x, na.rm = TRUE), numeric(2))
      xranges <- range(data$datetime)
      barmid <- apply(yranges, 2, mean)
      barlength <- min(apply(yranges, 2, diff))
      barwidth <- (1 / 64) * diff(xranges)
      barpos <- data.frame(
        left = xranges[2] + barwidth, right = xranges[2] + barwidth * 2,
        top = barmid + barlength / 2, bottom = barmid - barlength / 2,
        parts = colnames(yranges), datetime = xranges[2], y = barmid
      )
      p <- p + ggplot2::geom_rect(ggplot2::aes_(xmin = ~left, xmax = ~right, ymax = ~top, ymin = ~bottom), data = barpos, fill = "gray75", colour = "black", size = 1 / 3)
    }

    # Add axis labels
    p <- p + ggAddExtras(xlab = "Time", ylab = "")

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks = unique(round(pretty(data$datetime))))

    return(p)
  }
}

#' @rdname autoplot.ts
#' @export
autolayer.mts <- function(object, colour=TRUE, series=NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    cl <- match.call()
    cl[[1]] <- quote(autolayer)
    cl$object <- quote(object[, i])
    if (length(series) != NCOL(object)) {
      if (colour) {
        message("For a multivariate timeseries, specify a seriesname for each timeseries. Defaulting to column names.")
      }
      series <- colnames(object)
    }
    out <- list()
    for (i in 1:NCOL(object)) {
      cl$series <- series[i]
      out[[i]] <- eval(cl)
    }
    return(out)
  }
}

#' @rdname autoplot.ts
#' @export
autolayer.msts <- function(object, series = NULL, ...) {
  if (NCOL(object) > 1) {
    class(object) <- c("mts", "ts", "matrix")
  }
  else {
    if (is.null(series)) {
      series <- deparse(substitute(series))
    }
    class(object) <- c("ts")
  }
  attr(object, "msts") <- NULL
  autolayer(object, series = series, ...)
}

#' @rdname autoplot.ts
#' @export
autolayer.ts <- function(object, colour=TRUE, series=NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    tsdata <- data.frame(
      timeVal = as.numeric(time(object)),
      series = ifelse(is.null(series), deparse(substitute(object)), series),
      seriesVal = as.numeric(object)
    )
    if (colour) {
      ggplot2::geom_line(ggplot2::aes_(x = ~timeVal, y = ~seriesVal, group = ~series, colour = ~series), data = tsdata, ...)
    }
    else {
      ggplot2::geom_line(ggplot2::aes_(x = ~timeVal, y = ~seriesVal, group = ~series), data = tsdata, ...)
    }
  }
}

#' @rdname plot.forecast
#' @export
autolayer.forecast <- function(object, series = NULL, PI = TRUE, showgap = TRUE, ...) {
  PI <- PI & !is.null(object$level)
  data <- forecast2plotdf(object, PI = PI, showgap = showgap)
  mapping <- ggplot2::aes_(x = ~x, y = ~y)
  if (!is.null(object$series)) {
    data[["series"]] <- object$series
  }
  if (!is.null(series)) {
    data[["series"]] <- series
    mapping$colour <- quote(series)
  }
  if (PI) {
    mapping$level <- quote(level)
    mapping$ymin <- quote(ymin)
    mapping$ymax <- quote(ymax)
  }
  geom_forecast(mapping = mapping, data = data, stat = "identity", ...)
}

#' @rdname plot.mforecast
#' @export
autolayer.mforecast <- function(object, series = NULL, PI = TRUE, ...) {
  cl <- match.call()
  cl[[1]] <- quote(autolayer)
  cl$object <- quote(object$forecast[[i]])
  if (!is.null(series)) {
    if (length(series) != length(object$forecast)) {
      series <- names(object$forecast)
    }
  }
  out <- list()
  for (i in 1:length(object$forecast)) {
    cl$series <- series[i]
    out[[i]] <- eval(cl)
  }
  return(out)
}



#' Automatically create a ggplot for time series objects
#'
#' \code{autoplot} takes an object of type \code{ts} or \code{mts} and creates
#' a ggplot object suitable for usage with \code{stat_forecast}.
#'
#' \code{fortify.ts} takes a \code{ts} object and converts it into a data frame
#' (for usage with ggplot2).
#'
#' @param object Object of class \dQuote{\code{ts}} or \dQuote{\code{mts}}.
#' @param series Identifies the timeseries with a colour, which integrates well
#' with the functionality of \link{geom_forecast}.
#' @param facets If TRUE, multiple time series will be faceted (and unless
#' specified, colour is set to FALSE). If FALSE, each series will be assigned a
#' colour.
#' @param colour If TRUE, the time series will be assigned a colour aesthetic
#' @param model Object of class \dQuote{\code{ts}} to be converted to
#' \dQuote{\code{data.frame}}.
#' @param data Not used (required for \link{fortify} method)
#' @param ... Other plotting parameters to affect the plot.
#' @return None. Function produces a ggplot graph.
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link[stats]{plot.ts}}, \code{\link[ggplot2]{fortify}}
#' @examples
#'
#' library(ggplot2)
#' autoplot(USAccDeaths)
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' autoplot(lungDeaths)
#' autoplot(lungDeaths, facets=TRUE)
#'
#' @export
autoplot.ts <- function(object, series=NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!is.ts(object)) {
      stop("autoplot.ts requires a ts object, use object=object")
    }

    # Create data frame with time as a column labelled x
    # and time series as a column labelled y.
    data <- data.frame(y = as.numeric(object), x = as.numeric(time(object)))
    if (!is.null(series)) {
      data <- transform(data, series = series)
    }

    # Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(y = ~y, x = ~x), data = data)

    # Add data
    if (!is.null(series)) {
      p <- p + ggplot2::geom_line(ggplot2::aes_(group = ~series, colour = ~series), na.rm = TRUE)
    }
    else {
      p <- p + ggplot2::geom_line(na.rm = TRUE)
    }

    # Add labels
    p <- p + ggAddExtras(xlab = "Time", ylab = deparse(substitute(object)))

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks = ggtsbreaks)
    return(p)
  }
}

#' @rdname autoplot.ts
#' @export
autoplot.mts <- function(object, colour=TRUE, facets=FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (!stats::is.mts(object)) {
      stop("autoplot.mts requires a mts object, use x=object")
    }
    if (NCOL(object) <= 1) {
      return(autoplot.ts(object))
    }

    cn <- colnames(object)
    if (is.null(cn)) {
      cn <- paste("Series", seq_len(NCOL(object)))
    }

    data <- data.frame(
      y = as.numeric(c(object)), x = rep(as.numeric(time(object)), NCOL(object)),
      series = factor(rep(cn, each = NROW(object)), levels = cn)
    )

    # Initialise ggplot object
    mapping <- ggplot2::aes_(y = ~y, x = ~x, group = ~series)
    if (colour && (!facets || !missing(colour))) {
      mapping$colour <- quote(series)
    }
    p <- ggplot2::ggplot(mapping, data = data)
    p <- p + ggplot2::geom_line(na.rm = TRUE)
    if (facets) {
      p <- p + ggplot2::facet_grid(series~., scales = "free_y")
    }
    p <- p + ggAddExtras(xlab = "Time", ylab = deparse(substitute(object)))
    return(p)
  }
}

#' @rdname autoplot.ts
#' @export
autoplot.msts <- function(object, series = NULL, ...) {
  sname <- deparse(substitute(object))
  if (NCOL(object) > 1) {
    class(object) <- c("mts", "ts", "matrix")
  }
  else {
    class(object) <- c("ts")
  }
  attr(object, "msts") <- NULL
  autoplot(object, series = series, ...) + ggAddExtras(ylab = sname)
}

#' @rdname autoplot.ts
#' @export
fortify.ts <- function(model, data, ...) {
  # Use ggfortify version if it is loaded
  # to prevent cran errors
  if (exists("ggfreqplot")) {
    tsp <- attr(model, which = "tsp")
    dtindex <- time(model)
    if (any(tsp[3] == c(4, 12))) {
      dtindex <- zoo::as.Date.yearmon(dtindex)
    }
    model <- data.frame(Index = dtindex, Data = as.numeric(model))
    return(ggplot2::fortify(model))
  }
  else {
    sname <- deparse(substitute(model))
    model <- cbind(x = as.numeric(time(model)), y = as.numeric(model))
    transform(as.data.frame(model), 
              time = x,
              series = sname)
  }
}

#' @rdname autoplot.ts
#' @export
fortify.mts <- function(model, data, ...){
  model <- cbind(x = as.numeric(time(model)), y = as.numeric(model))
  transform(as.data.frame(model), 
            time = x,
            series = rep(colnames(model), each = NROW(model)))
}

# Produce nice histogram with appropriately chosen bin widths
# Designed to work with time series data without issuing warnings.



#' Histogram with optional normal and kernel density functions
#'
#' Plots a histogram and density estimates using ggplot.
#'
#'
#' @param x a numerical vector.
#' @param add.normal Add a normal density function for comparison
#' @param add.kde Add a kernel density estimate for comparison
#' @param add.rug Add a rug plot on the horizontal axis
#' @param bins The number of bins to use for the histogram. Selected by default
#' using the Friedman-Diaconis rule given by \code{\link[grDevices]{nclass.FD}}
#' @param boundary A boundary between two bins.
#' @param \dots Not used (for consistency with lag.plot)
#' @return None.
#' @author Rob J Hyndman
#' @seealso \code{\link[graphics]{hist}}, \code{\link[ggplot2]{geom_histogram}}
#' @examples
#'
#' gghistogram(lynx, add.kde=TRUE)
#'
#' @export
gghistogram <- function(x, add.normal=FALSE, add.kde=FALSE, add.rug=TRUE, bins, boundary=0) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }
  else {
    if (missing(bins)) {
      bins <- min(500, grDevices::nclass.FD(na.exclude(x)))
    }
    data <- data.frame(x = as.numeric(c(x)))
    # Initialise ggplot object and plot histogram
    binwidth <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bins
    p <- ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(x), data = data, binwidth = binwidth, boundary = boundary) +
      ggplot2::xlab(deparse(substitute(x)))
    # Add normal density estimate
    if (add.normal || add.kde) {
      xmin <- min(x, na.rm = TRUE)
      xmax <- max(x, na.rm = TRUE)
      if (add.kde) {
        h <- stats::bw.SJ(x)
        xmin <- xmin - 3 * h
        xmax <- xmax + 3 * h
      }
      if (add.normal) {
        xmean <- mean(x, na.rm = TRUE)
        xsd <- sd(x, na.rm = TRUE)
        xmin <- min(xmin, xmean - 3 * xsd)
        xmax <- max(xmax, xmean + 3 * xsd)
      }
      xgrid <- seq(xmin, xmax, l = 512)
      if (add.normal) {
        df <- data.frame(x = xgrid, y = length(x) * binwidth * stats::dnorm(xgrid, xmean, xsd))
        p <- p + ggplot2::geom_line(ggplot2::aes(df$x, df$y), col = "#ff8a62")
      }
      if (add.kde) {
        kde <- stats::density(x, bw = h, from = xgrid[1], to = xgrid[512], n = 512)
        p <- p + ggplot2::geom_line(ggplot2::aes(x = kde$x, y = length(x) * binwidth * kde$y), col = "#67a9ff")
      }
    }
    if (add.rug) {
      p <- p + ggplot2::geom_rug(ggplot2::aes(x))
    }
    return(p)
  }
}

# Functions to plot the roots of an ARIMA model

# Compute AR roots
arroots <- function(object) {
  if (!any(is.element(class(object), c("Arima", "ar")))) {
    stop("object must be of class Arima or ar")
  }
  if (is.element("Arima", class(object))) {
    parvec <- object$model$phi
  } else {
    parvec <- object$ar
  }
  if (length(parvec) > 0) {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0) {
      return(structure(list(
        roots = polyroot(c(1, -parvec[1:last.nonzero])),
        type = "AR"
      ), class = "armaroots"))
    }
  }
  return(structure(list(roots = numeric(0), type = "AR"), class = "armaroots"))
}

# Compute MA roots
maroots <- function(object) {
  if (!is.element("Arima", class(object))) {
    stop("object must be of class Arima")
  }
  parvec <- object$model$theta
  if (length(parvec) > 0) {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0) {
      return(structure(list(
        roots = polyroot(c(1, parvec[1:last.nonzero])),
        type = "MA"
      ), class = "armaroots"))
    }
  }
  return(structure(list(roots = numeric(0), type = "MA"), class = "armaroots"))
}

plot.armaroots <- function(x, xlab, ylab, main, ...) {
  if (missing(main)) {
    main <- paste("Inverse", x$type, "roots")
  }
  oldpar <- par(pty = "s")
  on.exit(par(oldpar))
  plot(
    c(-1, 1), c(-1, 1),
    xlab = xlab, ylab = ylab,
    type = "n", bty = "n", xaxt = "n", yaxt = "n", main = main, ...
  )
  axis(1, at = c(-1, 0, 1), line = 0.5, tck = -0.025)
  axis(2, at = c(-1, 0, 1), labels = c("-i", "0", "i"), line = 0.5, tck = -0.025)
  circx <- seq(-1, 1, length.out = 501)
  circy <- sqrt(1 - circx^2)
  lines(c(circx, circx), c(circy, -circy), col = "gray")
  lines(c(-2, 2), c(0, 0), col = "gray")
  lines(c(0, 0), c(-2, 2), col = "gray")
  if (length(x$roots) > 0) {
    inside <- abs(x$roots) > 1
    points(1 / x$roots[inside], pch = 19, col = "black")
    if (sum(!inside) > 0) {
      points(1 / x$roots[!inside], pch = 19, col = "red")
    }
  }
}


#' Plot characteristic roots from ARIMA model
#'
#' Produces a plot of the inverse AR and MA roots of an ARIMA model. Inverse
#' roots outside the unit circle are shown in red.
#'
#' \code{autoplot} will produce an equivalent plot as a ggplot object.
#'
#' @param x Object of class \dQuote{Arima} or \dQuote{ar}.
#' @param object Object of class \dQuote{Arima} or \dQuote{ar}. Used for ggplot
#' graphics (S3 method consistency).
#' @param type Determines if both AR and MA roots are plotted, of if just one
#' set is plotted.
#' @param main Main title. Default is "Inverse AR roots" or "Inverse MA roots".
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param ... Other plotting parameters passed to \code{\link[graphics]{par}}.
#' @return None. Function produces a plot
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso \code{\link{Arima}}, \code{\link[stats]{ar}}
#' @keywords hplot
#' @examples
#'
#' library(ggplot2)
#'
#' fit <- Arima(WWWusage, order = c(3, 1, 0))
#' plot(fit)
#' autoplot(fit)
#'
#' fit <- Arima(woolyrnq, order = c(2, 0, 0), seasonal = c(2, 1, 1))
#' plot(fit)
#' autoplot(fit)
#'
#' plot(ar.ols(gold[1:61]))
#' autoplot(ar.ols(gold[1:61]))
#' @export
plot.Arima <- function(x, type = c("both", "ar", "ma"), main,
                       xlab = "Real", ylab = "Imaginary", ...) {
  type <- match.arg(type)
  if (!is.element("Arima", class(x))) {
    stop("This function is for objects of class 'Arima'.")
  }

  q <- p <- 0
  # AR component
  if (length(x$model$phi) > 0) {
    test <- abs(x$model$phi) > 1e-09
    if (any(test)) {
      p <- max(which(test))
    }
  }
  # MA component
  if (length(x$model$theta) > 0) {
    test <- abs(x$model$theta) > 1e-09
    if (any(test)) {
      q <- max(which(test))
    }
  }

  # Check for MA parts
  if (type == "both") {
    if (p == 0) {
      type <- "ma"
    } else if (q == 0) {
      type <- "ar"
    }
  }
  if ((type == "ar" && (p == 0)) || (type == "ma" && (q == 0)) || (p == 0 && q == 0)) {
    warning("No roots to plot")
    if (missing(main)) {
      main <- "No AR or MA roots"
    }
  }

  if (type == "both") {
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
  }

  if (type != "ma") {
    plot(arroots(x), main = main, xlab = xlab, ylab = ylab, ...)
  }
  if (type != "ar") {
    plot(maroots(x), main = main, xlab = xlab, ylab = ylab, ...)
  }
}

#' @rdname plot.Arima
#' @export
plot.ar <- function(x, main, xlab = "Real", ylab = "Imaginary", ...) {
  if (!is.element("ar", class(x))) {
    stop("This function is for objects of class 'ar'.")
  }
  plot(arroots(x), main = main, xlab = xlab, ylab = ylab, ...)
}

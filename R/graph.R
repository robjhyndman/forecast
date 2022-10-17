### Time series graphics and transformations


#' Time series display
#'
#' Plots a time series along with its acf and either its pacf, lagged
#' scatterplot or spectrum.
#'
#' \code{ggtsdisplay} will produce the equivalent plot using ggplot graphics.
#'
#' @param x a numeric vector or time series of class \code{ts}.
#' @param plot.type type of plot to include in lower right corner.
#' @param points logical flag indicating whether to show the individual points
#' or not in the time plot.
#' @param smooth logical flag indicating whether to show a smooth loess curve
#' superimposed on the time plot.
#' @param ci.type type of confidence limits for ACF that is passed to
#' \code{\link[stats]{acf}}. Should the confidence limits assume a white noise
#' input or for lag \eqn{k} an MA(\eqn{k-1}) input?
#' @param lag.max the maximum lag to plot for the acf and pacf. A suitable
#' value is selected by default if the argument is missing.
#' @param na.action function to handle missing values in acf, pacf and spectrum
#' calculations. The default is \code{\link[stats]{na.contiguous}}. Useful
#' alternatives are \code{\link[stats]{na.pass}} and \code{\link{na.interp}}.
#' @param theme Adds a ggplot element to each plot, typically a theme.
#' @param main Main title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param pch Plotting character.
#' @param cex Character size.
#' @param \dots additional arguments to \code{\link[stats]{acf}}.
#' @return None.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{plot.ts}}, \code{\link{Acf}},
#' \code{\link[stats]{spec.ar}}
#' @references Hyndman and Athanasopoulos (2018) \emph{Forecasting: principles
#' and practice}, 2nd edition, OTexts: Melbourne, Australia.
#' \url{https://otexts.com/fpp2/}
#' @keywords ts
#' @examples
#' tsdisplay(diff(WWWusage))
#' ggtsdisplay(USAccDeaths, plot.type="scatter")
#'
#' @export
tsdisplay <- function(x, plot.type=c("partial", "histogram", "scatter", "spectrum"), points=TRUE, ci.type=c("white", "ma"),
                      lag.max, na.action=na.contiguous, main=NULL, xlab="", ylab="",
                      pch=1, cex=0.5, ...) {
  plot.type <- match.arg(plot.type)
  ci.type <- match.arg(ci.type)

  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  nf <- layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

  if (is.null(main)) {
    main <- deparse(substitute(x))
  }
  if (!is.ts(x)) {
    x <- ts(x)
  }
  if (missing(lag.max)) {
    lag.max <- round(min(max(10 * log10(length(x)), 3 * frequency(x)), length(x) / 3))
  }

  plot.ts(x, main = main, ylab = ylab, xlab = xlab, ylim = range(x, na.rm = TRUE), ...)
  if (points) {
    points(x, pch = pch, cex = cex, ...)
  }
  ylim <- c(-1, 1) * 3 / sqrt(length(x))

  junk1 <- stats::acf(c(x), lag.max = lag.max, plot = FALSE, na.action = na.action)
  junk1$acf[1, 1, 1] <- 0
  if (ci.type == "ma") {
    ylim <- range(ylim, 0.66 * ylim * max(sqrt(cumsum(c(1, 2 * junk1$acf[-1, 1, 1] ^ 2)))))
  }
  ylim <- range(ylim, junk1$acf)
  if (plot.type == "partial") {
    junk2 <- stats::pacf(c(x), lag.max = lag.max, plot = FALSE, na.action = na.action)
    ylim <- range(ylim, junk2$acf)
  }

  oldpar <- par(mar = c(5, 4.1, 1.5, 2))
  plot(junk1, ylim = ylim, xlim = c(1, lag.max), ylab = "ACF", main = "", ci.type = ci.type, ...)
  if (plot.type == "scatter") {
    n <- length(x)
    plot(x[1:(n - 1)], x[2:n], xlab = expression(Y[t - 1]), ylab = expression(Y[t]), ...)
  }
  else if (plot.type == "spectrum") {
    spec.ar(x, main = "", na.action = na.action)
  } else if (plot.type == "histogram") {
    graphics::hist(x, breaks = "FD", main = "", xlab = main)
  }
  else {
    plot(junk2, ylim = ylim, xlim = c(1, lag.max), ylab = "PACF", main = "", ...)
  }
  par(def.par)
  layout(1)
  invisible()
}



#' Seasonal plot
#'
#' Plots a seasonal plot as described in Hyndman and Athanasopoulos (2014,
#' chapter 2). This is like a time plot except that the data are plotted
#' against the seasons in separate years.
#'
#' @param x a numeric vector or time series of class \code{ts}.
#' @param s seasonal frequency of x
#' @param season.labels Labels for each season in the "year"
#' @param year.labels Logical flag indicating whether labels for each year of
#' data should be plotted on the right.
#' @param year.labels.left Logical flag indicating whether labels for each year
#' of data should be plotted on the left.
#' @param type plot type (as for \code{\link[graphics]{plot}}). Not yet
#' supported for ggseasonplot.
#' @param main Main title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param col Colour
#' @param labelgap Distance between year labels and plotted lines
#' @param \dots additional arguments to \code{\link[graphics]{plot}}.
#' @return None.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso \code{\link[stats]{monthplot}}
#' @references Hyndman and Athanasopoulos (2018) \emph{Forecasting: principles
#' and practice}, 2nd edition, OTexts: Melbourne, Australia.
#' \url{https://otexts.com/fpp2/}
#' @keywords ts
#' @examples
#' seasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)
#'
#' @export
seasonplot <- function(x, s, season.labels=NULL, year.labels=FALSE, year.labels.left=FALSE,
                       type="o", main, xlab=NULL, ylab="", col=1, labelgap=0.1, ...) {
  if (missing(main)) {
    main <- paste("Seasonal plot:", deparse(substitute(x)))
  }

  # Check data are seasonal and convert to integer seasonality
  if (missing(s)) {
    s <- round(frequency(x))
  }
  if (s <= 1) {
    stop("Data are not seasonal")
  }

  tspx <- tsp(x)
  x <- ts(x, start = tspx[1], frequency = s)

  # Pad series
  tsx <- x
  startperiod <- round(cycle(x)[1])
  if (startperiod > 1) {
    x <- c(rep(NA, startperiod - 1), x)
  }
  x <- c(x, rep(NA, s - length(x) %% s))
  Season <- rep(c(1:s, NA), length(x) / s)
  xnew <- rep(NA, length(x))
  xnew[!is.na(Season)] <- x

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
    if (s < 20) {
      labs <- 1:s
    } else {
      labs <- NULL
    }
    xLab <- "Season"
  }

  if (is.null(xlab)) {
    xlab <- xLab
  }
  if (is.null(season.labels)) {
    season.labels <- labs
  }
  if (year.labels) {
    xlim <- c(1 - labelgap, s + 0.4 + labelgap)
  } else {
    xlim <- c(1 - labelgap, s)
  }
  if (year.labels.left) {
    xlim[1] <- 0.4 - labelgap
  }
  plot(Season, xnew, xaxt = "n", xlab = xlab, type = type, ylab = ylab, main = main, xlim = xlim, col = 0, ...)
  nn <- length(Season) / s
  col <- rep(col, nn)[1:nn]
  for (i in 0:(nn - 1))
    lines(Season[(i * (s + 1) + 1):((s + 1) * (i + 1))], xnew[(i * (s + 1) + 1):((s + 1) * (i + 1))], type = type, col = col[i + 1], ...)
  if (year.labels) {
    idx <- which(Season[!is.na(xnew)] == s)
    year <- round(time(tsx)[idx], nchar(s))
    text(x = rep(s + labelgap, length(year)), y = tsx[idx], labels = paste(c(trunc(year))), adj = 0, ..., col = col[1:length(idx)])
  }
  if (year.labels.left) {
    idx <- which(Season[!is.na(xnew)] == 1)
    year <- round(time(tsx)[idx], nchar(s))
    if (min(idx) > 1) { # First year starts after season 1n
      col <- col[-1]
    }
    text(x = rep(1 - labelgap, length(year)), y = tsx[idx], labels = paste(c(trunc(year))), adj = 1, ..., col = col[1:length(idx)])
  }
  if (is.null(labs)) {
    axis(1, ...)
  } else {
    axis(1, labels = season.labels, at = 1:s, ...)
  }
}

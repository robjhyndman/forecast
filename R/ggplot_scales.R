#' Level colour scales
#'
#' This set of scales defines new scales for level geoms equivalent to the
#' ones already defined by ggplot2. This allows the shade of confidence intervals
#' to work with the legend output.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_level_*
#'
#' @name scale_level
#' @rdname scale_level
#'
NULL

#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_gradient
#' @param low,high Colours for low and high ends of the gradient.
#'
#' @export
scale_level_gradient <- function(..., low = "#888888", high = "#BBBBBB", space = "Lab", na.value = NA, guide = "level_colourbar") {
  ggplot2::continuous_scale("level", "gradient", scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}
#' @rdname scale_level
#'
#' @export
scale_level_continuous <- scale_level_gradient

#' Level shade bar guide
#' 
#' The level colourbar shows the colour from the forecast intervals which is blended with the series colour.
#' 
#' @inheritParams ggplot2::guide_colourbar
#' 
#' @rdname guide_level_colourbar
#' 
#' @export
guide_level_colourbar <- function(title = ggplot2::waiver(), title.position = NULL,
                                 title.theme = NULL, title.hjust = NULL,
                                 title.vjust = NULL, label = TRUE,
                                 label.position = NULL, label.theme = NULL,
                                 label.hjust = NULL, label.vjust = NULL,
                                 barwidth = NULL, barheight = NULL, nbin = 20,
                                 raster = TRUE, ticks = TRUE, draw.ulim = TRUE,
                                 draw.llim = TRUE, direction = NULL,
                                 default.unit = "line", reverse = FALSE, order = 0,
                                 ...) {
  if (!is.null(barwidth) && !grid::is.unit(barwidth))
    barwidth <- grid::unit(barwidth, default.unit)
  if (!is.null(barheight) && !grid::is.unit(barheight))
    barheight <- grid::unit(barheight, default.unit)
  structure(
    list(title = title, title.position = title.position,
         title.theme = title.theme, title.hjust = title.hjust,
         title.vjust = title.vjust, label = label,
         label.position = label.position, label.theme = label.theme,
         label.hjust = label.hjust, label.vjust = label.vjust,
         barwidth = barwidth, barheight = barheight, nbin = nbin,
         raster = raster, ticks = ticks, draw.ulim = draw.ulim,
         draw.llim = draw.llim, direction = direction,
         default.unit = default.unit, reverse = reverse, order = order,
         available_aes = c("level"), ..., name = "level_colourbar"),
    class = c("guide", "level_colourbar", "colorbar")
  )
}

#' @rdname guide_level_colourbar
#' @export
guide_level_colorbar <- guide_level_colourbar

#' Helper methods for guides
#'
#' @export
#' @rdname guide-helpers
#' @keywords internal
guide_train.level_colourbar <- function(guide, scale) {
  if (length(intersect(scale$aesthetics, c("level"))) == 0) {
    warning("level_colourbar guide needs level scales.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("level_colourbar guide needs continuous scales.")
    return(NULL)
  }
  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()
  ticks <- as.data.frame(stats::setNames(list(scale$map(breaks)),
                                  scale$aesthetics[1]))
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks)
  guide$key <- ticks
  .limits <- scale$get_limits()
  .bar <- scales::discard(pretty(.limits, n = guide$nbin), scale$get_limits())
  if (length(.bar) == 0) {
    .bar = unique(.limits)
  }
  guide$bar <- data.frame(colour = scale$map(.bar), value = .bar,
                          stringsAsFactors = FALSE)
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, digest::digest(list(title, key$.label,
                                                bar, name)))
  guide
}
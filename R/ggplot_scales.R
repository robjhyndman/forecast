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
scale_level_gradient <- function(..., low = "#888888", high = "#BBBBBB", space = "Lab", na.value = NA, guide = "level") {
  level_scale("level", "gradient", scales::seq_gradient_pal(low, high, space),
              na.value = na.value, guide = guide, ...)
}
#' @rdname scale_level
#'
#' @export
scale_level_continuous <- scale_level_gradient

ScaleLevel <- ggplot2::ggproto(NULL, ggplot2::ScaleContinuous)

#' @importFrom ggplot2 waiver
level_scale <- function (...) 
{
  scale <- ggplot2::continuous_scale(..., super = ScaleLevel)
  scale$range <- level_range()
  scale
}

#' Level shade bar guide
#' 
#' The level guide shows the colour from the forecast intervals which is blended with the series colour.
#' 
#' @inheritParams ggplot2::guide_colourbar
#' @param max_discrete The maximum number of levels to be shown using \code{\link[ggplot2]{guide_legend}}.
#' If the number of levels exceeds this value, level shades are shown with \code{\link[ggplot2]{guide_colourbar}}.
#' @param ... Further arguments passed onto either \code{\link[ggplot2]{guide_colourbar}} or \code{\link[ggplot2]{guide_legend}}
#' 
#' @export
guide_level <- function(title = waiver(), max_discrete = 5, ...) {
  structure(list(title = title,
                 max_discrete = max_discrete,
                 available_aes = "level",
                 args = list(...)),
            class=c("guide", "level_guide"))
}

#' Helper methods for guides
#'
#' @export
#' @rdname guide-helpers
#' @keywords internal
guide_train.level_guide <- function(guide, scale, aesthetic) {
  args <- append(guide[!(names(guide)%in%c("max_discrete", "args"))], guide$args)
  levels <- scale$range$levels
  if (length(levels) == 0 || all(is.na(levels))) 
    return()
  if(length(levels)<=guide$max_discrete){
    guide <- do.call("guide_legend", args)
    class(guide) <- c("guide", "guide_level")
    breaks <- levels
    
    key <- as.data.frame(stats::setNames(list(scale$map(breaks)), aesthetic %||% 
                                    scale$aesthetics[1]), stringsAsFactors = FALSE)
    key$.label <- scale$get_labels(breaks)
    if (!scale$is_discrete()) {
      limits <- scale$get_limits()
      noob <- !is.na(breaks) & limits[1] <= breaks & breaks <= 
        limits[2]
      key <- key[noob, , drop = FALSE]
    }
    if (guide$reverse) 
      key <- key[nrow(key):1, ]
    guide$key <- key
    guide$hash <- with(guide, digest::digest(list(title, key$.label, 
                                                  direction, name)))
  }
  else{
    guide <- do.call("guide_colourbar", args)
    breaks <- scale$get_breaks()
    ticks <- as.data.frame(setNames(list(scale$map(breaks)), 
                                    aesthetic %||% scale$aesthetics[1]))
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)
    guide$key <- ticks
    .limits <- scale$get_limits()
    .bar <- seq(.limits[1], .limits[2], length = guide$nbin)
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
  }
  guide
}

#' @export
#' @rdname guide-helpers
guide_geom.guide_level <- function (guide, layers, default_mapping) 
{
  class(guide) <- c("guide", "legend")
  guide <- guide_geom(guide, layers, default_mapping)
  guide$geoms <- lapply(guide$geoms, function(x){
    x$draw_key <- ggplot2::ggproto(NULL,NULL,
                          draw_key = function(data, params, size){
                            lwd <- min(data$size, min(size) / 4)
                            fillcol <- data$level #blendHex(data$col, data$level, 0.7) 
                            grid::rectGrob( 
                              width = grid::unit(1, "npc") - grid::unit(lwd, "mm"), 
                              height = grid::unit(1, "npc") - grid::unit(lwd, "mm"), 
                              gp = grid::gpar( 
                                col = fillcol, 
                                fill = scales::alpha(fillcol, data$alpha), 
                                lty = data$linetype, 
                                lwd = lwd * ggplot2::.pt, 
                                linejoin = "mitre" 
                              )
                            )
                          })$draw_key
    x
  })
  guide
}
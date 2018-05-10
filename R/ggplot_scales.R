#' Edge colour scales
#'
#' This set of scales defines new colour scales for edge geoms equivalent to the
#' ones already defined by ggplot2. The parameters are equivalent to the ones
#' from ggplot2 so there is nothing new under the sun. The different geoms will
#' know whether to use edge scales or the standard scales so it is not necessary
#' to write `level` in the call to the geom - just use `colour`.
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @family scale_edge_*
#'
#' @name scale_level
#' @rdname scale_level
#'
NULL

#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_hue
#'
#' @importFrom scales hue_pal
#' @export
scale_level_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50") {
  discrete_scale("level", "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...)
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_brewer
#'
#' @importFrom scales brewer_pal
#' @export
scale_level_brewer <- function(..., type = "seq", palette = 1, direction = 1) {
  discrete_scale("level", "brewer", brewer_pal(type, palette, direction), ...)
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_distiller
#'
#' @importFrom scales gradient_n_pal brewer_pal
#' @export
scale_level_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "level_colourbar") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  continuous_scale("level", "distiller",
                   gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, guide = guide, ...)
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}

#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @importFrom scales div_gradient_pal muted
#' @export
scale_level_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", guide = "level_colourbar") {
  continuous_scale("level", "gradient2",
                   div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler =
                     function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
                       scales::rescale_mid(x, to, from, midpoint)
                     }
  )
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_gradientn
#' @param colours,colors Vector of colours to use for n-colour gradient.
#'
#' @importFrom scales gradient_n_pal
#' @export
scale_level_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", guide = "level_colourbar", colors) {
  colours <- if (missing(colours)) colors else colours
  
  continuous_scale("level", "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_grey
#'
#' @importFrom scales grey_pal
#' @export
scale_level_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
  discrete_scale("level", "grey", grey_pal(start, end),
                 na.value = na.value, ...)
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_identity
#'
#' @importFrom scales identity_pal
#' @export
scale_level_identity <- function(..., guide = "none") {
  sc <- discrete_scale("level", "identity", identity_pal(), ...,
                       guide = guide, super = ScaleDiscreteIdentity)
  sc
}
#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_manual
#'
#' @export
scale_level_manual <- function(..., values) {
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n,
           " needed but only ", length(values), " provided.",
           call. = FALSE)
    }
    values
  }
  discrete_scale("level", "manual", pal, ...)
}

#' @rdname scale_level
#'
#' @inheritParams ggplot2::scale_colour_gradient
#' @param low,high Colours for low and high ends of the gradient.
#'
#' @importFrom scales seq_gradient_pal
#' @export
scale_level_gradient <- function(..., low = "#888888", high = "#BBBBBB", space = "Lab", na.value = NA, guide = "level_colourbar") {
  continuous_scale("level", "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}
#' @rdname scale_level
#'
#' @export
scale_level_continuous <- scale_level_gradient
#' @rdname scale_level

#' @export
scale_level_discrete <- scale_level_hue

# level_scale <- function (aesthetics, scale_name, palette, name = waiver(), 
#           breaks = waiver(), minor_breaks = waiver(), labels = waiver(), 
#           limits = NULL, rescaler = rescale, oob = censor, expand = waiver(), 
#           na.value = NA_real_, trans = "identity", guide = "legend", 
#           position = "left", super = ScaleContinuous){
#   check_breaks_labels(breaks, labels)
#   position <- match.arg(position, c("left", "right", "top", 
#                                     "bottom"))
#   if (is.null(breaks) && !is_position_aes(aesthetics) && guide != 
#       "none") {
#     guide <- "none"
#   }
#   trans <- as.trans(trans)
#   if (!is.null(limits)) {
#     limits <- trans$transform(limits)
#   }
#   ggproto(NULL, super, call = match.call(), aesthetics = aesthetics, 
#           scale_name = scale_name, palette = palette, range = level_range(), 
#           limits = limits, trans = trans, na.value = na.value, 
#           expand = expand, rescaler = rescaler, oob = oob, name = name, 
#           breaks = breaks, minor_breaks = minor_breaks, labels = labels, 
#           guide = guide, position = position)
# }
# 
# level_range <- ggplot2::ggproto(NULL, ggplot2::RangeContinuous,
#                                 train = function(self, x){
#                                   train_level <- function(new, existing = NULL){
#                                     if (is.null(new)){
#                                       return(existing)
#                                     }
#                                     suppressWarnings(range(existing, new, na.rm = TRUE, finite = TRUE))
#                                   }
#                                   self$range <- train_level(x, self$range)
#                                 })


#' @export
guide_level_colourbar <- function(title = waiver(), title.position = NULL,
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
#' @export
guide_level_colorbar <- guide_level_colourbar

#' @export
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
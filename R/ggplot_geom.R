GeomForecast2 <- ggplot2::ggproto("GeomForecast2", ggplot2::Geom,
                         required_aes = c("x", "y"),
                         optional_aes = c("ymin", "ymax", "level"),
                         default_aes = ggplot2::aes(
                           colour = "blue", fill = "grey60", size = .5,
                           linetype = 1, weight = 1, alpha = 1, level = NA
                         ),
                         handle_na = function(self, data, params) {
                           # TODO, add smart NA handler.
                           data
                         },
                         
                         draw_panel = function(data, panel_scales, coord) {
                           line_data <- data[is.na(data$level),]
                           interval_data <- data[!is.na(data$level),]
                           # Intervals have been provided
                           if(NROW(interval_data) > 0){
                             # Calculate colour
                             interval_data$fillcol <- blendHex(interval_data$colour, interval_data$level, 0.7)
                             # Compute alpha transparency
                             interval_data$alpha <- grDevices::col2rgb(interval_data$fillcol, alpha = TRUE)[4, ] / 255 * interval_data$alpha
                             GrobList <- lapply(
                               split(interval_data, interval_data$level),
                               function(x){
                                 # Select appropriate Geom and set defaults
                                 if (NROW(x) == 1) { # Linerange
                                   GeomForecastIntervalGeom <- ggplot2::GeomLinerange$draw_panel
                                   x <- transform(x, colour = fillcol, fill = NA, size = size*2)
                                 }
                                 else { # Ribbon
                                   GeomForecastIntervalGeom <- ggplot2::GeomRibbon$draw_group
                                   x <- transform(x, colour = NA, fill = fillcol)
                                 }
                                 return(list(
                                   grob = GeomForecastIntervalGeom(x, panel_scales, coord),
                                   range1 = x[1,"ymax"] - x[1,"ymin"]
                                 )) ## Create list pair with average ymin/ymax to order layers
                               }
                             )
                             # Sort GrobList
                             GrobList <- lapply(GrobList, function(x) x[["grob"]])[order(vapply(GrobList, FUN=function(x) x[["range1"]], FUN.VALUE=numeric(1)), decreasing = TRUE)]
                           }
                           else{
                             GrobList <- list()
                           }
                           if(NROW(line_data) > 0){
                             # Calculate colour
                             line_data$colour <- blendHex(line_data$colour, "gray30", 1)
                             if (NROW(line_data) == 1) { # Point
                               GeomForecastPointGeom <- ggplot2::GeomPoint$draw_panel
                               pointpred <- transform(line_data, fill = NA, size = size*2)
                             }
                             else { # Line
                               GeomForecastPointGeom <- ggplot2::GeomLine$draw_panel
                               pointpred <- transform(line_data, fill = NA)
                             }
                             GrobList <- append(GrobList, list(GeomForecastPointGeom(pointpred, panel_scales, coord)))
                           }
                           ggplot2:::ggname("geom_forecast2", do.call(grid::grobTree, GrobList))
                         }
)

geom_forecast2 <- function(mapping = NULL, data = NULL, stat = "forecast",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, PI=TRUE, showgap=TRUE, series=NULL, ...) {
  
  if (is.forecast(mapping) || is.mforecast(mapping)) {
    warning("Use autolayer instead of geom_forecast to add a forecast layer to your ggplot object.")
    cl <- match.call()
    cl[[1]] <- quote(autolayer)
    names(cl)[names(cl) == "mapping"] <- "object"
    return(eval.parent(cl))
  }
  if (is.ts(mapping)) {
    data <- data.frame(y = as.numeric(mapping), x = as.numeric(time(mapping)))
    mapping <- ggplot2::aes_(y = ~y, x = ~x)
  }
  if (stat == "forecast") {
    paramlist <- list(na.rm = na.rm, PI = PI, showgap = showgap, series = series, ...)
    if (!inherits(mapping, "uneval")) {
      mapping <- ggplot2::aes_()
    }
    if (!is.null(series)) {
      mapping$colour <- quote(..series..)
    }
    if (PI) {
      mapping$level <- quote(..level..)
    }
  }
  else {
    paramlist <- list(na.rm = na.rm, ...)
  }
  layer(
    geom = GeomForecast2, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = paramlist
  )
}
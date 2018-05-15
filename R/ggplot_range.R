level_range <- function(){
  ggplot2::ggproto(NULL, RangeLevel)
}

RangeLevel <- ggplot2::ggproto(NULL, NULL,
                               range = NULL,
                               reset = function(self){
                                 self$range <- NULL
                               },
                               train = function(self, x){
                                 browser()
                               }
)
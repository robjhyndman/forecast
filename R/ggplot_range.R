level_range <- function(){
  ggplot2::ggproto(NULL, RangeLevel)
}

RangeLevel <- ggplot2::ggproto(NULL, NULL,
                               range = NULL,
                               levels = NULL,
                               reset = function(self){
                                 self$range <- NULL
                                 self$levels <- NULL
                               },
                               train = function(self, x){
                                 self$range <- scales::train_continuous(x, self$range)
                                 self$levels <- unique(c(x[!is.na(x)],self$range))
                               }
)
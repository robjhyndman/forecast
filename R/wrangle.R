toMat <- function(x) {
  if (NCOL(x) > 1 && !is.matrix(x)) {
    x <- matrix(x, ncol = NCOL(x))
  }
  x
}

# Converts arguments into data.frame, whilst retaining mts/ts/matrix properties
datamat <- function(..., flatten = TRUE, functions = TRUE) {
  vars <- list(...)
  if (length(vars) == 0) {
    return(data.frame())
  }
  if (!is.null(names(vars))) {
    names(vars)[!nzchar(names(vars))] <- as.character(substitute(list(...))[
      -1
    ])[!nzchar(names(vars))]
  } else {
    names(vars) <- as.character(substitute(list(...))[-1])
  }
  if (flatten) {
    i <- 1
    while (i <= length(vars)) {
      if (is.data.frame(vars[[i]])) {
        vars <- c(vars, c(vars[[i]])) # Append data.frame components
        vars[[i]] <- NULL # Remove data.frame
      } else if (is.matrix(vars[[i]])) {
        for (j in seq_len(NCOL(vars[[i]]))) {
          vars[[length(vars) + 1]] <- vars[[i]][, j]
          names(vars)[length(vars)] <- make.names(colnames(vars[[i]])[j])
        }
        i <- i + 1
      } else {
        i <- i + 1
      }
    }
  }
  class(vars) <- "data.frame"
  row.names(vars) <- 1:max(vapply(vars, NROW, integer(1)))
  #   if(is.ts(vars[,1])){
  #     if(NCOL(vars)>1){
  #       class(vars) <- c(class(vars),"mts")
  #     }
  #     class(vars) <- c(class(vars),"ts")
  #     tspx <- unique(sapply(vars,tsp), MARGIN = 2)
  #     if(length(tspx)==3){
  #       attr(vars, "tsp") <- tspx
  #     }
  #   }
  vars
}

recoverTSP <- function(times.x) {
  freq <- sort(unique(round(times.x %% 1, digits = 6))) # The subset cannot increase frequency
  freq <- length(freq)
  c(min(times.x), min(times.x) + (length(times.x) - 1) / freq, freq)
}

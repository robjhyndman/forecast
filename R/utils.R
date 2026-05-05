getConfLevel <- function(level, fan) {
  if (fan) {
    seq(51, 99, by = 3)
  } else if (min(level) > 0 && max(level) < 1) {
    100 * level
  } else if (min(level) < 0 || max(level) > 99.99) {
    stop("Confidence limit out of range")
  } else {
    level
  }
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x) && x > 0
}

get_seed <- function() {
  seed <- get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(seed)) {
    runif(1L)
    seed <- get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }
  seed
}

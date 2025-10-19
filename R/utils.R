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

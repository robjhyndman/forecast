`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x) && x > 0
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

seasonal_period <- function(x) {
  max(1L, round(frequency(x)))
}

get_conf_level <- function(level, fan) {
  if (fan) {
    seq(51, 99, by = 3)
  } else if (min(level) > 0 && max(level) < 1) {
    sort(100 * level)
  } else if (min(level) < 0 || max(level) > 99.99) {
    stop("`level` must be between 0 and 99.99.")
  } else {
    sort(level)
  }
}

normal_intervals <- function(mean, se, level) {
  margin <- outer(as.numeric(se), qnorm(0.5 + level / 200))
  colnames(margin) <- paste0(level, "%")
  mean <- as.numeric(mean)
  list(lower = mean - margin, upper = mean + margin)
}

get_seed <- function() {
  seed <- get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(seed)) {
    runif(1L)
    seed <- get0(
      ".Random.seed",
      globalenv(),
      mode = "integer",
      inherits = FALSE
    )
  }
  seed
}

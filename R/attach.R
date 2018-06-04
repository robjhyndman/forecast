.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.2) return()

  tips <- c(
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Stackoverflow is a great place to get help on R issues:\n  http://stackoverflow.com/tags/forecasting+r.",
    "Crossvalidated is a great place to get help on forecasting issues:\n  http://stats.stackexchange.com/tags/forecasting.",
    "Need help getting started? Try the online textbook FPP:\n  http://OTexts.org/fpp2/",
    "Want to stay up-to-date? Read the Hyndsight blog:\n  https://robjhyndman.com/hyndsight/",
    "Want to meet other forecasters? Join the International Institute of Forecasters:\n  http://forecasters.org/"
  )
  tip <- sample(tips, 1)
  msg <- paste("This is forecast", packageVersion("forecast"), "\n ", tip)
  packageStartupMessage(msg)
}


# Code from dtplyr: https://github.com/hadley/dtplyr/blob/master/R/compat-dplyr-0.6.0.R
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  envir <- asNamespace(pkg)
  
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))
  
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}

.onLoad <- function(...) {
  ns <- getNamespace("ggplot2")
  if (exists("autolayer", ns)) {
    autolayer <<- ns$autolayer
    register_s3_method("ggplot2", "autolayer", "ts")
    register_s3_method("ggplot2", "autolayer", "mts")
    register_s3_method("ggplot2", "autolayer", "msts")
    register_s3_method("ggplot2", "autolayer", "forecast")
    register_s3_method("ggplot2", "autolayer", "mforecast")
  }
  invisible()
}
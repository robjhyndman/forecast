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

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

overwrite_s3_generic <- function(pkg, generic){
  if (pkg %in% loadedNamespaces()) {
    assign(generic, get(generic, asNamespace(pkg)), envir = asNamespace("forecast"))
  }

  # Always register hook in case package is later unloaded & reloaded
  # setHook(
  #   packageEvent(pkg, "onLoad"),
  #   function(...) {
  #     pkg_env <- asNamespace("forecast")
  #     unlockBinding(generic, pkg_env)
  #     assign(generic, get(generic, asNamespace(pkg)), envir = pkg_env)
  #     lockBinding(generic, pkg_env)
  #   }
  # )
}

#' @importFrom utils methods
.onLoad <- function(...) {
  overwrite_s3_generic("ggplot2", "autolayer")
  register_s3_method("ggplot2", "autolayer", "ts")
  register_s3_method("ggplot2", "autolayer", "mts")
  register_s3_method("ggplot2", "autolayer", "msts")
  register_s3_method("ggplot2", "autolayer", "forecast")
  register_s3_method("ggplot2", "autolayer", "mforecast")

  methods <- strsplit(utils::.S3methods(forecast), ".", fixed = TRUE)
  overwrite_s3_generic("fabletools", "forecast")
  for(method in methods){
    register_s3_method("fabletools", method[1], method[2])
  }

  methods <- strsplit(utils::.S3methods(accuracy), ".", fixed = TRUE)
  overwrite_s3_generic("fabletools", "accuracy")
  for(method in methods){
    register_s3_method("fabletools", method[1], method[2])
  }
  invisible()
}

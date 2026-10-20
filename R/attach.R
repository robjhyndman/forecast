.onAttach <- function(libname, pkgname) {
  if (!interactive() || withr::with_preserve_seed(stats::runif(1)) > 0.2) {
    return()
  }

  tips <- c(
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Stackoverflow is a great place to get help on R issues:\n  https://stackoverflow.com/tags/forecasting+r.",
    "Crossvalidated is a great place to get help on forecasting issues:\n  https://stats.stackexchange.com/tags/forecasting.",
    "Need help getting started? Try the online textbook FPP:\n  https://otexts.com/fpp2/",
    "Want to stay up-to-date? Read the Hyndsight blog:\n  https://robjhyndman.com/hyndsight/",
    "Want to meet other forecasters? Join the International Institute of Forecasters:\n  https://forecasters.org/"
  )
  tip <- withr::with_preserve_seed(sample(tips, 1))
  msg <- paste("This is forecast", packageVersion("forecast"), "\n ", tip)
  packageStartupMessage(msg)
}

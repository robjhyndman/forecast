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

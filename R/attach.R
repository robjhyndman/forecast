.onAttach <- function(...)
{
    version <- packageVersion("forecast")
    packageStartupMessage(paste("This is forecast",version,"\n"))
}

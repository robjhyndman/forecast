.onAttach <- function(...)
{
    library(help=forecast)$info[[1]] -> version
    version <- version[pmatch("Version",version)]
    um <- strsplit(version," ")[[1]]
    version <- um[nchar(um)>0][2]
    packageStartupMessage(paste("This is forecast",version,"\n"))
}

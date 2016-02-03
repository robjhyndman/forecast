toMat <- function(x){
  if(NCOL(x)>1 && !is.matrix(x)){
    x <- matrix(x,ncol=NCOL(x))
  }
  return(x)
}

#Converts arguments into data.frame, whilst retaining mts/ts/matrix properties
datamat <- function(..., flatten=TRUE, functions=TRUE){
  vars <- list(...)
  if(!is.null(names(vars))){
    names(vars)[!nzchar(names(vars))] <- as.character(substitute(list(...))[-1])[!nzchar(names(vars))]
  }
  else{
    names(vars) <- as.character(substitute(list(...))[-1])
  }
  if(flatten){
    i <- 1
    while (i <= length(vars)){
      if (is.data.frame(vars[[i]])){
        #Potentially check for matrix->data.frame structures
        vars <- c(vars,c(vars[[i]])) #Append data.frame components
        vars[[i]] <- NULL #Remove data.frame
      }
      else{
        i <- i + 1
      }
    }
  }
  if(functions){
    for(i in 1:length(names(vars))){
      term <- parse(text=names(vars)[i])[[1]]
      if(!is.symbol(term)){
        if(typeof(eval(term[[1]]))=="closure"){#If this term is a function (alike fourier)
          names(vars)[i] <- paste("FN.",term[[1]],"_",sep="")
        }
      }
    }
  }
  class(vars) <- "data.frame"
  row.names(vars) <- 1:max(sapply(vars, NROW))
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
  return(vars)
}

recoverTSP <- function(times.x){
  freq <- sort(unique(round(times.x%%1,digits=6))) #The subset cannot increase frequency
  freq <- length(freq)
  return(c(min(times.x),min(times.x)+(length(times.x)-1)/freq,freq))
}
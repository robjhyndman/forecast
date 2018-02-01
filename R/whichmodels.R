WhichModels <- function(max.p, max.q, max.P, max.Q, maxK) {
  total.models <- (max.p + 1) * (max.q + 1) * (max.P + 1) * (max.Q + 1) * length(0:maxK)
  x <- numeric(total.models)
  i <- 1

  for (x1 in 0:max.p) for (x2 in 0:max.q) {
      for (x3 in 0:max.P) for (x4 in 0:max.Q) {
          for (K in 0:maxK)
          {
            x[i] <- paste(x1, "f", x2, "f", x3, "f", x4, "f", K, sep = "")
            i <- i + 1
          }
        }
    }
  return(x)
}


UndoWhichModels <- function(n) {
  as.numeric(unlist(strsplit(n, split = "f")))
}

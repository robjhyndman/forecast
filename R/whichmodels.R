WhichModels <- function(max.p, max.q, max.P, max.Q, maxK) {
  grid <- expand.grid(0:maxK, 0:max.Q, 0:max.P, 0:max.q, 0:max.p, stringsAsFactors = FALSE)
  do.call(paste, c(rev(grid), sep = "f"))
}

UndoWhichModels <- function(n) {
  as.numeric(unlist(strsplit(n, split = "f", fixed = TRUE), use.names = FALSE))
}

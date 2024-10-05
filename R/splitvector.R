split.vector <- function(vector, groups) {
  max.range <- max(vector)
  nvals <- length(vector)

  size.main <- floor(max.range / groups)
  size.last <- max.range - (size.main * (groups - 1))
  lengths <- c(rep(size.main, groups - 1), size.last)

  group_labels <- rep(1:groups, lengths)

  result <- rep(NA, nvals)
  for (i in 1:groups) {
    result[group_labels == i] <- i
  }

  return(result)
}

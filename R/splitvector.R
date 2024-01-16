split.vector <- function(x, ranges){
  max.range <- max(x)
  nvals <- length(x)

  size.main <- floor(max.range/ranges)
  size.last <- max.range - sum(rep(size.main, ranges - 1))


  lengths <- c(rep(size.main, ranges - 1), size.last)

  splits <- split(x, rep(seq(ranges), lengths))

  for (i in seq(ranges-1)) splits[[as.character(i)]] <- rep(i, size.main)
  splits[[ranges]] <- rep(ranges, size.last)

  result <- append(splits[[1]], splits[[2]])
  for (i in 3:ranges) result <- append(result, splits[[i]])

  return(result)
}

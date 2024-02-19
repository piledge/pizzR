split.vector <- function (vector, groups) 
{
  max.range <- max(vector)
  nvals <- length(vector)
  size.main <- floor(max.range/groups)
  size.last <- max.range - sum(rep(size.main, groups - 1))
  lengths <- c(rep(size.main, groups - 1), size.last)
  splits <- split(vector, rep(seq(groups), lengths))
  for (i in seq(groups - 1)) splits[[as.character(i)]] <- rep(i, size.main)
  splits[[groups]] <- rep(groups, size.last)
  
  if (groups == 1){
    result <- splits[[1]]
  }else{
    result <- append(splits[[1]], splits[[2]])
    if (groups > 2) for (i in 3:groups) result <- append(result, splits[[i]])
  }
  return(result)
}

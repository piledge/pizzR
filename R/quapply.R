quapply <- function(x, qu){
  result <- matrix(unlist(lapply(x, quantile, probs = qu)), ncol=ncol(x), byrow = T)
  colnames(result) <- colnames(x)
  rownames(result) <- paste0(qu, 'qu')

  return(result)
}

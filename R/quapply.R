quapply <- function(x, qu){
  if (!is.data.frame(x)) return(warning("'x' has to be of class 'dataframe'"))
  if (!is.numeric(qu)) return(warning("'qu' has to be of class 'numeric'"))
  if (min(qu) < 0 || max(qu) > 1) return(warning("range of 'qu' has to be from 0 to 1"))

  result <- matrix(unlist(lapply(x, quantile, probs = qu)), ncol=ncol(x), byrow = T)
  colnames(result) <- colnames(x)
  rownames(result) <- paste0(qu, 'qu')

  return(result)
}

quapply <- function(x, qu){
  stopifnot(
    is.data.frame(x),
    is.numeric(qu),
    all(qu >= 0 & qu <= 1)
  )

  result <- matrix(unlist(lapply(x, quantile, probs = qu)), ncol=ncol(x), byrow = T)
  colnames(result) <- colnames(x)
  rownames(result) <- paste0(qu, 'qu')

  return(result)
}

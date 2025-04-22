quapply <- function(x, qu){
  lst <- if (is.data.frame(x)) x else list(x = x)
  q   <- sapply(lst, quantile, probs = qu)
  out <- t(as.matrix(q))
  rownames(out) <- sprintf("%03.0fqu", qu * 100)

  return(out)
}

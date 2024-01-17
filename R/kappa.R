kappa <- function(pred, ref) {
  accmat <- table("pred" = pred, "ref" = ref)
  N <- sum(accmat)
  No <- sum(diag(accmat))
  Ne <- 1 / N * sum(colSums(accmat) * rowSums(accmat))
  return( (No - Ne) / (N - Ne) )
}

cohens.kappa <- function(pred,ref) {

  if (!length(pred) == length(ref)) return(warning("Length of prediction and reference is not equal!"))

  accmat <- table("pred" = pred, "ref" = ref)
  N <- sum(accmat)
  No <- sum(diag(accmat))
  Ne <- 1 / N * sum(colSums(accmat) * rowSums(accmat))
  return( (No - Ne) / (N - Ne) )
}

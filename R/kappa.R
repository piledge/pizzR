cohens.kappa <- function(pred, ref) {

  stopifnot(length(pred) == length(ref), msg = "Length of prediction and reference is not equal!")

  accmat <- table("pred" = pred, "ref" = ref)
  N <- sum(accmat)
  No <- sum(diag(accmat))
  Ne <- sum(colSums(accmat) * rowSums(accmat)) / N

  return((No - Ne) / (N - Ne))
}

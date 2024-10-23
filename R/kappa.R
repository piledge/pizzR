cohens.kappa <- function(pred, ref) {

  stopifnot(length(pred) == length(ref))

  accmat <- table("pred" = pred, "ref" = ref)
  N <- sum(accmat)
  No <- sum(diag(accmat))
  Ne <- sum(colSums(accmat) * rowSums(accmat)) / N

  return((No - Ne) / (N - Ne))
}

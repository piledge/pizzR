confMatrix <- function(pred, ref){
  CM <- table("pred" = pred, "ref" = ref)
  UA <- round(diag(CM) / rowSums(CM) * 100, 1)
  PA <- round(diag(CM) / colSums(CM) * 100, 1)
  OA <- round(sum(diag(CM)) / sum(CM) * 100, 1)
  kappa <- pizzR::kappa(pred, ref)

  UA[length(PA) + 1] <- OA
  CM <- rbind(CM, PA)
  CM <- data.frame(cbind(CM, UA))

  return(list(CM=CM, UA=UA, PA=PA, OA = OA, KAPPA=kappa))
}

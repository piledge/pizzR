confMatrix <- function(pred, ref){
  cm <- table("pred" = pred, "ref" = ref)
  UA <- round(diag(cm) / rowSums(cm) * 100, 1)
  PA <- round(diag(cm) / colSums(cm) * 100, 1)
  OA <- round(sum(diag(cm)) / sum(cm) * 100, 1)

  UA[length(PA) + 1] <- OA
  cm <- rbind(cm, PA)
  cm <- data.frame(cbind(cm, UA))
  return(cm)
}

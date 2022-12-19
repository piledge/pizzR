slice.matrix <- function(x, nslices=1, windowsize=1, slice='h'){
  
  if (slice == 'h') base <- ncol(x)
  if (slice == 'v') base <- nrow(x)
  if (slice != 'h' && slice != 'v') return(warning("slice can only be 'h' or 'v'!"))
  
  normal.slice <- floor(base / (nslices))
  last.slice <- base - (nslices * normal.slice) + normal.slice
  if (nslices == 1) last.slice <- 0
  slice.size <- c(rep(normal.slice, nslices - 1), last.slice)
  
  oversize <- floor(windowsize / 2)
  
  selections <- list()
  if (nslices == 1){
    selections[[1]] <- 1:base
  }else{
    selections[[1]] <- 1:(normal.slice[1] + oversize)
    for (i in 1:(nslices - 2)) selections[[i+1]] <- (i * (normal.slice + 1) - oversize):((i+1) * normal.slice + oversize)
    selections[[nslices]] <- (base - last.slice - oversize + 1):base
  }
  return(selections)
}
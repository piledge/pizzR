slice.matrix <- function(x, nslices=1, windowsize=1, direction='h'){
  
  if (direction == 'h') base <- nrow(x)
  if (direction == 'v') base <- ncol(x)
  if (direction != 'h' && direction != 'v') return(warning("slice can only be 'h' or 'v'!"))
  
  normal.slice <- floor(base / (nslices))
  last.slice <- base - (nslices * normal.slice) + normal.slice
  if (nslices == 1) last.slice <- 0
  if ((base / (nslices * normal.slice)) == 1) last.slice <- 0
  #slice.size <- c(rep(normal.slice, nslices - 1), last.slice)
  
  oversize <- floor(windowsize / 2)
  
  selections <- list()
  if (nslices == 1){
    selections[[1]] <- 1:base
  }else{
    selections[[1]] <- 1:(normal.slice[1] + oversize)
    for (i in 1:(nslices - 2)) selections[[i+1]] <- (i * (normal.slice) - oversize):((i+1) * normal.slice + oversize)
    if (last.slice != 0)  selections[[nslices]] <- (base - last.slice - oversize):base
    if (last.slice == 0)  selections[[nslices]] <- (base - normal.slice - oversize):base
  }
  return(selections)
}

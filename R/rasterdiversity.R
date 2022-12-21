rasterdiversity <- function(x, index='shannon', w=3, ...){
  
  library(terra)
  
  if (index != 'eveness' && index != 'raosQ' && index != 'richness' && index != 'shannon' && index != 'simpson') return(cat("Index can either be 'eveness', 'raosQ', 'richness', 'shannon' or 'simpson'"))
  
  if (index == 'eveness'){
    fun <- function(x) {
      cnts <- table(x)
      cnts <- cnts / sum(cnts)
      shan <- -sum(cnts * log(cnts))
      even <- shan / max(shan)
    }
  }
  
  if (index == 'raosQ'){
    div <- length(x)^2
    fun <- function(x, distance = 'euclidean') sum(as.matrix(dist(x, method = distance)) / div)
  }
  
  if (index == 'richness'){
    fun <- function(x) length(unique(na.omit(x)))
  }
  
  if (index == 'shannon'){
    fun <- function(x) {
      cnts <- table(x)
      cnts <- cnts / sum(cnts)
      -sum(cnts * log(cnts))
    }
  }
  
  if (index == 'simpson'){
    fun <- function(x) {
      cnts <- table(x)
      cnts <- cnts / sum(cnts)
      -log(sum(cnts^2))
    }
  }
  
  fparameters             <- list(...)
  fparameters$x           <- x
  fparameters$w           <- w
  fparameters$fun         <- fun
  
  return(do.call(terra::focal, fparameters))
}

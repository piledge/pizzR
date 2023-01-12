rasterdiversity <- function(x, index='shannon', window=3, ...){

  index <- tolower(index)
  if (index != 'eveness' && index != 'raosq' && index != 'richness' && index != 'shannon' && index != 'simpson') return(cat("Index can either be 'eveness', 'raosq', 'richness', 'shannon' or 'simpson'"))
  
  library(terra)
  
  if (index == 'eveness'){
    fun <- function(x) {
      cnts <- table(x)
      cnts <- cnts / sum(cnts)
      shan <- -sum(cnts * log(cnts))
      even <- shan / max(shan)
    }
  }
  
  if (index == 'raosq'){
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
  fparameters$w           <- window
  fparameters$fun         <- fun
  
  return(do.call(terra::focal, fparameters))
}

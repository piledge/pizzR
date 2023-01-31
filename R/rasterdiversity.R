rasterdiversity <- function(x, index='shannon', window=3, ...){
  
  index <- tolower(index)
  if (index != 'eveness' && index != 'raosq' && index != 'richness' && index != 'shannon' && index != 'simpson') return(warning("Index can either be 'eveness', 'raosq', 'richness', 'shannon' or 'simpson'\n"))
  if (!is.numeric(window))                                                                                       return(warning("'window' has to be of type integer\n"))
  window.divided <- window / 2
  if (window.divided - floor(window.divided)) != 0.5)                                                            return(warning("'window' has to be a odd integer\n"))
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("raster", "Rcpp", "terra"))

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

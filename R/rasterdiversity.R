rasterdiversity <- function(x, index='shannon', win_xy=3, method=NULL, ...){

  index <- tolower(index)
  n_layer <- terra::nlyr(x)

  if (index != 'eveness' && index != 'raosq' && index != 'richness' && index != 'shannon' && index != 'simpson')  return(warning("Index can either be 'eveness', 'raosq', 'richness', 'shannon' or 'simpson'\n"))
  if (!inherits(win_xy, 'numeric'))                                                                               return(warning("win_xy has to be of type integer\n"))
  if (win_xy %% 2 == 0)                                                                                           return(warning("win_xy has to be a odd integer\n"))
  if (win_xy > terra::nrow(x) && win_xy > terra::ncol(x))                                                         return(warning("Focal window bigger than raster!\n"))
  if (n_layer != 1 && index != 'raosq')                                                                           return(warning("Multiband rasterdata is only applicable for RaosQ!\n"))

  pizzR::package.install(c("raster", "terra"), verbose = 1)

  if (index == 'eveness'){
    win <- win_xy
    fun <- function(x) {
      cnts <- table(x)
      s <- length(na.omit(cnts))
      cnts <- cnts / sum(cnts)
      shan <- -sum(cnts * log(cnts))
      shan / log(s)
    }
  }

  if (index == 'raosq'){
    if (n_layer != 1)  win_z <- n_layer
    if (n_layer == 1){
      win <- win_xy
      div <- win_xy^4
    }
    if (n_layer != 1){
      if (n_layer %% 2 == 0) win_z <- win_z + 1
      win <- c(win_xy, win_xy, win_z)
      div <- (win_xy^4) * (win_z^2)
    }

    if (!is.null(method) && method != 'euclidean' && method != 'maximum' && method != 'manhattan' && method != 'canberra' && method != 'binary'
        && method != 'minkowski') return(warning("'method' can either be 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'!\n"))
    if (is.null(method)) method <- "euclidean"

    fun <- function(x) sum(as.matrix(dist(na.omit(x), method=method))/div)

    for (i in seq(terra::nlyr(x))) x[[i]] <- trunc((x[[i]] / terra::minmax(x)[2,i]) * 255)

    if (n_layer %% 2 == 0){
      x.na <- terra::rast(ext=terra::ext(x), res = terra::res(x), crs=terra::crs(x))
      names(x.na) <- 'na_band'
      terra::values(x.na) <- rep(NA, terra::ncell(x.na))
      x <- c(x, x.na)
    }
  }

  if (index == 'richness'){
    win <- win_xy
    fun <- function(x) length(unique(na.omit(x)))
  }

  if (index == 'shannon'){
    win <- win_xy
    fun <- function(x) {
      cnts <- table(x)
      cnts <- cnts / sum(cnts)
      -sum(cnts * log(cnts))
    }
  }

  if (index == 'simpson'){
    win <- win_xy
    fun <- function(x) {
      p <- table(x)/length(x)
      1 / sum(p^2)
    }
  }

  fparameters             <- list(...)
  fparameters$x           <- x
  fparameters$w           <- win
  fparameters$fun         <- fun

  if (n_layer > 1)   result <- do.call(terra::focal3D, fparameters) else result <- do.call(terra::focal, fparameters)
  names(result) <- index

  return(result)
}

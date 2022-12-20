rastertiler <- function(x, nslices_x=2, nslices_y=2, overlap_x=0, overlap_y=0, verbose=F){
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "terra"))
  

  if (class(x)[1] != "SpatRaster") return(warning("Only Objects of class 'SpatRaster' are allowed!\n"))

  
  res_x.rst <- terra::res(x)[1]
  res_y.rst <- terra::res(x)[2]
  base_x.rst <- terra::ncol(x)
  base_y.rst <- terra::nrow(x)
  ext.rst <- terra::ext(x)

  normal.slice_x <- floor(base_x.rst / (nslices_x))
  normal.slice_y <- floor(base_y.rst / (nslices_y))
  
  last.slice_x <- base_x.rst - (nslices_x * normal.slice_x) + normal.slice_x
  last.slice_y <- base_y.rst - (nslices_y * normal.slice_y) + normal.slice_y


  if (nslices_x == 1) last.slice_x <- 0
  if (nslices_y == 1) last.slice_y <- 0
  if ((base_x.rst / (nslices_x * normal.slice_x)) == 1) last.slice_x <- 0
  if ((base_y.rst / (nslices_y * normal.slice_y)) == 1) last.slice_y <- 0


  normal.cropsize_x <- normal.slice_x / res_x.rst
  normal.cropsize_y <- normal.slice_y / res_y.rst
  last.cropsize_x <- last.slice_x / res_x.rst
  last.cropsize_y <- last.slice_y / res_y.rst
  overlap_x_units <- overlap_x / res_x.rst
  overlap_y_units <- overlap_y / res_y.rst


  normal_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[1] + (i_x - 1) * normal.cropsize_x, ext.rst[1] + (i_x) * normal.cropsize_x,
               ext.rst[4] - (i_y) * normal.cropsize_y, ext.rst[4] - (i_y - 1) * normal.cropsize_y)
    if (nslices_x == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_y == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }
  
  
  x_max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[2] - last.cropsize_x, ext.rst[2],
               ext.rst[4] - (i_y) * normal.cropsize_y, ext.rst[4] - (i_y - 1) * normal.cropsize_y)
    if (nslices_x == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_y == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }
  
  
  y_max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[1] + (i_x - 1) * normal.cropsize_x, ext.rst[1] + (i_x) * normal.cropsize_x,
               ext.rst[3], ext.rst[3] + last.cropsize_y)
    if (nslices_x == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_y == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }
  
  
  max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[2] - last.cropsize_x, ext.rst[2],
                 ext.rst[3], ext.rst[3] + last.cropsize_y)
    if (nslices_x == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_y == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }
  

  tiler <- function(ext.rst, i_x, i_y){
    if (i_x != nslices_x && i_y != nslices_y) return(normal_slice(ext.rst, i_x, i_y))
    if (i_x == nslices_x && i_y != nslices_y) return(x_max_slice(ext.rst, i_x, i_y))
    if (i_x != nslices_x && i_y == nslices_y) return(y_max_slice(ext.rst, i_x, i_y))
    if (i_x == nslices_x && i_y == nslices_y) return(max_slice(ext.rst, i_x, i_y))
  }
  
  
  oversizer <- function(tile, overlap_x_units, overlap_y_units, i_x, i_y, nslices_x, nslices_y){
    oversized <- terra::ext(tile[1] - overlap_x_units, tile[2] + overlap_x_units, tile[3] - overlap_y_units, tile[4] + overlap_y_units)
    if (i_x == 1) oversized[1] <- tile[1]
    if (i_x == nslices_x) oversized[2] <- tile[2]
    if (i_y == 1) oversized[4] <- tile[4]
    if (i_y == nslices_y) oversized[3] <- tile[3]
    return(ext(oversized))
  }
  
  if (verbose==T) terra::plot(ext.rst)
  
  tiles <- list()
  oversized <- list()
  counter <- 0
  
  for (i_y in 1:nslices_y){
    for (i_x in 1:nslices_x){
      counter <- counter + 1
      tiles[[counter]] <- tiler(ext.rst, i_x, i_y)
      
      if (overlap_x != 0 || overlap_y != 0){
        oversize <- oversizer(tiles[[counter]], overlap_x_units, overlap_y_units, i_x, i_y, nslices_x, nslices_y)
        oversized[[counter]] <- oversize
        plot(oversize, add=T, col='red')
      }


      if (verbose==T) terra::plot(tiles[[counter]], add=T, col='yellow')
    }
  }


  if (overlap_x != 0 || overlap_y != 0) return(list(tiles=tiles, oversized=oversized))
  if (overlap_x == 0 || overlap_y == 0) return(tiles)
}
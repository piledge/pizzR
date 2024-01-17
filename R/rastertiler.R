rastertiler <- function(x, nslices_h=2, nslices_v=2, overlap_h=0, overlap_v=0, verbose=F){

  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(pizzR::Systime(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(pizzR::Systime(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))

  if (class(x)[1] != "SpatRaster") return(warning("Only Objects of class 'SpatRaster' are allowed!\n"))
  if ((overlap_h %% 2 != 1) && (overlap_h != 0) || (overlap_v %% 2 != 1) && (overlap_v != 0)) return(warning("Overlaps have to be odd numbers!\n"))

  res_h.rst <- terra::res(x)[1]
  res_v.rst <- terra::res(x)[2]
  base_v.rst <- terra::ncol(x)
  base_h.rst <- terra::nrow(x)
  ext.rst <- terra::ext(x)

  normal.slice_v <- floor(base_v.rst / (nslices_v))
  normal.slice_h <- floor(base_h.rst / (nslices_h))

  last.slice_v <- base_v.rst - (nslices_v * normal.slice_v) + normal.slice_v
  last.slice_h <- base_h.rst - (nslices_h * normal.slice_h) + normal.slice_h


  if (nslices_v == 1) last.slice_v <- 0
  if (nslices_h == 1) last.slice_h <- 0
  if ((base_v.rst / (nslices_v * normal.slice_v)) == 1) last.slice_v <- 0
  if ((base_h.rst / (nslices_h * normal.slice_h)) == 1) last.slice_h <- 0


  normal.cropsize_v <- normal.slice_v * res_h.rst
  normal.cropsize_h <- normal.slice_h * res_v.rst
  last.cropsize_v <- last.slice_v * res_h.rst
  last.cropsize_h <- last.slice_h * res_v.rst

  if (last.cropsize_v == 0) last.cropsize_v = normal.cropsize_v
  if (last.cropsize_h == 0) last.cropsize_h = normal.cropsize_h
  overlap_h_units <- overlap_h / res_h.rst
  overlap_v_units <- overlap_v / res_v.rst


  normal_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[1] + (i_x - 1) * normal.cropsize_v, ext.rst[1] + (i_x) * normal.cropsize_v,
                         ext.rst[4] - (i_y) * normal.cropsize_h, ext.rst[4] - (i_y - 1) * normal.cropsize_h)
    if (nslices_v == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_h == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }


  x_max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[2] - last.cropsize_v, ext.rst[2],
                         ext.rst[4] - (i_y) * normal.cropsize_h, ext.rst[4] - (i_y - 1) * normal.cropsize_h)
    if (nslices_v == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_h == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }


  y_max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[1] + (i_x - 1) * normal.cropsize_v, ext.rst[1] + (i_x) * normal.cropsize_v,
                         ext.rst[3], ext.rst[3] + last.cropsize_h)
    if (nslices_v == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_h == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }


  max_slice <- function(ext.rst, i_x, i_y){
    extent <- terra::ext(ext.rst[2] - last.cropsize_v, ext.rst[2],
                         ext.rst[3], ext.rst[3] + last.cropsize_h)
    if (nslices_v == 1){
      extent[1] <- ext.rst[1]
      extent[2] <- ext.rst[2]
    }
    if (nslices_h == 1){
      extent[3] <- ext.rst[3]
      extent[4] <- ext.rst[4]
    }
    return(extent)
  }


  tiler <- function(ext.rst, i_x, i_y){
    if (i_x != nslices_v && i_y != nslices_h) return(normal_slice(ext.rst, i_x, i_y))
    if (i_x == nslices_v && i_y != nslices_h) return(x_max_slice(ext.rst, i_x, i_y))
    if (i_x != nslices_v && i_y == nslices_h) return(y_max_slice(ext.rst, i_x, i_y))
    if (i_x == nslices_v && i_y == nslices_h) return(max_slice(ext.rst, i_x, i_y))
  }


  oversizer <- function(tile, overlap_h_units, overlap_v_units, i_x, i_y, nslices_v, nslices_h){
    oversized <- terra::ext(tile[1] - overlap_h_units, tile[2] + overlap_h_units, tile[3] - overlap_v_units, tile[4] + overlap_v_units)
    if (i_x == 1) oversized[1] <- tile[1]
    if (i_x == nslices_v) oversized[2] <- tile[2]
    if (i_y == 1) oversized[4] <- tile[4]
    if (i_y == nslices_h) oversized[3] <- tile[3]
    return(terra::ext(oversized))
  }

  if (verbose) terra::plot(ext.rst)
  tiles <- list()
  oversized <- list()
  counter <- 1

  for (i_y in 1:nslices_h){
    for (i_x in 1:nslices_v){
      tiles[[counter]] <- tiler(ext.rst, i_x, i_y)

      if (overlap_h != 0 || overlap_v != 0){
        oversize <- oversizer(tiles[[counter]], overlap_h_units, overlap_v_units, i_x, i_y, nslices_v, nslices_h)
        oversized[[counter]] <- oversize
      }

      if (verbose) terra::plot(tiles[[counter]], add=T, col='yellow')
      counter <- counter + 1
    }
  }

  tiles.result <- matrix(ncol=4)
  for (i in 1:length(tiles)) tiles.result <- rbind(tiles.result, matrix(tiles[[i]], ncol = 4, byrow=T))
  tiles.result <- tiles.result[-1,]
  colnames(tiles.result) <- c('xmin', 'xmax', 'ymin', 'ymax')

  if (overlap_h != 0 || overlap_v != 0){

    oversized.result <- matrix(ncol=4)
    for (i in 1:length(oversized)) oversized.result <- rbind(oversized.result, matrix(oversized[[i]], ncol = 4, byrow=T))
    oversized.result <- oversized.result[-1,]
    colnames(oversized.result) <- c('xmin', 'xmax', 'ymin', 'ymax')

    return(list(tiles=tiles.result, oversized=oversized.result))
  }

  if (overlap_h == 0 || overlap_v == 0) return(tiles.result)
}

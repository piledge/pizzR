LSMS.scaler <- function(rasterobject=NULL, maxval=255){

  pizzR::package.install(c("raster", "terra"), verbose = 1)

  rsttype <- class(rasterobject)[1]
  if (rsttype != "SpatRaster") return(warning("Only Objects of class 'Spatraster' are allowed!\n"))

  terra::setMinMax(rasterobject, force=T)

  n.bands <- terra::nlyr(rasterobject)
  n.bands.chars <- nchar(n.bands)

  rst.minmax <- terra::minmax(rasterobject)

  for (i in seq(n.bands)){

    minval <- rst.minmax[1,i]
    maxval <- rst.minmax[2,i]

    fact <- (maxval - minval) / maxval

    cat(paste0("\n", pizzR::Systime(), ": Scale band ",sprintf(paste0("%0", n.bands.chars, ".f"), i), " of ", n.bands))
    rasterobject[[i]] <- (rasterobject[[i]]-minval)/fact
  }

  terra::setMinMax(rasterobject, force=T)
  cat(paste0("\n\n", pizzR::Systime(), ": Done ...\n\n"))

  return(rasterobject)
}

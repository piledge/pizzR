OTB_lsms_scaler <- function(rasterobject=NULL, maxval=255, truncate=F){

  pizzR::package.install(c("raster", "terra"), verbose = 1)

  rsttype <- class(rasterobject)[1]
  if (rsttype != "SpatRaster") return(warning("Only Objects of class 'Spatraster' are allowed!\n"))
  if (!is.logical(truncate))         return(warning("'truncate' has to be of class logical!\n"))
  if (!is.numeric(maxval))         return(warning("'maxval' has to be of class integer!\n"))

  terra::setMinMax(rasterobject, force=T)

  n.bands <- terra::nlyr(rasterobject)
  n.bands.chars <- nchar(n.bands)

  rst.minmax <- terra::minmax(rasterobject)

  for (i in seq(n.bands)){

    minval <- rst.minmax[1,i]
    maxval <- rst.minmax[2,i]
    fact <- 1/((maxval - minval))*255

    rasterobject[[i]] <- (rasterobject[[i]]-minval)*fact
    if (truncate)  rasterobject[[i]] <- trunc(rasterobject[[i]])

    cat(paste0("\n", pizzR::Systime(), ": Scale band ",sprintf(paste0("%0", n.bands.chars, ".f"), i), " of ", n.bands))
  }

  terra::setMinMax(rasterobject, force=T)
  cat(paste0("\n\n", pizzR::Systime(), ": Done ...\n\n"))

  return(rasterobject)
}

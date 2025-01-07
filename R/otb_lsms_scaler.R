OTB_lsms_scaler <- function (rasterobject = NULL, maxval = 255, truncate = F)
{
  pizzR::package.install(c("raster", "terra"), verbose = 1)

  rsttype <- class(rasterobject)[1]
  stopifnot(rsttype == "SpatRaster")
  stopifnot(is.logical(truncate))
  stopifnot(is.numeric(maxval))

  n.bands <- terra::nlyr(rasterobject)
  n.bands.chars <- nchar(n.bands)

  cat(sprintf("\n\n%s: Scale %d bands ...\n", pizzR::Systime(), n.bands))

  terra::setMinMax(rasterobject, force = T)
  rst.minmax <- terra::minmax(rasterobject)
  for (i in seq(n.bands)) {
    minval <- rst.minmax[1, i]
    maxval <- rst.minmax[2, i]
    fact <- 1/((maxval - minval)) * 255
    rasterobject[[i]] <- (rasterobject[[i]] - minval) * fact
    if (truncate)
      rasterobject[[i]] <- round(rasterobject[[i]], 0)
    cat(sprintf("\n%s: Scale band %0*d of %d", pizzR::Systime(), n.bands.chars, i, n.bands))
  }
  terra::setMinMax(rasterobject, force = T)
  cat(sprintf("\n\n%s: Done ...\n\n", pizzR::Systime()))
  return(rasterobject)
}

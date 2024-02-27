ranRasterpredict <- function(rasterobject, ranger, na.rm = F, visualize=F, ...){
  pizzR::package.install(c("ranger", "raster", "terra"), verbose = 1)

  if (class(rasterobject)[1] != "SpatRaster") return(warning("Not a suitable rasterfile!\n"))
  if (class(ranger) != "ranger") return(warning("Not a suitable ranger-model!\n"))
  if (!is.logical(na.rm)) return(warning("'na.rm' needs to be boolean!\n"))
  if (!is.logical(plot)) return(warning("'plot' needs to be boolean!\n"))

  dots        <- list(...)
  dots$object <- rasterobject
  dots$model  <- ranger
  dots$fun    <- \(...)  predict(...)$predictions
  dots$na.rm  <- na.rm

  pred <- do.call(terra::predict, dots)

  if (visualize) terra::plot(pred)

  return(pred)
}



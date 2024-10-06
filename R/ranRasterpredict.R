ranRasterpredict <- function(rasterobject, ranger, na.rm = FALSE, plot = FALSE, ...){
  pizzR::package.install(c("ranger", "raster", "terra"), verbose = 1)

  stopifnot(
    inherits(rasterobject, "SpatRaster"),
    inherits(ranger, "ranger"),
    is.logical(na.rm),
    is.logical(plot)
  )

  dots <- list(...)
  dots$object <- rasterobject
  dots$model  <- ranger
  dots$fun    <- \(...) predict(...)$predictions
  dots$na.rm  <- na.rm

  pred <- do.call(terra::predict, dots)

  mtx <- matrix(c(NaN, NA), ncol = 2, byrow = TRUE)
  pred <- terra::classify(pred, mtx)

  if (plot) terra::plot(pred)

  return(pred)
}

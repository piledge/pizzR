ranRasterpredict <- function(rasterobject, ranger, na.rm = FALSE, plot = FALSE, ...){
  pizzR::package.install(c("ranger", "raster", "terra"), verbose = 1)

  stopifnot(class(rasterobject)[1] == "SpatRaster", msg = "Not a suitable rasterfile!")
  stopifnot(class(ranger) == "ranger", msg = "Not a suitable ranger-model!")
  stopifnot(is.logical(na.rm), msg = "'na.rm' needs to be boolean!")
  stopifnot(is.logical(plot), msg = "'plot' needs to be boolean!")

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

plot.rstshp <- function(x, y){
  rsttype <- class(x)[1]
  stopifnot(inherits(rsttype, c("SpatRaster", "SpatVector", "RasterLayer", "RasterBrick", "RasterStack")))
  
  pizzR::package.install(c("raster", "terra"), verbose = 1)
  
  if(rsttype == "SpatRaster" || rsttype == "SpatVector"){
    terra::plot(x)
    terra::plot(y, add = TRUE)
  }
  if(rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack" || rsttype == "SpatialPolygonsDataFrame"){
    raster::plot(x)
    raster::plot(y, add = TRUE)
  }
}

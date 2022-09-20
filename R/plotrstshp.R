plot.rstshp <- function(x, y){
  rsttype <- class(x)[1]
  
  if(rsttype == "SpatRaster" || rsttype == "SpatVector"){
    terra::plot(x)
    terra::plot(y, add = TRUE)
  }
  if(rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack" || rsttype == "SpatialPolygonsDataFrame"){
    raster::plot(x)
    raster::plot(y, add = TRUE)
  }
  if(rsttype != "SpatRaster" && rsttype != "SpatVector" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") warning("\nNot a suitable rasterfile!\n")
}

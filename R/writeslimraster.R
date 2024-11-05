function (x) 
{
  pizzR::package.install(c("memuse", "raster", "terra"), verbose = 1)
  rsttype <- class(x)[1]
  stopifnot(rsttype %in% c("SpatRaster", "RasterLayer", "RasterBrick", 
                           "RasterStack"))
  if (rsttype == "SpatRaster") {
    terra::setMinMax(x, force = T)
    minmaxvals <- terra::minmax(x)
    vals_sample <- as.matrix(terra::spatSample(x, size = 100, 
                                               na.rm = T))
  }
  else {
    minmaxvals <- raster::minValue(x)
    vals_sample <- as.matrix(raster::sampleRandom(x, size = 100, 
                                                  na.rm = T))
  }
  if (any(is.logical(vals_sample))) 
    return("LOG1S")

  if (inherits(vals_sample, c('matrix', 'array'))){
    n_classes <- length(unique(test))
    if (n_classes < 65534){
      return("INT2U") 
    }
    else {
        return("INT4U")
      }
  }

  rst_min <- min(minmaxvals, na.rm = TRUE)
  rst_max <- max(minmaxvals, na.rm = TRUE)
  rst_significant_value <- max(abs(c(rst_min, rst_max)), na.rm = TRUE)
  rst_signed <- rst_min < 0
  rst_float <- !all((floor(vals_sample) == vals_sample), na.rm = TRUE)
  if (rst_float) 
    return(ifelse(rst_significant_value < 3.4e+38, "FLT4S", 
                  "FLT8S"))
  if (rst_signed) {
    if (rst_significant_value <= 127) {
      return("INT2S")
    }
    else if (rst_significant_value <= 32767) {
      return("INT2S")
    }
    else if (rst_significant_value <= 2147483647) {
      return("INT4S")
    }
  }
  else {
    if (rst_significant_value <= 254) {
      return("INT2U")
    }
    else if (rst_significant_value <= 65534) {
      return("INT2U")
    }
    else if (rst_significant_value <= 4294967294) {
      return("INT4U")
    }
  }
}

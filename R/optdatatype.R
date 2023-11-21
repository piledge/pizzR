opt.datatype <- function(x) {
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)) {
      cat(paste0(Sys.time(), ": install missing packages '", 
                 paste(x[to_install], collapse = ", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], 
                                                           collapse = ", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))

  rsttype <- class(x)[1]
  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && 
      rsttype != "RasterBrick" && rsttype != "RasterStack") 
    return(warning("Not a suitable rasterfile!\n"))

  if (rsttype == "SpatRaster")                                                            minmaxvals <- terra::minmax(x)
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack")   minmaxvals <- raster::minValue(x)
  
  rst_min <- min(minmaxvals)
  rst_max <- max(minmaxvals)
  rst_significant_value <- max(abs(c(rst_min, rst_max)))
  rst_signed <- rst_min < 0
  rst_float <- TRUE
  
  if(all((floor(minmaxvals)/minmaxvals) == 1, na.rm = T)) rst_float <- FALSE
  
  if (all(is.logical(minmaxvals)))
    return("LOG1S")
  if (rst_float == T) {
    if (rst_significant_value < 3.4e+38) {
      return("FLT4S")
    }
    else return("FLT8S")
  }
  if (rst_float == FALSE) {
    if (rst_signed == TRUE) {
      if (rst_significant_value <= 127) {
        return("INT2S")
      }
      if (rst_significant_value <= 32767) {
        return("INT2S")
      }
      if (rst_significant_value <= 2147483647) {
        return("INT4S")
      }
    }
    if (rst_signed == FALSE) {
      if (rst_significant_value <= 254) {
        return("INT2U")
      }
      if (rst_significant_value <= 65534) {
        return("INT2U")
      }
      if (rst_significant_value <= 4294967294) {
        return("INT4U")
      }
    }
  }
}

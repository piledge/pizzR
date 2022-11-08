opt.datatype <- function(spatialraster, samplesize=25){
  rsttype <- class(spatialraster)[1]
  
  if (samplesize <= 0) return(warning("Samplesize must be >0!\n"))
  if (ncell(spatialraster) < samplesize) size <- ncell(spatialraster) else size <- samplesize
  
  if (rsttype == "SpatRaster") rst_sample_cells <- terra::spatSample(spatialraster, size, "random", values = TRUE, na.rm = TRUE, warn = FALSE)
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") rst_sample_cells <- as.data.frame(raster::sampleRandom(spatialraster, size, na.rm = TRUE))
  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
  
  rst_sample_cells[rst_sample_cells==Inf] <- NA
  
  if (rsttype == "SpatRaster"){
    rst_min <- min(spatialraster@ptr[["range_min"]])
    rst_max <- max(spatialraster@ptr[["range_max"]])
  }
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){
    rst_min <- min(minValue(spatialraster))
    rst_max <- max(maxValue(spatialraster))
  }
  
  rst_significant_value <- max(abs(c(rst_min, rst_max)))
  rst_signed <- rst_min < 0
  
  rst_float <- TRUE
  if (all((floor(rst_sample_cells) / rst_sample_cells) == 1, na.rm = T)) rst_float <- FALSE
  
  if (nrow(rst_sample_cells) == 0) return("LOG1S")                                #LOG
  if (is.logical(rst_sample_cells[,1:ncol(rst_sample_cells)])) return("LOG1S")
  
  if (rst_float == T){                            #FLOAT
    if (rst_significant_value < 3.4e+38){
      return("FLT4S")
    }else
      return("FLT8S")
  }
  
  if (rst_float == FALSE){                            #INT
    if (rst_signed == TRUE){                             #INTS
      if (rst_significant_value <= 127){
        return("INT2S")
      }
      if (rst_significant_value <= 32767){
        return("INT2S")
      }
      if (rst_significant_value <= 2147483647){
        return("INT4S")
      }
    }
    if (rst_signed == FALSE){                             #INTU
      if (rst_significant_value <= 254){
        return("INT2U")
      }
      if (rst_significant_value <= 65534){
        return("INT2U")
      }
      if (rst_significant_value <= 4294967294){
        return("INT4U")
      }
    }
  }
}

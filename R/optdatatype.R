opt.datatype <- function(x, samplesize=25){
  rsttype <- class(x)[1]
  
  if (samplesize <= 0) return(warning("Samplesize must be >0!\n"))
  if (ncell(x) < samplesize) size <- ncell(x) else size <- samplesize
  
  if (rsttype == "SpatRaster") rst_sample_cells <- terra::spatSample(x, size, "random", values = TRUE, na.rm = TRUE, warn = FALSE)
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") rst_sample_cells <- as.data.frame(raster::sampleRandom(x, size, na.rm = TRUE))
  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
  
  rst_sample_cells[rst_sample_cells==Inf] <- NA
  
  if (rsttype == "SpatRaster"){         
    rst_min <- min(x@ptr[["range_min"]])
    rst_max <- max(x@ptr[["range_max"]])
    
    if (is.na(rst_min) || is.na(rst_max)){                                        #slower; if ptr not available
      rst.summary <- terra::summary(x, warn = F)
      
      rst_min <- rst.summary[1,]
      rst_min <- unlist(strsplit(rst_min, ":"))
      rst_min <- rst_min[-grep("Min", rst_min)]
      rst_min <- min(as.numeric(rst_min))
      
      rst_max <- rst.summary[6,]
      rst_max <- unlist(strsplit(rst_max, ":"))
      rst_max <- rst_max[-grep("Max", rst_max)]
      rst_max <- min(as.numeric(rst_max))
    }
  }
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){
    rst_min <- min(minValue(x))
    rst_max <- max(maxValue(x))
    
    if (is.na(rst_min) || is.na(rst_max)){                                        #slower; if ptr not available
      rst.summary <- raster::summary(x, maxsamp = 10000)
      min <- rst.summary[1,]
      max <- rst.summary[1,]
      
      min(rst.summary[rownames(rst.summary) == "Min.", ])
      max(rst.summary[rownames(rst.summary) == "Max.", ])
    }
  }
  
  rst_significant_value <- max(abs(c(rst_min, rst_max)))
  rst_signed <- rst_min < 0
  
  rst_float <- TRUE
  if (all((floor(rst_sample_cells) / rst_sample_cells) == 1, na.rm = T)) rst_float <- FALSE
  
  if (nrow(rst_sample_cells) == 0) return("LOG1S")                              #LOG
  if (is.logical(rst_sample_cells[,1:ncol(rst_sample_cells)])) return("LOG1S")
  
  if (rst_float == T){                                                          #FLOAT
    if (rst_significant_value < 3.4e+38){
      return("FLT4S")
    }else
      return("FLT8S")
  }
  
  if (rst_float == FALSE){                                                      #INT
    if (rst_signed == TRUE){                                                    #INTS
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
    if (rst_signed == FALSE){                                                   #INTU
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

writeslimRaster <- function(x, filename, compression=T, overwrite=T, BIGTIFF="IF_NEEDED", filetype="GTiff", samplesize=25, ...){
  
  opt.datatype <- function(x, x.samplesize=samplesize){
    rsttype <- class(x)[1]
    
    if (x.samplesize <= 0) return(warning("x.samplesize must be >0!\n"))
    if (ncell(x) < x.samplesize) size <- ncell(x) else size <- x.samplesize
    
    if (rsttype == "SpatRaster") rst_sample_cells <- terra::spatSample(x, size, "random", values = TRUE, na.rm = TRUE, warn = FALSE)
    if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") rst_sample_cells <- as.data.frame(raster::sampleRandom(x, size, na.rm = TRUE))
    if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
    
    rst_sample_cells[rst_sample_cells==Inf] <- NA
    
    if (rsttype == "SpatRaster"){
      rst_min <- min(x@ptr[["range_min"]])
      rst_max <- max(x@ptr[["range_max"]])
    }
    if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){
      rst_min <- min(minValue(x))
      rst_max <- max(maxValue(x))
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
  
  rsttype <- class(x)[1]
  
  fparameters             <- list(...)
  fparameters$x           <- x
  fparameters$filename    <- filename
  fparameters$overwrite   <- overwrite
  
  if (compression == TRUE)  cat(paste0("\n", Sys.time(), ": Export slim rasterfile ...\n"))
  if (compression == FALSE) cat(paste0("\n", Sys.time(), ": Export rasterfile ...\n"))
  
  if (rsttype == "SpatRaster"){

    fparameters$filetype                            <- filetype
    if (compression == TRUE)  fparameters$datatype  <- opt.datatype(x)
    if (compression == TRUE)  fparameters$gdal      <- c(paste0("BIGTIFF = ", BIGTIFF), "COMPRESS = DEFLATE", "ZLEVEL = 9", "PREDICTOR = 2")
    if (compression == FALSE) fparameters$gdal      <- c(paste0("BIGTIFF = ", BIGTIFF))
    
    do.call(terra::writeRaster, fparameters)
  }
  
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){

    fparameters$format                             <- filetype
    if (compression == TRUE) fparameters$datatype  <- opt.datatype(x)
    if (compression == TRUE) fparameters$options   <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9")
    
    do.call(raster::writeRaster, fparameters)
  }

  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
}

writeslimRaster <- function(x, filename, compression = T, overwrite = T, BIGTIFF = "IF_NEEDED", filetype = "GTiff", ...){

  opt.datatype <- function(spatialraster){
    sample.size <- function(cellnumber, k1 = 0.5, d1 = 15000, k2 = 0.25, d2 = 40000, maxsample = 50000){
      if (k2 * (cellnumber - d2) + k1*(d2-d1)+d1 > maxsample){
        return(maxsample)
      }
      if (cellnumber > d2){
        return(k2 * (cellnumber - d2) + k1*(d2-d1)+d1)
      }else if (cellnumber > d1){
        return(k1 * (cellnumber - d1) + d1)
      }else{
        return(cellnumber)
      }
    }

    rsttype <- class(spatialraster)[1]

    if(rsttype == "SpatRaster") rst_sample_cells <- terra::spatSample(spatialraster, sample.size(ncell(spatialraster)), "random", values = TRUE, na.rm = TRUE, warn = FALSE)
    if(rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack") rst_sample_cells <- raster::sampleRandom(spatialraster, sample.size(ncell(spatialraster)), na.rm = TRUE)

    rst_sample_cells[rst_sample_cells==Inf] <- NA

    rst_min <- range(rst_sample_cells)[1]
    rst_max <- range(rst_sample_cells)[2]

    rst_significant_value <- max(abs(c(rst_min, rst_max)))
    rst_signed <- rst_min < 0

    rst_float <- TRUE
    if(all((floor(rst_sample_cells) / rst_sample_cells) == 1, na.rm = T)) rst_float <- FALSE


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
          return("INT1S")
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
          return("INT1U")
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

  if(compression == TRUE)  cat(paste0("\n", Sys.time(), ": Export slim rasterfile ...\n"))
  if(compression == FALSE) cat(paste0("\n", Sys.time(), ": Export rasterfile ...\n"))

  if(rsttype == "SpatRaster"){

                             fparameters$filetype  <- filetype
    if(compression == TRUE)  fparameters$datatype  <- opt.datatype(x)
    if(compression == TRUE)  fparameters$gdal      <- c(paste0("BIGTIFF = ", BIGTIFF), "COMPRESS = DEFLATE", "ZLEVEL = 9", "PREDICTOR = 2")
    if(compression == FALSE) fparameters$gdal      <- c(paste0("BIGTIFF = ", BIGTIFF))

    do.call(terra::writeRaster, fparameters)
  }

  if(rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){

                            fparameters$format    <- filetype
    if(compression == TRUE) fparameters$datatype  <- opt.datatype(x)
    if(compression == TRUE) fparameters$options   <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9")

    do.call(raster::writeRaster, fparameters)
  }

  if(rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
}

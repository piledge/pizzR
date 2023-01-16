writeslimRaster <- function(x, filename, compression=T, overwrite=T, BIGTIFF="YES", filetype="GTiff", samplesize=25, datatype="ESTIMATE", ...){
  
  rsttype <- class(x)[1]
  if (rsttype != "SpatRaster" && rsttype != "RasterLayer" && rsttype != "RasterBrick" && rsttype != "RasterStack") return(warning("Not a suitable rasterfile!\n"))
  if (!is.logical(compression)) return(warning("'compression' needs to be boolean!\n"))
  if (!is.logical(overwrite)) return(warning("'overwrite' needs to be boolean!\n"))
  if (BIGTIFF != "YES" && BIGTIFF != "NO" && BIGTIFF != "IF_SAFER" && BIGTIFF != "IF_NEEDED") return(warning("'BIGTIFF' should be one of the following options: YES/NO/IF_SAFER/IF_NEEDED!\n"))
  if (datatype != "LOG1S" && datatype != "INT1S" && datatype != "INT1U" && datatype != "INT2S" && datatype != "INT2U" && datatype != "INT4S" && datatype != "INT4U" && datatype != "FLT4S" && datatype != "FLT8S" && datatype != "ESTIMATE") return(warning("'datatype' should be either ESTIMATE or one of the following options: LOG1S/INT1S/INT1U/INT2S/INT2U/INT4S/INT4U/FLT4S/FLT8S!\n"))
  
  fparameters             <- list(...)
  fparameters$x           <- x
  fparameters$filename    <- filename
  fparameters$overwrite   <- overwrite
  fparameters$datatype    <- pizzR::opt.datatype(x, samplesize)
  
  if (rsttype == "SpatRaster"){
    
    fparameters$filetype                                                        <- filetype
    if (compression && datatype == "ESTIMATE") cat(paste0("\n", Sys.time(), ": Estimate datatype ..."))

    if (compression)  fparameters$gdal                                  <- c(paste0("BIGTIFF = ", BIGTIFF), "COMPRESS = DEFLATE", "ZLEVEL = 9", "PREDICTOR = 2")
    if (!compression) fparameters$gdal                                  <- c(paste0("BIGTIFF = ", BIGTIFF))
    
    if (compression)  cat(paste0("\n", Sys.time(), ": Write slim rasterfile as '", fparameters$datatype,"' ...\n"))
    if (!compression) cat(paste0("\n", Sys.time(), ": Write rasterfile ...\n"))
    do.call(terra::writeRaster, fparameters)
  }
  
  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){
    
    fparameters$format                                                          <- filetype
    if (compression && datatype == "ESTIMATE") cat(paste0("\n", Sys.time(), ": Estimate datatype ..."))

    if (compression && datatype != "ESTIMATE") fparameters$datatype     <- datatype
    if (compression) fparameters$options                                <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9")
    
    if (compression)  cat(paste0("\n", Sys.time(), ": Write slim rasterfile as '", fparameters$datatype,"' ...\n"))
    if (!compression) cat(paste0("\n", Sys.time(), ": Write rasterfile ...\n"))
    do.call(raster::writeRaster, fparameters)
  }
  cat(paste0("\n", Sys.time(), ": Done ...\n"))
}

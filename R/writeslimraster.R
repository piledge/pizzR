writeslimRaster <- function(rasterobject, filename, compression=T, overwrite=T, BIGTIFF="YES", filetype="GTiff", datatype="ESTIMATE", verbose = T, ...){

  pizzR::package.install(c("raster", "terra"), verbose = 1)

  rsttype <- class(rasterobject)[1]
  stopifnot(
    rsttype %in% c("SpatRaster", "RasterLayer", "RasterBrick", "RasterStack"),
    is.logical(compression),
    is.logical(overwrite),
    BIGTIFF %in% c("YES", "NO", "IF_SAFER", "IF_NEEDED"),
    datatype %in% c("LOG1S", "INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT8S", "ESTIMATE")
  )

  fparameters             <- list(...)
  fparameters$filename    <- filename
  fparameters$overwrite   <- overwrite
  fparameters$filetype    <- filetype
  if (datatype != "ESTIMATE") fparameters$datatype <- datatype

  if (rsttype == "SpatRaster"){

    if (compression && datatype == "ESTIMATE"){
      if (verbose) cat(sprintf("\n%s: Estimate datatype ...", pizzR::Systime()))
      fparameters$datatype                                                      <- pizzR::opt.datatype(rasterobject)
    }
    if (compression){
      fparameters$gdal <- c(sprintf("BIGTIFF = %s", BIGTIFF), "COMPRESS = DEFLATE", "ZLEVEL = 9", "PREDICTOR = 2")
      if (verbose) cat(sprintf("\n%s: Write slim rasterfile as '%s' ...\n", pizzR::Systime(), fparameters$datatype))
    }else{
      fparameters$gdal <- c(sprintf("BIGTIFF = %s", BIGTIFF))
      if (verbose) cat(sprintf("\n%s: Write rasterfile ...\n", pizzR::Systime()))
    }

    terra::setMinMax(rasterobject, force = T)
    fparameters$x                                                               <- rasterobject
    do.call(terra::writeRaster, fparameters)
  }

  if (rsttype == "RasterLayer" || rsttype == "RasterBrick" || rsttype == "RasterStack"){

    if (compression && datatype == "ESTIMATE"){
      if (verbose) cat(sprintf("\n%s: Estimate datatype ...", pizzR::Systime()))
      fparameters$datatype                                                      <- pizzR::opt.datatype(x, samplesize)
    }
    if (compression && datatype != "ESTIMATE") fparameters$datatype     <- datatype
    if (compression) fparameters$options                                <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9")

    if (compression && verbose) cat(sprintf("\n%s: Write slim rasterfile as '%s' ...\n", pizzR::Systime(), fparameters$datatype)) else cat(sprintf("\n%s: Write rasterfile ...\n", pizzR::Systime()))

    fparameters$x                                                               <- rasterobject
    do.call(raster::writeRaster, fparameters)
  }
  gc(reset = T, full = T)
  if (verbose) cat(sprintf("%s: Done ...\n", pizzR::Systime()))
}

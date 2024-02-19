tableToSpatialpoints <- function(northing,easting,crs.origin,crs.project=NULL,attributes=NULL,filename=NULL,overwrite=T,plot=FALSE,filetype=NULL,layer=NULL,write=F,opath=NULL,options="ENCODING=UTF-8",...){

  pizzR::package.install(c("sp", "raster", "terra"), verbose = 1)

  table_xy <- data.frame(latitude=suppressWarnings(as.numeric(northing)), longitude=suppressWarnings(as.numeric(easting)))

  if (any(is.na(table_xy$longitude)) && any(is.na(table_xy$latitude)))  return(warning("Northing- and Easting contain non-numeric values"))
  if (any(is.na(table_xy$longitude)))                                   return(warning("Northing contains non-numeric values"))
  if (any(is.na(table_xy$latitude)))                                    return(warning("Easting contains non-numeric values"))
  if (!is.logical(overwrite))                                           return(warning("'overwrite' has to be of type logical!"))
  if (!is.logical(plot))                                                return(warning("'plot' has to be of type logical!"))
  if (!is.logical(write))                                               return(warning("'write' has to be of type logical!"))

  if (!write && is.null(opath))   opath <- tempdir()
  if (write && is.null(opath))    opath <- getwd()
  if (!write && !is.null(opath))  opath <- tempdir()

  if (!is.null(attributes)) data <- cbind(table_xy, attributes) else data <- table_xy

  crs.origin.param <- pizzR::get.crsparams(crs.origin)

  xySPoints <- sp::SpatialPointsDataFrame(coords = c(data[,c("longitude", "latitude")]),
                                          proj4string = sp::CRS(crs.origin.param),
                                          data = data)

  shp <- terra::vect(xySPoints)
  crs.export <- crs.origin

  if (!is.null(crs.project)){
    crs.project.param <- pizzR::get.crsparams(crs.project)
    shp <- terra::project(shp, crs.project.param)
    crs.export <- crs.project
  }

  if (!is.null(filename)) filename <- file.path(opath, filename) else filename <- file.path(opath, paste0("points_", crs.export, ".shp"))
  if (plot) terra::plot(shp, main=paste0('CRS: ', crs.export))

  fparameters             <- list(...)
  fparameters$x           <- shp
  fparameters$filename    <- filename
  fparameters$overwrite   <- overwrite
  fparameters$filetype    <- filetype
  fparameters$layer       <- layer
  fparameters$options     <- options

  do.call(terra::writeVector, fparameters)

  if (write) cat(paste0("\n", pizzR::Systime(), ": '",filename, "' written to '", dirname(filename), "'\n"))

  invisible(terra::vect(filename))
}

tableToSpatialpoints <- function(northing,easting,crs.origin,crs.project=NULL,attributes=NULL,filename=NULL,plot=FALSE){
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("sp", "raster", "terra", "Rcpp", "rvest", "xml2"))
  
  table_xy <- data.frame(longitude=suppressWarnings(as.numeric(northing)), latitude=suppressWarnings(as.numeric(easting)))
  
  if (any(is.na(table_xy$longitude)) && any(is.na(table_xy$latitude))) return(warning("Northing- and Easting contain non-numeric values"))
  if (any(is.na(table_xy$longitude)))                                  return(warning("Northing contains non-numeric values"))
  if (any(is.na(table_xy$latitude)))                                   return(warning("Easting contains non-numeric values"))
  
  if (!is.null(attributes)) data <- cbind(table_xy, attributes) else data <- table_xy
  
  crs.origin.link <- paste0("https://spatialreference.org/ref/epsg/", crs.origin, "/proj4/")
  crs.origin.param <- rvest::html_text(xml2::read_html(crs.origin.link))
  
  xySPoints <- sp::SpatialPointsDataFrame(coords = c(data[,c("longitude", "latitude")]),
                                          proj4string = sp::CRS(crs.origin.param),
                                          data = data)
  
  shp <- terra::vect(xySPoints)
  crs.export <- crs.origin
  
  if (!is.null(crs.project)){
    crs.project.link <- paste0("https://spatialreference.org/ref/epsg/", crs.project, "/proj4/")
    crs.project.param <- rvest::html_text(xml2::read_html(crs.project.link))
    
    shp <- terra::project(shp, crs.project.param)
    crs.export <- crs.project
  }
  
  if (!is.null(filename)) filename <- filename else filename <- paste0("points_", crs.export, ".shp")
  
  if (plot == T) terra::plot(shp, main=paste0('CRS: ', crs.export))
  
  terra::writeVector(shp, filename=filename, filetype=NULL, layer=NULL,
                     overwrite=T, options="ENCODING=UTF-8", )
  
  cat(paste0("\n", Sys.time(), ": '",filename, "' exported to '", getwd(), "'\n\n"))
}

get.crsparams <- function(epsg.crs=NULL){
  pizzR::package.install(c("rvest", "xml2"), verbose = 1)

  crs.link <- paste0("https://spatialreference.org/ref/epsg/", epsg.crs, "/proj4.txt")
  crs.params <- rvest::html_text(xml2::read_html(crs.link))
  return(crs.params)
}

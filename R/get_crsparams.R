get.crsparams <- function(epsg.crs = NULL) {
  if (is.null(epsg.crs) || !is.numeric(epsg.crs)) {
    stop("Bitte einen gültigen EPSG-Code angeben.")
  }

  pizzR::package.install(c("rvest", "xml2"), verbose = 1)
  
  crs.link <- paste0("https://spatialreference.org/ref/epsg/", epsg.crs, "/proj4.txt")
  
  tryCatch({
    crs.params <- rvest::html_text(xml2::read_html(crs.link))
  }, error = function(e) {
    stop("Fehler beim Abrufen der CRS-Parameter: ", e$message)
  })
  
  return(crs.params)
}

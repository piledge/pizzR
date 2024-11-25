get.crsparams <- function(epsg.crs = NULL) {
  if (is.null(epsg.crs) || !is.numeric(epsg.crs)) {
    stop("Please enter a valid EPSG-Code")
  }

  pizzR::package.install(c("rvest", "xml2"), verbose = 1)

  crs.link <- paste0("https://spatialreference.org/ref/epsg/", epsg.crs, "/proj4.txt")

  tryCatch({
    crs.params <- rvest::html_text(xml2::read_html(crs.link))
  }, error = function(e) {
    stop("Error when retrieving the CRS parameters: ", e$message)
  })

  return(crs.params)
}

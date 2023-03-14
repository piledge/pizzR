raster_to_envi_converter <- function(input.path, output.path, interleave = 'BSQ', input.filetype = '.tif$'){

  interleave <- toupper(interleave)
  if (!interleave %in% c('BSQ', 'BIL', 'BIP'))  return(warning("'interleave' has to be 'BSQ', 'BIL' or 'BIP'"))
    rfiles <- list.files(input.path, full.names = T, pattern = input.filetype)
  if (length(rfiles) == 0)  return(warning("No raster files found!"))
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)) {
      cat(paste0(Sys.time(), ": install missing packages '", 
        paste(x[to_install], collapse = ", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], 
        collapse = ", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))

  pizzR::setcreate.wd(output.path)
  for (i in seq(rfiles)) terra::writeRaster(terra::rast(rfiles[i]), overwrite = T,
                                            filename = sub('.tif', '', basename(rfiles[i])), filetype="ENVI",
                                            gdal=c(paste0('INTERLEAVE=', interleave), "BIGTIFF = IF_NEEDED", "COMPRESS = DEFLATE", "ZLEVEL = 9", ""))
}

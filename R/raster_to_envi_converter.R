raster_to_envi_converter <- function(input.path, output.path, interleave = 'BSQ', input.filetype = '.tif$'){

  interleave <- toupper(interleave)
  if (!interleave %in% c('BSQ', 'BIL', 'BIP'))  return(warning("'interleave' has to be 'BSQ', 'BIL' or 'BIP'"))
    rfiles <- list.files(input.path, full.names = T, pattern = input.filetype)
  if (length(rfiles) == 0)  return(warning("No raster files found!"))

  pizzR::package.install(c("memuse", "raster", "Rcpp", "terra"), verbose = 1)

  pizzR::setcreate.wd(output.path)
  for (i in seq(rfiles)) terra::writeRaster(terra::rast(rfiles[i]), overwrite = T,
                                            filename = sub('.tif', '', basename(rfiles[i])), filetype="ENVI",
                                            gdal=c(paste0('INTERLEAVE=', interleave), "BIGTIFF = IF_NEEDED", "COMPRESS = DEFLATE", "ZLEVEL = 9", ""))
}

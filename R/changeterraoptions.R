change.terraOptions <- function (tempdir=NULL,progress=3,verbose=F,...)
{
  if (!is.logical(verbose))         return(warning("'verbose' has to be of class logical!\n"))
  if (!is.numeric(progress))        return(warning("'progress' has to be of class integer!\n"))
  if (is.null(tempdir)){
    if (Sys.info()[1] == 'Windows') tempdir <- 'C:/temp/Rtmp'
    if (Sys.info()[1] == 'Linux')   tempdir <- '/tmp/Rtmp'
  }

  pizzR::package.install(c("memuse", "raster", "terra"), verbose = 1)
  require(terra)

  memmax <- memuse::Sys.meminfo()$totalram@size
  if (((memmax - 3)/memmax) > 0.9) {
    memfrac <- 0.9
  }
  else {
    memfrac <- ((memmax - 3)/memmax)
  }

  fparameters <- list(...)
  if (!is.null(tempdir)){
    pizzR::setcreatewd(tempdir)
    fparameters$tempdir <- tempdir
    Sys.setenv(TMP = tempdir, TEMP = tempdir)
  }
  fparameters$memfrac  <- memfrac
  fparameters$progress <- progress

  do.call(terra::terraOptions, fparameters)
  if (verbose) terra::terraOptions()
}

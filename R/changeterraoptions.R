change.terraOptions <- function (tempdir=NULL, progress=3, verbose=FALSE,...)
{
  stopifnot(is.logical(verbose),
            is.numeric(progress))
  if (is.null(tempdir)) tempdir <- if (Sys.info()[1] == 'Windows') 'C:/temp/Rtmp' else '/tmp/Rtmp'

  pizzR::package.install(c("memuse", "raster", "terra"), verbose = 1)
  require(terra)

  memmax                        <- memuse::Sys.meminfo()$totalram@size
  memfrac                       <- min(0.9, (memmax - 3) / memmax)

  fparameters                   <- list(...)
  if (!is.null(tempdir)){
    pizzR::setcreate.wd(tempdir)
    fparameters$tempdir <- tempdir
    Sys.setenv(TMP = tempdir, TEMP = tempdir)
  }
  fparameters$memfrac           <- memfrac
  fparameters$progress          <- progress

  do.call(terra::terraOptions, fparameters)
  if (verbose) terra::terraOptions()
}

change.rasterOptions <- function(tmpdir=NULL, OSRAM_remaining=3, progress='', verbose=FALSE, ...)
{
  stopifnot(is.logical(verbose),
            is.numeric(OSRAM_remaining))
  if (is.null(tempdir)) tempdir <- if (Sys.info()[1] == 'Windows') 'C:/temp/Rtmp' else '/tmp/Rtmp'

  pizzR::package.install(c("memuse", "raster", "terra"), verbose = 1)
  require(raster)

  maxmemory                     <- memuse::Sys.meminfo()$totalram@size
  memfrac                       <- min(0.9, (maxmemory - OSRAM_remaining) / maxmemory)

  fparameters                   <- list(...)
  if (!is.null(tmpdir)){
    pizzR::setcreate.wd(tmpdir)
    fparameters$tmpdir <- tmpdir
    Sys.setenv(TMP = tmpdir, TEMP = tmpdir)
  }
  fparameters$memfrac           <- memfrac
  fparameters$maxmemory         <- maxmemory * 1024^3
  fparameters$progress          <- progress

  do.call(raster::rasterOptions, fparameters)
  if (verbose) raster::rasterOptions()
}

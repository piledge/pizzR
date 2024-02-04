change.rasterOptions <- function(changetmpdir=F, tmpdir="", OSRAM.remaining=3, progress='', verbose=F, ...){

  if (!is.logical(changetmpdir)) return(warning("'changetmpdir' has to be of class logical!\n"))
  if (!is.logical(verbose)) return(warning("'verbose' has to be of class logical!\n"))

  pizzR::package.install(c("memuse", "raster", "Rcpp", "terra"))

  require(raster)

  if (changetmpdir && dir.exists(tmpdir) == F) {
    base::dir.create(tmpdir, recursive = T)
    base::cat(paste("\n", pizzR::Systime(), tmpdir, "created"))
  }

  maxmemory <- memuse::Sys.meminfo()$totalram@size
  if (((maxmemory-OSRAM.remaining) / maxmemory) > 0.9){
    memfrac <-  0.9
  }else{
    memfrac <- ((maxmemory - OSRAM.remaining) / maxmemory)
  }

  fparameters                  <- list(...)
  if (changetmpdir) fparameters$tmpdir           <- tmpdir
  fparameters$memfrac          <- memfrac
  fparameters$maxmemory        <- maxmemory * 1024^3
  fparameters$progress         <- progress

  do.call(raster::rasterOptions, fparameters)
  if (verbose) raster::rasterOptions()
  if (changetmpdir) Sys.setenv(TMP = tmpdir, TEMP = tmpdir)
}

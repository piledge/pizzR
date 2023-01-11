change.rasterOptions <- function(changetmpdir=FALSE, tmpdir="", OSRAM.remaining=3, progress="", verbose=FALSE, ...){

  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))


  require(raster)

  if (changetmpdir && dir.exists(tmpdir) == FALSE) {
    base::dir.create(tmpdir, recursive = TRUE)
    base::cat(paste("\n", Sys.time(), tmpdir, "created"))
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
}

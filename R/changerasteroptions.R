change.rasterOptions <- function(changetmpdir=FALSE, tmpdir="", OSRAM.remaining=3, progress="", verbose=FALSE, ...){

package.install <- function(c) {
  to_install <- !c %in% installed.packages()
  if (any(to_install)){
    cat(paste0(Sys.time(), ": install missing packages '", paste(c[to_install], collapse=", "), "'\n"))
    install.packages(c[to_install], dependencies = T)
    cat(paste0(Sys.time(), ": missing packages '", paste(c[to_install], collapse=", "), "' installed\n\n"))
  }
}
  package.install(c("memuse", "raster", "terra"))


  require(raster)

  if (changetmpdir == TRUE && dir.exists(tmpdir) == FALSE) {
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
  if (changetmpdir == T) fparameters$tmpdir           <- tmpdir
                         fparameters$memfrac          <- memfrac
                         fparameters$maxmemory        <- maxmemory * 1024^3
                         fparameters$progress         <- progress

  do.call(raster::rasterOptions, fparameters)
  if (verbose == TRUE) raster::rasterOptions()
}

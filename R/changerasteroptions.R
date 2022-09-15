#' change.rasterOptions() ##################################################################
#' to adjust raster-parameters
#' @example change.rasterOptions(TRUE, "Y:/tempfiles", 4)
#' @example change.rasterOptions(FALSE, "", 4)
#' BOOLEAN if tempfolder should be changed or not
#' new temporary folder for raster-operations
#' OSRAM.remaining for operating system in GByte

change.rasterOptions <- function(changetempfolder = FALSE, tempfolder = "", OSRAM.remaining = 3){
  
  package.install <- function(packages) {
    to_install <- !packages %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(packages, collapse=", "), "'\n"))
      install.packages(packages[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(packages, collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster"))
  
  
  require(raster)

  if (changetempfolder == TRUE && dir.exists(tempfolder) == FALSE) {
    base::dir.create(tempfolder, recursive = TRUE)
    base::cat(paste("\n", Sys.time(), tempfolder, "created"))
  }
  
  sys.memory <- memuse::Sys.meminfo()$totalram@size
  if (((sys.memory-OSRAM.remaining) / sys.memory) > 0.9){
    raster.memfrac <-  0.9
  }else{
    raster.memfrac  <-  ((sys.memory-OSRAM.remaining) / sys.memory)
  }

  raster::rasterOptions(tmpdir = tempfolder, memfrac = raster.memfrac, maxmemory = sys.memory * 1024^3, progress = "text")
  raster::rasterOptions()
}

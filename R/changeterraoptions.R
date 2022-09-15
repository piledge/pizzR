#' change.terraOptions() ##################################################################
#' to adjust terra-parameters
#' @example change.terraOptions(TRUE, "Y:/tempfiles", 4)
#' @example change.terraOptions(FALSE, "", 4)
#' BOOLEAN if tempfolder should be changed or not
#' new temporary folder for terra-operations
#' OSRAM.remaining for operating system in GByte

change.terraOptions <- function(changetempfolder = FALSE, tempfolder = "", OSRAM.remaining = 3){
  
  package.install <- function(packages) {
    to_install <- !packages %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(packages, collapse=", "), "'\n"))
      install.packages(packages[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(packages, collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "terra"))
  
  
  require(terra)

  if (changetempfolder == TRUE && dir.exists(tempfolder) == FALSE) {
    base::dir.create(tempfolder, recursive = TRUE)
    base::cat(paste("\n", Sys.time(), tempfolder, "created"))
  }
  
  sys.memory <- memuse::Sys.meminfo()$totalram@size
  if (((sys.memory-OSRAM.remaining) / sys.memory) > 0.9){
    terra.memfrac  <-  0.9
  }else{
    terra.memfrac <- ((sys.memory-OSRAM.remaining) / sys.memory)
  }
  
  terra::terraOptions(tempdir = tempfolder, memfrac = terra.memfrac, memmax = sys.memory, progress = 1)
  terra::terraOptions()
}

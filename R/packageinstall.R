#' package.install() ####################################################################
#' check if package is installed, install if not
#' @param package.name name of package
#' @example            package.install("raster")

package.install <- function(packages) {
  to_install <- !packages %in% installed.packages()
  if (any(to_install)){
    cat(paste0("\n", Sys.time(), ": install missing packages '", paste(packages, collapse=", "), "'\n"))
    install.packages(packages[to_install], dependencies = T)
    cat(paste0("\n", Sys.time(), ": missing packages '", paste(packages, collapse=", "), "' installed\n\n"))
  }
  else{
    cat(paste0("\n", Sys.time(), ": all packages installed\n\n"))
  }
}

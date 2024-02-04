.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\nPizzRtime!\n")
  pizzR::install.dependencies()
  library("magrittr")

}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(pizzRtime('red'))
  library("magrittr")
}

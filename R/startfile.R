if (length(grep('magrittr', rownames(installed.packages()))) == 0) install.packages("magrittr", dependencies = T)
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\nPizzRtime!\n")
  library("magrittr")
}

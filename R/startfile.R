.onAttach <- function(libname, pkgname) {
	packageStartupMessage(
	"\nPizzRtime!\n")
	library("magrittr")
	options(digits.secs = 0)
}

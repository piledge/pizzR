package.install <- function(x=pizzR::dependencies, verbose = 1){
  
  stopifnot(is.character(x), verbose %in% c(0, 1, 2))
  
  to_install <- !x %in% rownames(installed.packages())
  
  if (any(to_install)){
    missing_packages <- x[to_install]
    repo_packages <- rownames(available.packages())
    available_packages <- missing_packages[missing_packages %in% repo_packages]
    not_to_install <- !missing_packages %in% repo_packages
    unavailable_packages <- missing_packages[not_to_install]
    
    if (any(to_install) && length(available_packages) == 0) stop("Packages not available!")
    
    n_missing <- length(available_packages)
    if (verbose > 0) cat(sprintf("\n\nMissing packages: %s\n", paste(available_packages, collapse = ", ")))
    
    if (verbose > 0) cat("\nInstalling ...\n")
    for (i in seq(n_missing)){
      if (verbose > 0) cat(sprintf("%0*d of %d: %s\n", nchar(n_missing), i, n_missing, available_packages[i]))
      
      quiet <- TRUE
      if (verbose == 2) quiet <- FALSE
      install.packages(available_packages[i], quiet = quiet)
    }
    
    success <- (all(x %in% rownames(installed.packages())))
    if (success) cat("\nDone.\n\n")
    if (!success){
      to_install <- !x %in% rownames(installed.packages())
      missing_packages <- as.character(x[to_install])
      warning(sprintf("\n\nPackages not able to install: %s\n\n", paste(missing_packages, collapse = ", ")))
    }
  }
}

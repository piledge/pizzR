package.install <- function(x=pizzR::dependencies, verbose = 1){

  if (!is.character(class(x)))  return(warning("'x' has to be of class character!"))
  if (!verbose %in% c(0, 1, 2)) return(warning("'verbose' can either be 0 (no messages), 1 (installation status) or 2 (full output)!"))

  to_install <- !x %in% rownames(installed.packages())

  if (any(to_install)){
    missing_packages <- as.character(x[to_install])
    repo_packages <- rownames(available.packages())
    available_packages <- missing_packages[missing_packages %in% repo_packages]
    not_to_install <- !missing_packages %in% repo_packages
    unavailable_packages <- missing_packages[not_to_install]

    if (any(to_install) && length(available_packages) == 0) return(warning("Packages not available!"))

    n.missing <- length(available_packages)
    nchar.missing <- nchar(n.missing)
    if (verbose > 0) cat("\n\nMissing packages:", paste(available_packages, sep = ',', collapse= ", "), "\n")

    if (verbose > 0) cat("\nInstalling ...\n")
    for (i in seq(n.missing)){
      if (verbose > 0) cat(sprintf(paste0("%0", nchar.missing, ".f"), i), 'of', n.missing, ':', available_packages[i], '\n')

      quiet <- TRUE
      if (verbose == 2) quiet <- FALSE
      install.packages(available_packages[i], quiet = quiet)
    }

    success <- (all(x %in% rownames(installed.packages())))
    if (success) cat("\nDone.\n\n")
    if (!success){
      to_install <- !x %in% rownames(installed.packages())
      missing_packages <- as.character(x[to_install])
      warning(cat("\n\nPackages not able to install:", paste(missing_packages, sep = ',', collapse= ", "), "\n\n"))
    }
  }
}

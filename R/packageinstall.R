package.install <- function(x=pizzR::dependencies, verbose = F){
  
  to_install <- !x %in% rownames(installed.packages())
  
  if (any(to_install)){
    missing_packages <- as.character(x[to_install])
    n.missing <- length(missing_packages)
    nchar.missing <- nchar(n.missing)
    cat("\n\nInstall packages:", paste(missing_packages, sep = ',', collapse= ", "), "\n")

    cat("\nInstalling ...\n")
    for (i in seq(n.missing)){
      cat(sprintf(paste0("%0", nchar.missing, ".f"), i), 'of', n.missing, ':', missing_packages[i], '\n')
      install.packages(missing_packages[i], x = F, quiet = !verbose)
    }
    
    success <- (all(x %in% rownames(installed.packages())))
    if (success) cat("\nDone.\n\n")
    if (!success){
      to_install <- !x %in% rownames(installed.packages())
      missing_packages <- as.character(x[to_install])
      cat("\n\nPackages not able to install:", paste(missing_packages, sep = ',', collapse= ", "), "\n\n")
    }
  }else{
    cat("\nAll packages are installed\n\n")
  }
}

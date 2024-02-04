install.dependencies <- function(){
  
  to_install <- !dependencies %in% rownames(installed.packages())
  
  if (any(to_install)){
    missing_dependencies <- as.character(dependencies[to_install])
    n.missing <- length(missing_dependencies)
    nchar.missing <- nchar(n.missing)
    
    if (n.missing == 1) cat("\n\nDependencies missing:", paste0(missing_dependencies, sep=""), "\n")
    if (n.missing != 1) cat("\n\nDependencies missing:", paste0(missing_dependencies, sep=","), "\n")
    
    cat("\nInstalling ...\n")
    for (i in seq(n.missing)){
      cat(sprintf(paste0("%0", nchar.missing, ".f"), i), 'of', n.missing, ':', missing_dependencies[i], '\n')
      install.packages(missing_dependencies[i], dependencies = F, quiet = T)
    }
    
    success <- (all(dependencies %in% rownames(installed.packages())))
    if (success) cat("\nDone.\n")
    if (!success){
      to_install <- !dependencies %in% rownames(installed.packages())
      missing_dependencies <- as.character(dependencies[to_install])
      n.missing <- length(missing_dependencies)
      if (n.missing == 1) cat("\n\nDependencies still missing:", paste0(missing_dependencies, sep=""), "\n")
      if (n.missing != 1) cat("\n\nDependencies still missing:", paste0(missing_dependencies, sep=","), "\n")
      cat("\nTry to install manually.\n\n")
    }
  }else{
    cat("\nDependencies meet the requirements\n")
  }
}

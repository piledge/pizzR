translate <- function(x,dictionary=NULL,verbose=F){
  colnames_dictionary <- colnames(dictionary)
  if (is.null(dictionary)) return(warning("No suitable dictionary!\n"))
  if (!all(c('new', 'old') %in% colnames_dictionary)) return(warning("No suitable dictionary!\n"))
  if (!is.logical(verbose))                           return(warning("'verbose' has to be of class logical!\n"))

  loops <- seq_len(nrow(dictionary))
  nchar.loops <- nchar(max(loops))
  colnr_old <- grep("old", colnames_dictionary)
  colnr_new <- grep("new", colnames_dictionary)

  if (!verbose){
    for (i in loops){
      x[x == dictionary[i, colnr_old]] <- dictionary[i, colnr_new]
    }
  }
  if (verbose){
    for (i in loops){
      base::cat(sprintf(paste0("\r %s: remaining items to translate: % ", nchar.loops, "s"),
                        pizzR::Systime(), loops - i + 1))
      x[x == dictionary[i, colnr_old]] <- dictionary[i, colnr_new]
    }
  }
  if (verbose){
    base::cat(sprintf(paste0("\r %s: %", nchar.loops + 13, "s items translated\n"),
                      pizzR::Systime(), max(loops)))
  }
  return(x)
}


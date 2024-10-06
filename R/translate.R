translate <- function(x,dictionary=NULL,verbose=F){
  colnames_dictionary <- colnames(dictionary)
  stopifnot(!is.null(dictionary),
            all(c('new', 'old') %in% colnames_dictionary),
            is.logical(verbose))

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
      cat(sprintf("\r %s: remaining items to translate: %*d", pizzR::Systime(), nchar_loops, loops - i + 1))
      x[x == dictionary[i, colnr_old]] <- dictionary[i, colnr_new]
    }
  }
  if (verbose) cat(sprintf("\r %s: %*d items translated\n", pizzR::Systime(), nchar_loops + 13, max(loops)))
  return(x)
}


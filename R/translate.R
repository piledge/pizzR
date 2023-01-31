translate <- function(x,y,verbose=F){
  
  if (length(grep("new",colnames(y))) != 1 | length(grep("old",colnames(y))) != 1) return(warning("No suitable dictionary!\n"))
  if (!is.logical(verbose)) return(warning("'verbose' has to be of class logical!\n"))
  
  loops <- seq_len(nrow(y))
  nchar.loops <- nchar(max(loops))
  colnames_y <- colnames(y)
  colnr_old <- grep("old", colnames_y)
  colnr_new <- grep("new", colnames_y)
  
  if (!verbose){
    for (i in loops){
      x[x == y[i, colnr_old]] <- y[i, colnr_new]
    }
  }
  if (verbose){
    for (i in loops){
      base::cat(sprintf(paste0("\r %s: remaining items to translate: % ", nchar.loops, "s"),
                        Sys.time(), loops - i))
      x[x == y[i, colnr_old]] <- y[i, colnr_new]
    }
  }
  if (verbose){
    base::cat(sprintf(paste0("\r %s: %", nchar.loops + 13, "s items translated\n"),
                      Sys.time(), loops))
  }
  return(x)
}

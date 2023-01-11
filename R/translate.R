translate <- function(x,y,verbose=FALSE){
  if (length(grep("new",colnames(y))) != 1 | length(grep("old",colnames(y))) != 1) return(warning("No suitable dictionary!\n"))

  loops <- nrow(y)
  nchar.loops <- nchar(loops)
  colnames_y <- colnames(y)

  if (verbose){
    for (i in 1 : loops){
      x[x == y[i, grep("old", colnames_y)]] <- y[i, grep("new", colnames_y)]
    }
  }
  if (verbose){
    for (i in 1 : loops){
      Sys.sleep(0.2)
      base::cat(sprintf(paste0("\r %s: remaining items to translate: % ", nchar.loops, "s"),
                  Sys.time(), loops - i))
      x[x == y[i, grep("old", colnames_y)]] <- y[i, grep("new", colnames_y)]
      }
  }
  if (verbose){
    base::cat(sprintf(paste0("\r %s: %", nchar.loops+13, "s items translated\n"),
                Sys.time(), loops))
  }
  return(x)
}

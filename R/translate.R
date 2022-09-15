translate <- function(data, dictionary, verbose = TRUE){

  loops <- nrow(dictionary)
  nchar.loops <- nchar(loops)
  colnames.dictionary <- colnames(dictionary)

  if (verbose == FALSE){
    for (i in 1 : loops){
      data[data == dictionary[i, grep("old", colnames.dictionary)]] <- dictionary[i, grep("new", colnames.dictionary)]
    }
  }
  if (verbose == TRUE){
    for (i in 1 : loops){
      Sys.sleep(0.2)
      base::cat(sprintf(paste0("\r %s: remaining items to translate: % ", nchar.loops, "s"),
                  Sys.time(), loops - i))
      data[data == dictionary[i, grep("old", colnames.dictionary)]] <- dictionary[i, grep("new", colnames.dictionary)]
      }
  }
  if (verbose == TRUE){
    base::cat(sprintf(paste0("\r %s: %", nchar.loops+13, "s items translated\n"),
                Sys.time(), loops))
  }
  return(data)
}

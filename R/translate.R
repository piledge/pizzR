#' translate() ##########################################################################
#' can be used to translate values in a row by use of a dictionary
#' similar purpose than merge, but better performance
#' @param data       row in dataframe which has to be translated
#' @param dictionary has to be a dataframe with rows "old" and "new" representing the link between the values to be replaced
#' @example          translate(data$replaced,dictionary)

translate <- function(data, dictionary, output=TRUE){
  for (i in 1 : nrow(dictionary)){
    if (i < nrow(dictionary) & output == TRUE){
      base::cat(paste("\r", Sys.time(), "Remaining items to translate:", nrow(dictionary) - i, "                                          "))
    }

    data[data == dictionary[i, grep("old", colnames(dictionary))]] <- dictionary[i, grep("new", colnames(dictionary))]

    if (i == nrow(dictionary) & output == TRUE){
      base::cat(paste("\r", Sys.time(), nrow(dictionary), "items translated                                              "))
    }
  }
  return(data)
}

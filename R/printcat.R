#' clear.cat() ###########################################################################
#' can be used instead of cat()-funktion. Deletes the whole console before the output to clean up the run
#' @param print.text string to print on console
#' @example          clr.cat(paste("Loop",i))
#' @example          clr.cat("This output will be written on the console")

clr.cat <- function(print.text = ""){
  base::cat("\f")
  base::cat(as.character(paste0(Sys.time(),": ",print.text,"                                                                       ")))
}

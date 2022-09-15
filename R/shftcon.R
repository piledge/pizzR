#' shft.con() ###########################################################################
#' can be used to move up the console x lines
#' @param   n.lines number of lines to move console up
#' @example shft.con(50)

shft.con <- function(n.lines=50){
  base::cat(rep("\n", n.lines))
}

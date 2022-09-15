#' plot.rstshp() ###########################################################################
#' can be used to plot a rasterlayer + overlaid shapefile by using just a single command
#' @param rasterlayer rasterlayer to plot
#' @param shapefile   shapefile to overlay
#' @example           plot.rstshp(rasterlayer, shapefile)

plot.rstshp <- function(rasterlayer, shapefile){
  plot(rasterlayer)
  plot(shapefile, add = TRUE)
}

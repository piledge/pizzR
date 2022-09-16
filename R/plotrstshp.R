plot.rstshp <- function(x, y){
  terra::plot(x)
  terra::plot(y, add = TRUE)
}

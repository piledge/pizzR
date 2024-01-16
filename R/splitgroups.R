split.groups <- function(x, ranges){

  table.x <- table(x)
  polygons.names <- as.numeric(names(table.x))
  polygons.counts <- as.numeric(table.x)

  groups <- pizzR::split.vector(polygons.names, ranges)
  dict <- data.frame(old=polygons.names, new=groups)

  return(pizzR::translate(x, dict))
}

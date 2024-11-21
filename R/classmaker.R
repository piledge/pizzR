classmaker <- function(x, class_range = 50, include.lowest = TRUE, right = FALSE) {
  max_value <- ceiling(max(x, na.rm = TRUE) / class_range) * class_range
  breaks <- seq(0, max_value, by = class_range)
  classes <- as.integer(cut(x, breaks = breaks, include.lowest = include.lowest, right = right))
  classes[is.na(classes)] <- 0
  
  return(classes)
}

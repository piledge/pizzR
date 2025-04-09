species_percentage <- function (rst, shp, classes='class', classnames = NULL, digits = NULL){
  extr <- terra::extract(rst, shp)
  extr <- extr[complete.cases(extr), ]

  class_col <- which(classes == colnames(extr))
  unique_polys <- unique(extr$ID)
  n_polys <- length(unique_polys)
  n_classes <- length(unique(extr[, class_col]))

  extr$ID <- factor(extr$ID, levels = unique_polys)
  extr[, class_col] <- factor(extr[, class_col], levels = 1:n_classes)

  tbl <- table(extr$ID, extr[, class_col])
  percent_tbl <- sweep(tbl, 1, rowSums(tbl), FUN = "/") * 100
  res <- as.data.frame.matrix(percent_tbl)
  colnames(res) <- paste0("class_", 1:n_classes)

  if (!is.null(digits))
    res <- round(res, digits)
  if (!is.null(classnames))
    colnames(res) <- classnames
  return(res)
}

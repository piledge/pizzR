species_percentage <- function(rst, shp, classnames = NULL) {

  extr <- terra::extract(rst, shp)
  extr <- extr[complete.cases(extr), ]

  unique_polys <- unique(extr$ID)
  n_polys <- length(unique_polys)
  n_classes <- length(unique(extr$NPDA_S2_PS_map_masked))

  extr$ID <- factor(extr$ID, levels = unique_polys)
  extr$NPDA_S2_PS_map_masked <- factor(extr$NPDA_S2_PS_map_masked, levels = 1:n_classes)

  tbl <- table(extr$ID, extr$NPDA_S2_PS_map_masked)

  percent_tbl <- sweep(tbl, 1, rowSums(tbl), FUN = "/") * 100

  res <- as.data.frame.matrix(percent_tbl)
  colnames(res) <- paste0("class_", 1:n_classes)

  if (!is.null(classnames)) colnames(res) <- classnames

  return(res)
}

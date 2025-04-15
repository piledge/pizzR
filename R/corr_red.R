corr_red <- function(data, classes = 'class', cutoff = 0.8) {
  pizzR::package.install('caret')
  
  classes_col <- which(colnames(data) %in% classes)
  id_col <- which(colnames(data) %in% 'ID')
  stopifnot(cutoff >= 0, cutoff <= 1, length(classes_col) <= 1, length(id_col) <= 1)
  
  cor_data <- data[, -c(id_col, classes_col), drop = FALSE]
  cor_matrix <- cor(cor_data)
  
  high_corr_idx <- caret::findCorrelation(cor_matrix, cutoff = cutoff)
  low_cor <- if (length(high_corr_idx) > 0) cor_data[, -high_corr_idx, drop = FALSE] else cor_data
  
  small_corr <- cbind(data[, id_col, drop = FALSE], data[, classes_col, drop = FALSE], low_cor)
  return(small_corr)
}

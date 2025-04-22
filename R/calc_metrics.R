calc_metrics <- function (data, id = "ID", verbose = FALSE)
{
  id_col <- which(names(data) == id)
  unique_ids <- unique(data[[id]])
  n_samples <- length(unique_ids)
  nchar_n_samples <- nchar(n_samples)
  metric_names <- c("005q", "010q", "015q", "020q", "025q",
                    "030q", "035q", "040q", "045q", "050q",
                    "055q", "060q", "065q", "070q", "075q",
                    "080q", "085q", "090q", "095q", "iqr", "mean", "sd")
  metrics_names_rep <- paste(rep(colnames(data)[-id_col], length(metric_names)),
                             rep(metric_names, each = ncol(data) - 1), sep = "_")
  
  iqr_apply <- function(x)  return(matrix(unlist(lapply(x, IQR))))
  
  res <- data.frame((matrix(NA, nrow = n_samples, ncol = length(metrics_names_rep))))
  colnames(res) <- metrics_names_rep
  for (i in seq_along(unique_ids)) {
    if (verbose) pizzR::loop_progress(i, total_digits = nchar_n_samples)
    data_subs <- na.omit(data[ data[[id]] == i, -id_col, drop = FALSE])
    metrics <- c(pizzR::quapply(data_subs, 0.05),
                 pizzR::quapply(data_subs, 0.10),
                 pizzR::quapply(data_subs, 0.15),
                 pizzR::quapply(data_subs, 0.20),
                 pizzR::quapply(data_subs, 0.25),
                 pizzR::quapply(data_subs, 0.30),
                 pizzR::quapply(data_subs, 0.35),
                 pizzR::quapply(data_subs, 0.40),
                 pizzR::quapply(data_subs, 0.45),
                 pizzR::quapply(data_subs, 0.50),
                 pizzR::quapply(data_subs, 0.55),
                 pizzR::quapply(data_subs, 0.60),
                 pizzR::quapply(data_subs, 0.65),
                 pizzR::quapply(data_subs, 0.70),
                 pizzR::quapply(data_subs, 0.75),
                 pizzR::quapply(data_subs, 0.80),
                 pizzR::quapply(data_subs, 0.85),
                 pizzR::quapply(data_subs, 0.90),
                 pizzR::quapply(data_subs, 0.95),
                 iqr_apply(data_subs),
                 colMeans(data_subs),
                 apply(data_subs, 2, sd))
    
    res[i, ] <- c(metrics)
  }
  res <- data.frame(ID=unique_ids, res)
  gc(reset = T, full = T)
  return(res)
}

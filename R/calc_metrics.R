calc_metrics <- function (data, id = "ID")
{
  id_col <- which(names(data) == id)
  unique_ids <- unique(data[[id]])
  n_samples <- length(unique_ids)
  nchar_n_samples <- nchar(n_samples)
  metric_names <- c("005q", "010q", "015q", "020q", "025q",
                    "030q", "035q", "040q", "045q", "050q",
                    "055q", "060q", "065q", "070q", "075q",
                    "080q", "085q", "090q", "095q", "mean", "sd")
  metrics_names_rep <- paste(rep(colnames(data)[-id_col], length(metric_names)),
                             rep(metric_names, each = ncol(data) - 1), sep = "_")

  res <- data.frame((matrix(NA, nrow = n_samples, ncol = length(metrics_names_rep))))
  colnames(res) <- metrics_names_rep
  for (i in seq_along(unique_ids)) {
    pizzR::loop_progress(i, total_digits = nchar_n_samples)
    data_subs <- na.omit(data[data$ID == i, -id_col])
    metrics <- matrix(rbind(pizzR::quapply(data_subs, 0.05),
                            pizzR::quapply(data_subs, 0.1),
                            pizzR::quapply(data_subs, 0.15),
                            pizzR::quapply(data_subs, 0.2),
                            pizzR::quapply(data_subs, 0.25),
                            pizzR::quapply(data_subs, 0.3),
                            pizzR::quapply(data_subs, 0.35),
                            pizzR::quapply(data_subs, 0.4),
                            pizzR::quapply(data_subs, 0.45),
                            pizzR::quapply(data_subs, 0.5),
                            pizzR::quapply(data_subs, 0.55),
                            pizzR::quapply(data_subs, 0.6),
                            pizzR::quapply(data_subs, 0.65),
                            pizzR::quapply(data_subs, 0.7),
                            pizzR::quapply(data_subs, 0.75),
                            pizzR::quapply(data_subs, 0.8),
                            pizzR::quapply(data_subs, 0.85),
                            pizzR::quapply(data_subs, 0.9),
                            pizzR::quapply(data_subs, 0.95),
                            colMeans(data_subs),
                            apply(data_subs, 2, sd)),
                            nrow = length(metric_names))
    res[i, ] <- c(metrics)
  }
  return(res)
}

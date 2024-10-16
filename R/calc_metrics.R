calc_metrics <- function(extr, id='ID'){

  id_col <- grep(id, names(extr))
  n_samples <- length(unique(extr[,id_col]))
  nchar_n_samples <- nchar(n_samples)
  metric_names <- c('005q', '010q', '015q', '020q', '025q', '030q', '035q', '040q', '045q', '050q', '055q', '060q', '065q', '070q', '075q', '080q', '085q', '090q', '095q', 'mean', 'sd')

  metrics_names_rep <- paste(rep(colnames(extr)[-id_col], 21), rep(metric_names, each = ncol(extr)-1), sep = '_')
  res <- data.frame((matrix(NA, nrow = n_samples, ncol = length(metrics_names_rep))))
  colnames(res) <- metrics_names_rep

  for (i in unique(data$ID)){
    cat(sprintf('%s: ID %0*d\r', pizzR::Systime(), nchar_n_samples, i))
    extr_subs <- extr[extr$ID==i, -id_col]

    metrics <- matrix(rbind(pizzR::quapply(extr_subs, .05), pizzR::quapply(extr_subs, .10), pizzR::quapply(extr_subs, .15), pizzR::quapply(extr_subs, .20), pizzR::quapply(extr_subs, .25),
                            pizzR::quapply(extr_subs, .30), pizzR::quapply(extr_subs, .35), pizzR::quapply(extr_subs, .40), pizzR::quapply(extr_subs, .45), pizzR::quapply(extr_subs, .50),
                            pizzR::quapply(extr_subs, .55), pizzR::quapply(extr_subs, .60), pizzR::quapply(extr_subs, .65), pizzR::quapply(extr_subs, .70), pizzR::quapply(extr_subs, .75),
                            pizzR::quapply(extr_subs, .80), pizzR::quapply(extr_subs, .85), pizzR::quapply(extr_subs, .90), pizzR::quapply(extr_subs, .95), colMeans(extr_subs), apply(extr_subs, 2, sd)), nrow = length(metric_names))

    res[i,] <- c(metrics)
  }
  return(res)
}

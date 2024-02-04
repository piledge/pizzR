rfFeatsel <- function(x, y, fs_seq = NULL, keep.models = TRUE, savename='D:/', savedir = NULL, best_thr = .975, ...){

  pizzR::package.install(c("randomForest"), verbose = 1)
  
  OOB_OA <- function(x){
    x <- with(x, confusion[, seq_len(nrow(confusion))])
    sum(diag(x)) / sum(x)
  }
  
  dots            <- list(...)
  dots$x          <- x
  dots$y          <- as.factor(y) 
  dots$importance <- TRUE
  dots$keep.inbag <- TRUE
  dots$na.action  <- na.omit
  
  rf_mod          <- do.call(randomForest::randomForest, dots)
  
  if (is.null(fs_seq))  fs_seq  <- seq_len(ncol(x))
  if (is.null(savedir)) savedir <- "."
  if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)
  
  fs_seq          <- sort(fs_seq, decreasing = TRUE)
  out             <- numeric(length = length(fs_seq))
  names(out)      <- fs_seq
  N               <- max(nchar(fs_seq))
  
  if (rf_mod$type == 'classification'){
    imp             <- sort(randomForest:::importance(rf_mod)[, 'MeanDecreaseAccuracy'], decreasing = TRUE)
    rf_type         <- 'MDA'
  } else {
    imp             <- sort(randomForest:::importance(rf_mod)[, '%IncMSE'], decreasing = TRUE)
    rf_type         <- 'IncMSE'
  }
  
  ind             <- 0
  
  for (i in fs_seq){
    
    ind       <- ind + 1
    
    dots$x    <- dots$x[, names(imp)[seq_len(i)], drop = FALSE]
    rf_submod <- do.call(randomForest::randomForest, dots)
      
    save(rf_submod,
         file = sprintf(paste0("%s/%s_fs_%s_bw_%0", N, "d.Rdata"),
                        savedir, savename, rf_type, i))
      
    
    if (rf_submod$type == "classification"){
      imp       <- sort(randomForest:::importance(rf_submod)[, 'MeanDecreaseAccuracy'], decreasing = TRUE)
      # out[ind]  <- with(rf_submod, sum(diag(confusion)) / sum(confusion[, -ncol(confusion)]))
      out[ind]  <- OOB_OA(rf_submod)
    } else {
      imp       <- sort(randomForest:::importance(rf_submod)[, '%IncMSE'], decreasing = TRUE)
      out[ind]  <- mean(rf_submod$rsq)
    }
    cat("               \r", i)
  }
  
  write.table(data.frame(names(out), out),
              file = sprintf("%s/%s_fs_%s_bw_mean_rsq.Rdata",
                             savedir, savename, rf_type))
  
  if (is.null(best_thr)){
    out_max <- out[which.max(out)]
  } else {
    best_thr <- quantile(out, probs = best_thr)
    out_max  <- out[max(which(out >= best_thr))]
  }
  
  load(file = sprintf(paste0("%s/%s_fs_%s_bw_%0", N, "d.Rdata"),
                      savedir, savename, rf_type, as.integer(names(out_max))))
  rf_fs_best <- rf_submod
  
  save(rf_fs_best,
       file = sprintf(paste0("%s/%s_fs_%s_bw_best.Rdata"),
                      savedir, savename, rf_type))
  
  if (!isTRUE(keep.models)){
    file.remove(list.files(savedir, pattern = sprintf("_fs_%s_bw_[0-9]+[.]Rdata$", rf_type), full.names = TRUE))
  }  
  if (rf_submod$type == "classification"){
    return(list(OOB_OA = out, rf_fs_best = rf_fs_best))
  } else {
    return(list(mrsq = out, rf_fs_best = rf_fs_best))
  }
}

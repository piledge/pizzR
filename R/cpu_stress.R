stress_cpu <- function(n_cores = NULL, dur_s = NULL) {
  pizzR::package.install(c("foreach", "parallel", "doParallel"), verbose = 1)
  
  library(foreach)
  
  if (is.null(n_cores)) n_cores <- parallel::detectCores() - 1
  stopifnot(is.numeric(n_cores), n_cores > 0)
  
  if (!is.null(dur_s)) {
    stopifnot(is.numeric(dur_s), dur_s > 0)
    dur_s <- as.integer(dur_s)
  }
  
  clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(clust)
  
  if (is.null(dur_s)) {
    foreach(i = 1:n_cores) %dopar% {
      while(TRUE) 0.7 / 0.3
    }
  }

  if (!is.null(dur_s)) {
    end_time <- Sys.time() + dur_s
    foreach(i = 1:n_cores) %dopar% {
      while(Sys.time() < end_time) 0.7 / 0.3
    }
  }

  parallel::stopCluster(clust)
}

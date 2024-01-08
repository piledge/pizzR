stress_cpu <- function(n_cores=NULL,dur_s=NULL){

  library(foreach)
  if (is.null(n_cores)) n_cores <- (parallel::detectCores() - 1)
  if (!is.numeric(n_cores)) return(warning('n_cores has to be integer!'))

  if (!is.null(dur_s)){
    if (!is.numeric(dur_s)) return(warning('dur_s has to be integer!'))
    dur_s <- as.integer(dur_s)
  }

  clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(clust)

  if (is.null(dur_s)){
    foreach(i=1:n_cores) %dopar% {
      while(T) 0.7/0.3
    }
  }

  if (!is.null(dur_s)){
    st <- Sys.time() + dur_s
    foreach(i=1:n_cores) %dopar% {
      while(Sys.time() < st) 0.7/0.3
    }
  }

  parallel::stopCluster(clust)
}

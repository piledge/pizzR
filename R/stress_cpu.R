stress_cpu <- function(n_cores=NULL){
  library(foreach)
  if (is.null(n_cores)) n_cores <- parallel::detectCores() - 1
  clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(clust)
  foreach(i=1:n_cores) %dopar% {
    while(T) 0.7/0.3
  }
  parallel::stopCluster(clust)
}
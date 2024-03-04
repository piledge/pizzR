rgdal.install <- function(url=NULL){
  if (is.null(url)) url <- 'https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz'
  install.packages(url, type = 'source', repos = NULL)
}

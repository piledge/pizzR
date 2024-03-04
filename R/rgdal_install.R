rgdal.install <- function(url=NULL, quiet = T){
  if (!is.logical(quiet))         return(warning("'quiet' has to be of class logical!\n"))

  if (is.null(url)) url <- 'https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz'
  install.packages(url, type = 'source', repos = NULL, quiet = quiet)

  successful <- 'rgdal' %in% rownames(installed.packages())
  if (successful)   warning('Depreciated rgdal-package successfully installed! Use alternatives sf and terra instead!')
  if (!successful)  warning('Error during installation occured')
}

rgdal.install()




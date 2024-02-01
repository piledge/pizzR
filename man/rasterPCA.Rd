\name{rst.pca}
\alias{rst.pca}
\title{Calculate principle components of a Rasterfile}
\usage{
  rst.pca()
}
\arguments{
  \item{x}{Object of type SpatialRaster.}
  \item{scale}{Scaling of input-variables.}
}
\description{
  Calculate principle components of a Rasterfile. This function is able to deal with rasterfiles containing NA-values.
  Currently only the raster is returned. Future updates may contain also further metrics.
}
\examples{
  library(pizzR)
  library(terra)

  ### load sample-raster
  i.path <- system.file('extdata/rastersample.tif', package="pizzR")
  r <- rast(i.path)
  plot(r)

  ### shoot out some pixels and replace by NA-values
  na.cells20 <- sample(ncell(r), ncell(r) * 0.2, replace = T)
  values(r)[na.cells20, ] <- NA
  plot(r)

  # calculate raster-pca
  pca.model <- rst.pca(r)
  plot(pca.model)
}

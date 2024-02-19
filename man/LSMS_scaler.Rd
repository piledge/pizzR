\name{LSMS.scaler}
\alias{LSMS.scaler}
\title{Scale SpatRaster-Object for LSMS-Segmentation.}
\usage{
  LSMS.scaler()
}
\arguments{
  \item{rasterobject}{SpatRaster. Object of type SpatialRaster.}
  \item{maxval}{Integer. Default is 255 (Values will be scaled from 0 to 255).}
}
\description{
  Scale SpatRaster-Object for LSMS-Segmentation
}
\examples{
  library(pizzR)
  library(terra)

  # prepare SpatRaster for LSMS-segmentation
  r.scaled <- LSMS.scaler(r)
  plot(r.scaled)
}

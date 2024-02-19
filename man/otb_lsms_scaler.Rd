\name{OTB_lsms_scaler}
\alias{OTB_lsms_scaler}
\title{Scale SpatRaster-Object for LSMS-Segmentation.}
\usage{
  OTB_lsms_scaler()
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
  r.scaled <- OTB_lsms_scaler(r)
  plot(r.scaled)
}

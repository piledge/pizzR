\name{get.crsparams}
\alias{get.crsparams}
\title{Get Proj4-projection parameters of CRS.}
\usage{
  get.crsparams()
}
\arguments{
  \item{get.crsparams}{EPSG-Code of Coordinate Reference System.}
}
\description{
  Get Proj4-projection parameters of a Coordinate Reference System by the CRS EPSG-Code.
}
\examples{
  library(pizzR)

  # get parameters
  get.crsparams(32633)
  get.crsparams(31287)
}

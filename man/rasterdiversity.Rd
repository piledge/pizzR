\name{rasterdiversity}
\alias{rasterdiversity}
\title{Calculate common biodiversity indices of a Rasterfile}
\usage{
  rasterdiversity()
}
\arguments{
  \item{x}{Object of type SpatialRaster.}
  \item{index}{string. Index can either be 'eveness', 'raosq', 'richness', 'shannon' or 'simpson'.}
  \item{win_xy}{integer. Size of focal window in xy. Note: z-direction of cube will be calculated by \code{nlyr(x)}.}
  \item{method}{string. Distance-Method for RaosQ. Can either be 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'. Default is 'euclidean'.}
}
\description{
  Calculate common biodiversity indices of a Rasterfile. For Raos Q also multispectral rasterfiles can be used.
}
\examples{
  library(pizzR)
  library(terra)

  ### load sample-raster
  i.path <- system.file('extdata/sentinel2_sample.tif', package="pizzR")
  r <- rast(i.path)
  plot(r[[1:4]])

  ### calculate shannon-index (only monospektral)
  shannon <- rasterdiversity(r[[1]], index = 'shannon', win_xy = 3)

  ### calculate raosq-index (multispectral)
  raosq.mono <- rasterdiversity(r[[1]], index = 'raosq', win_xy = 3)

  ### calculate raosq-index (multispectral)
  raosq.selected <- rasterdiversity(r[[c(1,2,3,7)]], index = 'raosq', win_xy = 3)

  ### calculate raosq-index (multispectral, bands selected)
  raosq.multi <- rasterdiversity(r, index = 'raosq', win_xy = 3)

  ### plot results
  par(mfrow = c(2, 2))
  terra::plot(shannon, main = "Shannon H'")
  terra::plot(raosq.mono, main = "RaosQ mono")
  terra::plot(raosq.selected, main = "RaosQ selected")
  terra::plot(raosq.multi, main = "RaosQ multi")

}

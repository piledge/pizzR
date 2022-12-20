\name{rastertiler}
\alias{rastertiler}
\title{Split extent of a SpatialRaster in n tiles}
\usage{
rastertiler()
}
\arguments{
\item{x}{SpatialRaster.}
\item{nslices_x}{integer. Horizontal number of slices}
\item{nslices_y}{integer. Vertical number of slices}
\item{overlap_x}{integer. Number of overlapping pixels in horizontal direction}
\item{overlap_y}{integer. Number of overlapping pixels in vertical direction}
\item{verbose}{logical. Plot additional output}
}
\description{
Returns a list including the extent of the input raster, split in a number of subsets. If required, an overlapping area for the tiles can be specified.
If \code{is.integer(nrow / nslices_x) == F} or \code{is.integer(ncol / nslices_y == F}, the remaining pixels will be added to the right and bottom tiles.
}
\examples{
library(terra)
library(pizzR)

# define a SpatialRaster
r <- rast(nrows=10, ncols=10, vals=sqrt(sample(100, 100)))


# slice the SpatialRaster tiles
tiles <- rastertiler(r, nslices_x = 3, nslices_y = 4, verbose=T))
tiles

# slice the SpatialRaster tiles with 3 pixels larger in size
tiles <- rastertiler(r, nslices_x = 3, nslices_y = 4, overlap_x = 3, overlap_y = 3, verbose=T))
tiles


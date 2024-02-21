\name{OTB_lsms}
\alias{OTB_lsms}
\title{Wrapper function to execute OTB's LSMS conveniently from R.}
\usage{
OTB_lsms(IMGpath,OTBpath,savedir, ...)
}
\arguments{
\item{IMGpath}{character. Path to the (input image) file.}
\item{OTBpath}{character. Path to OTB-X.X.X-win64 or OTB-X.X.X-debian or OTB-X.X.X-Linux64.
  If not accessible, newest version in specified parent directory will be used.}
\item{savedir}{character. output directory to store results. If \code{NULL}
  (the default), a folder will be created in the paretn directory of IMGpath.
  If savedir does not exist, an attempt is made to create it.}
\item{spatrad}{integer. Vector spatial radius of LSMS.}
\item{rangerad}{numeric. Vector range radius of LSMS.}
\item{minsize}{integer. Vector minimum object size in the SmallRegionsMerging.}
\item{maxiter}{integer. Maximum number of iterations in LSMS.}
\item{thres}{numeric. Mode convergence threshold of LSMS.}
\item{tilesizex}{integer. Tile size in x-direction.}
\item{tilesizey}{integer. Tile size in y-direction.}
\item{vectorize}{logical. should the segmentation result be exported in ESRI Shapefile format?}
\item{resume}{logical. if \code{TRUE} (the default) only parameter combinations are used,
  for which no results are found in savedir.}
\item{Ncore}{integer. Maximum number of threads to use.}
\item{ram}{integer. Amount of memory (in MB) to be made available for OTB.}
}
\description{
Wrapper function to execute OTB's LSMS conveniently from R. If more than one
level is supplied for the parameters 'spatrad', 'rangerad', 'minsize' or
'fact' segmentation will be run for each unique combination.
Make sure OTB is properly installed! (visit https://www.orfeo-toolbox.org
for download and instructions on installation).
}
\examples{
library(pizzR)
library(terra)

i.path <- system.file('extdata/rastersample.tif', package="pizzR")
o.path <- tempdir()
OTBp <- 'C:/OTB-8.1.2-Win64'

# Do a segmentation on a raster
OTB_lsms(IMGpath = i.path, OTBpath = OTBp, savedir = o.path, maxiter = 10,
                spatrad = 20, rangerad = 30, minsize = 200, fact = 1,
                Ncore = 2, ram = 2048)

# show result
plot.rstshp(rast(system.file('extdata/rastersample.tif', package="pizzR"))[[1]],
            vect(list.files(tempdir(), pattern = '.shp', full.names = T)[1]))
}

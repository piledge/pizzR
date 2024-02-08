\name{raster.compressor}
\alias{raster.compressor}
\title{Easy compress a folder full of '.tif'- or '.tiff'-files..}
\usage{
raster.compressor(x,tmpdir=NA,recursive=T,dryrun=T)
}
\arguments{
\item{x}{character. Input-path of a Folder containing '.tif'- or '.tiff'-files.}
\item{tmpdir}{character. Optional path for temporary files. Default is a subfolder in \code{tempdir() of R}.}
\item{recursive}{logical. If files in subfolders should be considered. Default is \code{tempdir()}.}
\item{dryrun}{logical. Compress raster without changing files for testing. For safety, default is \code{TRUE}.}
}
\description{
Easy compress a folder full of '.tif'- or '.tiff'-files. Input-variable has to be a folder including the files.
}
\examples{
library(pizzR)

# specify input folder
i.path <- system.file('extdata', package="pizzR")

# compress a folders '.tif'- or '.tiff'-files
pizzR::raster.compressor(x=i.path, dryrun = T)

# write information to object
output <- pizzR::raster.compressor(x=i.path, dryrun = T)
}

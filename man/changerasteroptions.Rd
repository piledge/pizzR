\name{change.rasterOptions}
\alias{change.rasterOptions}
\title{Simple modification of \code{raster}-package parameters.}
\usage{
change.rasterOptions()
}
\arguments{
\item{changetmpdir}{logical. If \code{TRUE}, path of \code{tmpdir} is required.}
\item{tmpdir}{character. Path to write temporary files if RAM is exeeded.}
\item{OSRAM.remaining}{integer. Memory remaining for operating system. To small values may lead to system instability.}
\item{progress}{character. Valid values are "text", "window" and "" (the default in most functions, no progress bar).}
\item{verbose}{logical. Console output.}
\item{...}{additional arguments for writing files. See rasterOptions() of raster-package.}
}
\description{
Simple modification of 'raster'-package parameters.
}
\examples{
library(pizzR)
library(raster)

# default options of the raster-package
rasterOptions()


# default options by using change.rasterOptions() for maximum RAM usage
change.rasterOptions(verbose=TRUE)

# further options passed to change.rasterOptions()
change.rasterOptions(changetmpdir=TRUE,tmpdir=tempdir(),OSRAM.remaining=3,verbose=TRUE)
}

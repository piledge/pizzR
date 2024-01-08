\name{OTB_lsms}
\alias{OTB_lsms}
\title{Wrapper function to execute OTB's LSMS conveniently from R}
\usage{
OTB_lsms(IMGpath,OTBpath,savedir, ...)
}
\arguments{
\item{x}{character. Input-path of a Folder containing LAS-files}
\item{y}{character. Optional Output-path for LAS-Files. Will be created if not specified}
\item{verbose}{logical. Console output}
}
\description{
Wrapper function to execute OTB's LSMS conveniently from R. If more than one
level is supplied for the parameters 'spatrad', 'rangerad', 'minsize' or 
'fact' segmentation will be run for each unique combination.
Make sure OTB is properly installed! (visit https://www.orfeo-toolbox.org
for download and instructions on installation)
}
\examples{
library(pizzR)
library(terra)

i.path <- system.file('extdata/rastersample.tif', package="pizzR")
o.path <- tempdir()
OTBp <- 'C:/OTB-8.1.2-Win64'

# Just specify a Folder full of .LAS-files. You can also specify a output-folder of your choice
OTB_lsms(IMGpath = i.path, OTBpath = OTBp, savedir = o.path, maxiter = 10,
                spatrad = 20, rangerad = 30, minsize = 200,
                fact = 1, vectorize = TRUE,
                Ncore = 2, ram = 2048)

# show result
plot.rstshp(rast(system.file('extdata/rastersample.tif', package="pizzR"))[[1]],
            vect(list.fFiles(tempdir(), pattern = '.shp', full.names = T)))
}
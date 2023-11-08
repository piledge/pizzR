\name{LAStoLAZ}
\alias{LAStoLAZ}
\title{Easy compress a folder full of .LAS-Files}
\usage{
LAStoLAZ(x,y,verbose=T)
}
\arguments{
\item{x}{character. Input-path of a Folder containing LAS-files}
\item{y}{character. Output-path for LAS-Files. Will be created if not specified}
\item{verbose}{logical. Console output}
}
\description{
Easy compress a folder full of .LAS-Files to .LAZ-Files. Input-Files have to be in a folder and this path has to be the input-variable.
}
\examples{
library(lidR)

i.path <- dirname (system.file("extdata", "Megaplot.laz", package="lidR"))
o.path <- file.path(tempdir(), "example.tif")

# just specify a Folder full of .LAS-files
pizzR::LAStoLAZ(i.path)

# You can also specify a output-folder of your choice
pizzR::LAStoLAZ(i.path, o.path)
}

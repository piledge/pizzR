\name{LAStoLAZ}
\alias{LAStoLAZ}
\title{Easy compress a folder full of .LAS-Files}
\usage{
LAStoLAZ(x,y,verbose=F)
}
\arguments{
\item{x}{character. Input-path of a Folder containing LAS-files}
\item{y}{character. Optional Output-path for LAS-Files. Will be created if not specified}
\item{verbose}{logical. Console output}
}
\description{
Easy compress a folder full of .LAS-Files to .LAZ-Files. Input-variable has to be a folder including the files.
}
\examples{
library(lidR)
library(pizzR)

i.path <- system.file('extdata', package="pizzR")
o.path <- tempdir()

# Just specify a Folder full of .LAS-files. You can also specify a output-folder of your choice
LAStoLAZ(i.path, o.path)
}

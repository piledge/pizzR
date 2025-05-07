\name{LAStoLAZ}
\alias{LAStoLAZ}
\title{Easy compress a folder full of .LAS-Files.}
\usage{
LAStoLAZ(in_path,out_path,verbose=F)
}
\arguments{
\item{in_path}{character. Input-path of a Folder containing LAS-files.}
\item{out_path}{character. Optional Output-path for LAS-Files. Will be created if not specified.}
\item{verbose}{logical. Console output.}
}
\description{
Easy compress a folder full of .LAS-Files to .LAZ-Files. Input-variable has to be a folder including the files.
}
\examples{
library(pizzR)

i.path <- system.file('extdata', package="pizzR")
o.path <- tempdir()

# Just specify a Folder full of .LAS-files. You can also specify a output-folder of your choice
LAStoLAZ(i.path, o.path, verbose=T)
}

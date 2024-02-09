\name{setcreate.wd}
\alias{setcreate.wd}
\title{A useful combination of setwd() and dir.create().}
\usage{
setcreate.wd(path)
}
\arguments{
\item{path}{Path.}
}
\description{
Can be used instead of setwd(). Creates recursive directories prior to change of working directory.
}
\examples{
library(pizzR)

current.wd <- tempdir()

# create new folder if not exist and change working directory
setcreate.wd(paste0(current.wd,"/samplefolder"))

# change working directory if exist
setcreate.wd(current.wd)
}

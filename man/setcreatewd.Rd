\name{setcreate.wd}
\alias{setcreate.wd}
\title{a useful combination of setwd() and dir.create()}
\usage{
setcreate.wd(x)
Arguments
x
Path
}
\description{
Can be used instead of setwd(). Creates recursive path prior to change of working directory.
}
\examples{
library(pizzR)

# create new folder if not exist and change working directory
setcreate.wd(paste0(getwd(),"/samplefolder"))

# change working directory if exist
setcreate.wd(paste0(getwd(),"/samplefolder"))
}

\name{setcreate.wd}
\alias{setcreate.wd}
\title{a useful combination of setwd() and dir.create()}
\usage{
setcreate.wd(x)
}
\arguments{
  \item{x}{Path}
}
\description{
Can be used instead of setwd(). Creates recursive path prior to change of working directory.
}
\examples{
library(pizzR)

current.wd <- tempdir()

# create new folder if not exist and change working directory
setcreate.wd(paste0(current.wd,"/samplefolder"))

# change working directory if exist
setcreate.wd(current.wd)
}

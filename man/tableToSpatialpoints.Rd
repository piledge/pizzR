\name{tableToSpatialpoints}
\alias{tableToSpatialpoints}
\title{To create a vector file from a table}
\usage{
tableToSpatialpoints(northing, easting, crs.origin, ...)
}
\arguments{
\item{northing}{numeric. Northing of the points}
\item{easting}{numeric. Easting of the points}
\item{crs.origin}{character. EPSG-Code for point-coordinates}
\item{crs.project}{character. Optional EPSG-Code to project points}
\item{attributes}{data.frame. Optional data to be added to the vector file}
\item{filename}{character. Filename for export}
\item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
\item{plot}{logical. Plot the output}
\item{options}{additional GDAL-Options}
\item{...}{additional arguments for writing files. See writeVector() of terra-package}
}
\description{
Writes a vector file of a given table.
}
\examples{
library(pizzR)

# create some data for the example
data <- data.frame(northing=c(673333, 673383, 673363),
                   easting=c(5534512, 5534532, 5534513),
                   height=c(721, 722, 723),
                   attr1=c(1, 2, 3),
                   attr2=c(3, 2, 1))

setwd(tempdir())

# create a shapefile from a data.frame
tableToSpatialpoints(northing=data$northing, easting = data$easting, filename = 'points.shp',
                     crs.origin = '32633', plot = T,)

# create a shapefile from a data.frame and project it to another crs
tableToSpatialpoints(northing=data$northing, easting = data$easting, filename = 'projected.shp',
                     crs.origin = '32633', crs.project = '32632', plot = T)

# add some attributes to the table
tableToSpatialpoints(northing=data$northing, easting = data$easting, filename = 'attribues.shp',
                     crs.origin = '32633', attributes = data[,3:5], plot = T)
}

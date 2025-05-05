\name{split_poly_with_line}
\alias{split_poly_with_line}
\title{Can serve as an alternative to the QGIS 'Split with Lines' function, which may occasionally produce unexpected results.}
\usage{
  split_poly_with_line(poly_path, roads_path)
}
\arguments{
  \item{poly_path}{Path to polygon-shapefile.}
  \item{roads_path}{Path to line-shapefile.}
  \item{out_name}{Path to export-file.}
}
\description{
  Calculate principle components of a Rasterfile. This function is able to deal with rasterfiles containing NA-values.
  Currently only the raster is returned. Future updates may contain also further metrics.
}
\examples{
  library(pizzR)

  ### load sample-data
  poly_path  <- system.file('extdata/aoi_32633/aoi_32633.shp', package="pizzR")
  roads_path <- system.file('extdata/streets_32633/streets_32633.shp', package="pizzR")
  out_name   <- file.path(tempdir(), 'split.shp')

  ### Split polygon with lines
  split_poly_with_line(poly_path, roads_path, out_name)
}

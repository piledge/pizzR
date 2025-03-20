\name{aws_S2_process_tile_full}
\alias{aws_S2_process_tile_full}
\title{Funcion to process full S2-tiles from AWS.}
\usage{
aws_S2_process_tile_full(path, prefix = 's2', allbands=FALSE, scl=TRUE, export_path=NULL, n_cores=2)
}
\arguments{
\item{path}{character. Path to find raw files.}
\item{prefix}{character. To customize export filenames.}
\item{allbands}{logical. Should all S2-Bands be included, elswhere only RGBIR.}
\item{scl}{logical. Should SCL be included.}
\item{export_path}{character. Path to write processed files.}
\item{n_cores}{numeric. Number of workers for parallel processing. Ensure to have enough system memory if you increase \code{n_cores}.}
}
\description{
Simple function to process Sentinel-2 L2A-products downloaded from AWS. Best to use in combination with \code{aws_S2_lookup()} and \code{aws_S2_lookup()}.
.
}
\examples{
library(pizzR)

# download a single scene
aws_S2_download_tile('33UWP', 2022, 6, 1)

# process full tile
aws_S2_process_tile_full(path = tempdir(), allbands = F, scl = T)
}


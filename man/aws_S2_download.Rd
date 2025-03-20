\name{aws_S2_download}
\alias{aws_S2_download}
\title{Funcion to download S2-Scenes from AWS.}
\usage{
aws_S2_download(tile, year, month, day, cloud_cover=100, d_path='C:/temp/')
}
\arguments{
\item{tile}{character. Tile name of requested area.}
\item{year}{numeric. Year of scene to download.}
\item{month}{numeric. Month of scene to download.}
\item{day}{numeric. Day of scene to download.}
\item{cloud_cover}{numeric. Cloud-Cover-threshold to filter downloads. For Scenes with Cloud-Cover higher than specified only metadata will be downloaded.}
\item{d_path}{character. Path to download files.}
}
\description{
Simple function to download Sentinel-2 L2A-products in AWS. Best to use in combination with \code{aws_S2_lookup()}.
}
\examples{
library(pizzR)

# specify tile and time range
tile <- '33UWP'
years <- c(2022, 2024)
months <- 6:8
days <- 1:31
cloud_cover <- 50

# look for available scenes in specified timerange
available_scenes <- aws_S2_lookup(tile=tile, years=years, months=months, days=days)

# download a single scene
aws_S2_download_tile(tile, available_scenes$year[1], available_scenes$month[1], available_scenes$day[1], cloud_cover = cloud_cover)

# or simply loop over if you want to download multiple or all
for (i in 1:5 aws_S2_download_tile(tile, available_scenes$year[i], available_scenes$month[i], available_scenes$day[i], cloud_cover = cloud_cover)
}

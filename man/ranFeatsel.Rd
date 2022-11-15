\name{ranFeatsel}
\alias{ranFeatsel}
\title{Random-Forest Models with Feature Selection based on the Ranger-package}
\usage{
ranFeatsel(x, y, ...)
}
\arguments{
\item{x}{dataframe including features and classes}
\item{y}{character. Colname of the classes-column in x}
\item{ntree}{integer. Number of trees for each random forest. Default is 1000}
\item{best_thr}{numeric. Quantile-Treshold to select the fittest model of the feature-selection}
\item{best_thr}{numeric. Quantile-Treshold to select the fittest model of the feature-selection}
\item{nimpplot}{integer. Number variables in importance-plot}
\item{savename}{character. Basename for output-files}
\item{savendir}{character. Path for output-files}
\item{keep.files}{logical. If \code{TRUE}, all models created during feature selection will be kept. Default is \code{TRUE}}
\item{...}{additional arguments for randomForest/ranger. See specific packages for help}
}
\description{
...
}
\examples{
library(pizzR)

...
}

\name{calc_metrics}
\alias{calc_metrics}
\title{Fast and easy metric-calculation of sample plots.}
\usage{
function (data, id = "ID", verbose = FALSE)
}
\arguments{
\item{data}{data.frame. Tidy data.frame() including the data.}
\item{id}{character. Plot-ID of the data.}
\item{verbose}{logical. Console output.}
}
\description{
Fast and easy metric-calculation of sample plots. Can be used to process metrics of raster-extracts done with the terra-Package.
Calculates mean, sd and quantiles of a data.frame().
}
\examples{
library(pizzR)

n_samples <- 1000
n_plots <- 20

# Create a sample dataset
data <- data.frame(ID = pizzR::split.vector(seq(n_samples), n_plots),
                   feat_1 = sample(n_samples, replace = T), feat_2 = sample(n_samples, replace = T),
                   feat_3 = sample(n_samples, replace = T),feat_4 = sample(n_samples, replace = T))
head(data)


# Calculate Feature metrics
metrics <- calc_metrics(data, id = 'ID')


# Show results
head(metrics)
colnames(metrics)
}

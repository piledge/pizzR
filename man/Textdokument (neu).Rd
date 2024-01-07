\name{stress_cpu}
\alias{stress_cpu}
\title{simple stresstest for cpu}
\usage{
stress_cpu()
}
\arguments{
\item{n_core}{Number of cores to be used. Default is n_threads - 1}
}
\description{
Simple stresstest for CPU.
}
\examples{
library(pizzR)

# Stresstest with n_threads - 1
stress_cpu()
}

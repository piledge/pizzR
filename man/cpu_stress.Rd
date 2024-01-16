\name{cpu_stress}
\alias{cpu_stress}
\title{Simple stresstest for cpu.}
\usage{
cpu_stress()
}
\arguments{
\item{n_core}{Number of cores to be used. Default is n_threads - 1.}
\item{dur_s}{Duration in seconds. Default is Infinity.}
}
\description{
Simple stresstest for CPU.
}
\examples{
library(pizzR)

# Stresstest with a duration of 10 Seconds n_threads - 1
stress_cpu(dur_s = 10)
}

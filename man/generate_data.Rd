% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{generate_data}
\alias{generate_data}
\title{Function to generate chaotic time series
Based on the Hastings and Powell (1991) equations as used in 
Perretti et al. (2013) in Ecology}
\usage{
generate_data(nsamples, seed = 301, ncores = 6, state = c(X = 1, Y = 1, Z
  = 1), times = seq(1, 1000, by = 1))
}
\arguments{
\item{nsamples}{Number of samples}

\item{seed}{Random number seed used for parameter sampling}

\item{ncores}{Number of cores, currently uses mclapply}

\item{state}{Starting values, defaults to X = 1, Y = 1, Z = 1}
}
\description{
Function to generate chaotic time series
Based on the Hastings and Powell (1991) equations as used in 
Perretti et al. (2013) in Ecology
}

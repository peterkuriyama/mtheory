% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mtheory_ctl.R
\name{mtheory_ctl}
\alias{mtheory_ctl}
\title{Create control list for mtheory
Function to specify the parameters for run simplex}
\usage{
mtheory_ctl(seed, nsamples, state, times, samp_freq, E, lib, pred)
}
\arguments{
\item{seed}{Seed for random number generator}

\item{nsamples}{Number of iterations to run}

\item{state}{Starting state three species}

\item{times}{Length of time series}

\item{samp_freq}{Sampling frequency for generated data}

\item{E}{Dimensionality of simplex}

\item{lib}{Index of library values for simplex}

\item{pred}{Index of prediction values for simplex}
}
\description{
Create control list for mtheory
Function to specify the parameters for run simplex
}

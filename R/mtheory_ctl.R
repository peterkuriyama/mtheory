#' Create control list for mtheory

#' Function to specify the parameters for run simplex
#' @param seed Seed for random number generator
#' @param nsamples Number of iterations to run
#' @param state Starting state three species
#' @param times Length of time series
#' @param samp_freq Sampling frequency for generated data
#' @param E Dimensionality of simplex
#' @param lib Index of library values for simplex
#' @param pred Index of prediction values for simplex
#' @param error_sd Standard deviation of added error; default is 0

#' @export

mtheory_ctl <- function(seed, nsamples, state, times, samp_freq, E, lib, pred,
  error_sd = 0){
  out <- list(seed = seed, nsamples = nsamples, state = state, times = times, 
    samp_freq = samp_freq, E = E, lib = lib, pred = pred, error_sd = error_sd)
  return(out)
}
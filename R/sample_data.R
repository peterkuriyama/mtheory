#' Sample Data

#' Function to sample data

#Sample values with some frequency, samp_freq
#' @param samp_freq Frequency to sample data
#' @param data_in Data input to the function
#' @export

sample_data <- function(samp_freq = 50, data_in = samps){
  #Convert the samples to a list and sample 100 values
  sample_ts <- split(samps$samps, f = samps$samps$par)
  
  sample_ts <- lapply(sample_ts, FUN = function(xx){
    tt <- dcast(xx, pars + time ~ variable, value.var = 'value')
    tt <- tt[order(tt$time), ]
    tt <- tt[seq(1, length(tt$time), by = samp_freq), ]
    return(tt)
  })
  return(sample_ts)
}

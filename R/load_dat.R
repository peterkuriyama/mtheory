#' Load dat

#' Function to either load or simulate data

#' @param ctl_in Ctl list in; output from mtheory_ctl function
#' @param ncores Number of cores
#' @export

load_dat <- function(ctl_in, ncores){
# browser()
  #Create filename for looking up
  timez <- paste0("times_", paste(range(ctl_in$times), collapse = "_"))
  seedz <- paste0("seed", ctl_in$seed)
  statez <- paste0("state", paste(ctl_in$state, collapse = "_"))
  sampz <- paste0('nsamples', ctl_in$nsamples)
  filename <- paste0("dat_", timez, "_", seedz, "_", statez, "_",
    sampz, ".Rdata")
  
  set.seed(ctl_in$seed) #Set seed above the data generation
  #----------------------------------------------
  #Generate data if not already present
  if(length(grep(filename, list.files("output"))) == 0){
    samps <- generate_data(seed = ctl_in$seed, nsamples = ctl_in$nsamples,
      state = ctl_in$state, times = ctl_in$times, ncores = ncores)
  }

  #If present just load the data
  if(length(grep(filename, list.files("output"))) != 0){
    load(paste0('output//', filename))
    samps <- outs
    rm(outs)  
  }
  return(samps)
}

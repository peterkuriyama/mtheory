#' Make filename ctl
#' Function to generate filenames from ctl 

#' @param ctl_in Ctl from mtheory_ctl function
#' @export

make_filename <- function(ctl_in){

  timez <- paste0("times_", paste(range(ctl_in$times), collapse = "_"))
  seedz <- paste0("seed", ctl_in$seed)
  statez <- paste0("state", paste(ctl_in$state, collapse = "_"))
  sampz <- paste0('nsamples', ctl_in$nsamples)
  filename <- paste0(timez, "_", seedz, "_", statez, "_",
    sampz)
  return(filename)
}

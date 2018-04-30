#' Apply simplex list function
#' Function to apply simplex to list of sampled data for values of X, Y, and Z

#' @param E Embedding dimensions
#' @param lib Index of library values
#' @param pred Index of prediction values
#' @param samp_ts Output from sample_data function

#' @export

apply_simplex_list <- function(E, lib, pred, samp_ts){
  ss <- lapply(samp_ts, FUN = function(yy){
    #Apply simplex to elements of the list
    temp <- apply_simplex(E = E, lib = lib, pred = pred,
      dat_in = yy)
    return(temp)
  })
  return(ss)
}

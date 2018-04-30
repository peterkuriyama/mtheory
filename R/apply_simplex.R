#' Apply Simplex

#' Function to apply simplex to a data frame
#' @param E Embedding dimensions; defaults to 2:5
#' @param lib index of library values
#' @param pred index of predicted values
#' @param dat_in; data used in the function

#' @export

apply_simplex <- function(E = 2:5, lib, pred, dat_in){
  simps <- lapply(c("X", "Y", "Z"), FUN = function(var){
  out <- simplex(dat_in[, c('time', var)], E = E, 
    lib = lib, pred = pred)
  })

  names(simps) <- c("X", "Y", "Z")
  simps <- ldply(simps)
  simps <- plyr::rename(simps, c('.id' = 'variable'))

  return(simps)
}
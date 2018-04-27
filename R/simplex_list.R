#'Apply simplex to a list
#'Function to apply simplex to 

#'@param input_df Input data frame
#'@param E Dimensions
#'@param lib Library indices, no default
#'@param pred Prediction indices, no default

#'@export

simplex_list <- function(input_df, E = 2:10, lib, pred){
  #Apply simplex with a list
  E_vals <- lapply(c("X", "Y", "Z"), function(var){
    simplex(input_df[, c('time', var)], E = E, lib = lib, pred = pred)
  })
  
  #Format data frame
  names(E_vals) <- c("X", "Y", "Z")
  E_vals <- ldply(E_vals)
  E_vals <- plyr::rename(E_vals, c(".id" = "Var"))
}





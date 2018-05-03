#' Multivariate function

#' Function to evaluate multivariate rEDM models

#' @param ctl_in Ctl list; used to define lib and pred in block_lnlp
#' @param dat_in Data frame of sampled values; from run_simplex[[1]]
#' @param max_lag Maximum lag value for make_block
#' @param base_column Base columns to include in multivariable prediction
#' @param add_column Additional columns to evaluate
#' @param target_column Target column to predict

#' @export

multivariate <- function(ctl_in, dat_in, max_lag, base_column, 
  add_column, target_column){

  #----------------------------------------
  #make block data
  tt <- split(dat_in, f = dat_in$iter)
  tt1 <- lapply(tt, function(xx){
    make_block(xx[, c("X", "Y", "Z")], t = xx$time,
      max_lag = max_lag, lib = c(1, nrow(xx)))
  })

  #----------------------------------------
  #Generate the block predictions
  #With no additional 
  blocks <- lapply(tt1, FUN = function(xx){
    block_lnlp(xx, lib = ctl_in$lib, pred = ctl_in$pred,
      columns = base_column, 
      target_column = target_column, stats_only = TRUE)
  })
  blocks <- ldply(blocks)
  names(blocks)[1] <- "iter"
  blocks$iter <- as.numeric(blocks$iter)
  blocks <- blocks %>% arrange(iter)

  #Run with additional columns
  add_block <- lapply(tt1, FUN = function(xx){
    block_lnlp(xx, lib = ctl_in$lib, pred = ctl_in$pred,
      columns = c(base_column, add_column), 
      target_column = target_column, stats_only = TRUE)
  })
  add_block <- ldply(add_block)
  names(add_block)[1] <- 'iter'
  add_block$iter <- as.numeric(add_block$iter)
  add_block <- add_block %>% arrange(iter)
  
  return(list(base_block = blocks, add_block = add_block))  
}

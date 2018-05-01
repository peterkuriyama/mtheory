#' Function to generate chaotic time series
#' Based on the Hastings and Powell (1991) equations as used in 
#' Perretti et al. (2013) in Ecology

#' @param nsamples Number of samples
#' @param seed Random number seed used for parameter sampling
#' @param ncores Number of cores, currently uses mclapply
#' @param state Starting values, defaults to X = 1, Y = 1, Z = 1

#' @export
#Parameter ranges
#Table from Perretti
#a1: 3.5-6.5
#a2: 0.07-0.13
#b1: 2.1-3.9
#b2: 1.4-2.6
#d1: .28-.52
#d2: 0.007-0.013
#F: 0.025 to 0.075
generate_data <- function(nsamples, seed = 301, ncores = 6,
  state = c(X = 1, Y = 1, Z = 1), times = seq(1, 1000, by = 1)){
  #------------------------------------------------------
  #Source the hastings and powell function used in 
  #Hastings and Powell 1991
  hastings <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      
      #Define the fishing
      f1 <- (a1 * X) / (1 + b1 * X)
      f2 <- (a2 * Y) / (1 + b2 * Y)
  
      dX <- X * (1 - X) - f1 * Y - F * X
      dY <- f1 * Y - f2 * Z - d1 * Y
      dZ <- f2 * Z - d2 * Z
      
      #Return rate of change
      list(c(dX, dY, dZ))
    })
  }

  #------------------------------------------------------
  #Range of parameters
  set.seed(seed)

  a1s <- runif(nsamples, min = 3.5, max = 6.5)
  a2s <- runif(nsamples, min = .07, max = .13)
  b1s <- runif(nsamples, min = 2.1, max = 3.9)
  b2s <- runif(nsamples, min = 1.4, max = 2.6)
  d1s <- runif(nsamples, min = .28, max = .52)
  d2s <- runif(nsamples, min = .007, max = .013)
  F <- runif(nsamples, min = .025, max = .075)

  #Create a data frame of parameters
  pars <- data.frame(a1s, a2s, b1s, b2s, d1s, d2s, F)

  #------------------------------------------------------
  #Simulate the data
  #Specify as a list
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  samps <- foreach::foreach(xx = 1:nsamples, 
    .packages = c("deSolve", "dplyr", "reshape2")) %dopar% {
    temp <- pars[xx, ]
    parameters <- c(a1 = temp$a1s, a2 = temp$a2s, b1 = temp$b1s,
      b2 = temp$b2s, d1 = temp$d1s, d2 = temp$d2s, F = temp$F)
    out <- ode(y = state, times = times, func = hastings, parms = parameters)
  }
    
  stopCluster(cl)
  
  # samps <- mclapply(1:nsamples, FUN = function(xx){
  #   temp <- pars[xx, ]
  #   parameters <- c(a1 = temp$a1s, a2 = temp$a2s, b1 = temp$b1s,
  #     b2 = temp$b2s, d1 = temp$d1s, d2 = temp$d2s, F = temp$F)
    
  #   # times <- seq(.01, 1000, by = .01)
  #   out <- ode(y = state, times = times, func = hastings, parms = parameters)
  # }, mc.cores = ncores)

  #------------------------------------------------------
  #Format output

  #Combine the samples from list
  names(samps) <- 1:length(samps)
  samps1 <- ldply(samps)
  samps1 <- plyr::rename(samps1, c(".id" = 'pars'))

  samps1 <- melt(samps1, id.vars = c('pars', 'time'))

  outs <- list(pars = pars, samps = samps1)  

  #Create filename to save output
  timez <- paste0("times_", paste(range(times), collapse = "_"))
  seedz <- paste0("seed", seed)
  statez <- paste0("state", paste(state, collapse = "_"))
  sampz <- paste0('nsamples', nsamples)
  # filename <- paste0("dat_", timez, "_", seedz, "_", statez, ".Rdata")
  filename <- paste0("dat_", timez, "_", seedz, "_", statez, "_",
    sampz, ".Rdata")
  filename <- paste0("output//", filename)
  
  save(outs, file = filename)

  return(outs)
}
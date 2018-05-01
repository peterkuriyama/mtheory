#----------------------------------------------------------------------------------------
#Set working directory and load packages
setwd("C://Users//Peter//Dropbox//postdoc//calcofi")
setwd("/Users/peterkuriyama/Dropbox/postdoc/calcofi/mtheory")

library(devtools)
library(deSolve)
library(parallel)
library(plyr)
library(tidyverse)
library(rEDM)
library(reshape2)
library(doParallel)

devtools::install_github('peterkuriyama/mtheory')
library(mtheory)
# load_all()
#----------------------------------------------------------------------------------------
#Simulate three species time series

##Add error to the time series
# ts_err <- ts + rnorm(length(ts), sd = sd(ts) * 0.2)
# smap_output_err <- s_map(ts_err, lib, pred, E = 2)
# plot(smap_output_err$theta, smap_output_err$rho, type = "l", xlab = "Nonlinearity (theta)", 
#     ylab = "Forecast Skill (rho)")
##

#-----------------------
#Generate data for three species
samps <- generate_data(seed = 500, nsamples = 12, 
  state = c(X = 1, Y = 1, Z = 1), times = seq(1, 5000, by = 1))
#samps are formatted this way to facilitate plotting in ggplot

#-----------------------
#Sample data at some frequency
sample_ts <- sample_data(data_in = samps, samp_freq = 10)

#-----------------------
#Use simplex to find best embedding dimension
simplex_list <- apply_simplex_list(E = 2:8, lib = c(1, 100), pred = c(1, 100), 
  samp_ts = sample_ts)


#-----------------------
#Function to 


ctl <- mtheory_ctl(seed = 500, nsamples = 12, state = c(X = 1, Y = 1, Z = 1), 
  times = seq(1, 5000, by = 1), samp_freq = 10, E = 2:8, lib = c(1, 100),
  pred = c(1, 100))



run_simplex(ctl_in = ctl, ncores = 6)

#-----------------------
#Identify nonlinearity



#-------------------------------------------------------------------
#Plot simplex_list values
sl1 <- ldply(simplex_list)
names(sl1)[1] <- 'iter'

sl1 %>% ggplot() + geom_line(aes(colour = variable, x = E, y = rho)) +
  facet_wrap(~ iter) + ylim(c(0, 1))

ldply

samp_freq <- 50 #sample every 50 values to start


#To do
####Detect chaos in time series


#----------------------------------------------------------------------------------------
#Plot the time series in ggplot
samps$samps  %>% ggplot(aes(x = time, y = value)) +
    geom_line(aes(colour = variable)) + facet_wrap(~ pars, scales = 'free') 

#----------------------------------------------------------------------------------------
generate_data(nsamples = 6, seed = 301, ncores = 6)

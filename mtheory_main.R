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
#----------------------------------------------------------------------------------------
#Simulate three species time series

samps <- generate_data(seed = 300, nsamples = 6, 
  state = c(X = 1, Y = 1, Z = .5))

#Plot the time series in ggplot
samps$samps  %>% ggplot(aes(x = time, y = value)) +
    geom_line(aes(colour = variable)) + facet_wrap(~ pars, scales = 'free') 

#----------------------------------------------------------------------------------------
#Write the parallelization with dopar
generate_data(nsamples = 6, seed = 301, ncores = 6)


ind <- 2
out <- samps[[ind]]
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")


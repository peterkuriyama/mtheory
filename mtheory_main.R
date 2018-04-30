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

load_all()
#----------------------------------------------------------------------------------------
#Simulate three species time series

##Add error to the time series
# ts_err <- ts + rnorm(length(ts), sd = sd(ts) * 0.2)
# smap_output_err <- s_map(ts_err, lib, pred, E = 2)
# plot(smap_output_err$theta, smap_output_err$rho, type = "l", xlab = "Nonlinearity (theta)", 
#     ylab = "Forecast Skill (rho)")
##

#Generate data for three species
samps <- generate_data(seed = 500, nsamples = 12, 
  state = c(X = 1, Y = 1, Z = 1), times = seq(1, 1000, by = 1))
#samps are formatted this way to facilitate plotting in ggplot

#Convert the samples to a list and sample 100 values
sample_ts <- split(samps$samps, f = samps$samps$par)

sample_ts <- lapply(sample_ts, FUN = function(xx){
  tt <- dcast(xx, pars + time ~ variable, value.var = 'value')
  tt <- tt[order(tt$time), ]
  # tt$time <- tt$time * 100
  #Multiply time by 100 and sample every 100 values
  # tt <- tt[seq(1, 100000, by = 100), ]
})


#Conduct simplex on each list


simplex_out <- lapply(c("AbvBioAnnProd", "noh020tot", "invrichness"), function(var) {
    simplex(composite_ts[, c("Year", var)], E = 2:4, lib = composite_lib, pred = composite_pred)
})
#Sample every 100 values because the time steps are currently too fine



#Apply simplex to sampled data
temp <- samps$samps %>% filter(pars == 1)
temp <- dcast(temp, pars + time ~ variable, value.var = 'value')




#sample every 1000 values
temp <- temp[seq(1, 100000, by = 100), ]
lib <- c(1, 100)
pred <- c(801, 900)

tt <- simplex(temp$Y, lib, pred, stats_only = FALSE)
tt

#----------------------------------------------------------------------------------------
#Plot the time series in ggplot
samps$samps  %>% ggplot(aes(x = time, y = value)) +
    geom_line(aes(colour = variable)) + facet_wrap(~ pars, scales = 'free') 
samps$samps$time <- samps$samps$time * 100


samps$samps 
samps


lib <- c(1, 100)
pred <- c(901, 1000)

simplex_output <- simplex(samps$samps %>% filter(pars == 1), lib, pred, stats_only = FALSE)
simplex_output <- simplex(ts, lib, pred)



#----------------------------------------------------------------------------------------
#Write the parallelization with dopar
generate_data(nsamples = 6, seed = 301, ncores = 6)


ind <- 2
out <- samps[[ind]]
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")


#----------------------------------------------------------------------------------------
#Set working directory and load packages
setwd("C://Users//Peter//Dropbox//postdoc//calcofi")
setwd("C://Users//peter.kuriyama//Desktop//mtheory//")
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
#One simulation, sampling every 10 values

#Add error to the time series

ctl <- mtheory_ctl(seed = 500, nsamples = 18, state = c(X = 1, Y = 1, Z = 1), 
  times = seq(1, 5000, by = 1), samp_freq = 10, E = 2:8, lib = c(301, 375),
  pred = c(476, 500), error_sd = .2)

samps <- load_dat(ctl_in = ctl, ncores = 6)

# ctl$pred <- c(301, 400)
samp10 <- run_simplex(ctl_in = ctl, ncores = 6)

#Visualize the time series and the single variable
pdf(width = 7, height = 7, file = 'figs/samp_plots_every1_error.pdf')
for(ii in 1:length(unique(samp10[[1]]$iter))){
  show_results(samp_input = samp10, nrun = ii)  
}
dev.off()


#-----------------------
#Add in covariates to reconstruct the simplex
yy <- multivariate(ctl_in = ctl, dat_in = samp10[[1]], max_lag = 3, 
  base_column = c("X", "X_1", "X_2"),
  add_column = c("Y_1"), target_column = "Y")

#Calc
round(yy[[2]]$rho - yy[[1]]$rho, digits = 2)

#----------------------------------------------------------------------------------------
#Visualize the time series and the single variable
show_results(samp_input = samp10, nrun = 1)


#----------------------------------------------------------------------------------------
#Example to look at what happens when sampling data less frequently

#Sample every 10 values
ctl <- mtheory_ctl(seed = 500, nsamples = 12, state = c(X = 1, Y = 1, Z = 1), 
  times = seq(1, 5000, by = 1), samp_freq = 10, E = 2:8, lib = c(1, 100),
  pred = c(1, 100))

ctl$pred <- c(401, 500)
samp10 <- run_simplex(ctl_in = ctl, ncores = 6)







samp10[[2]] %>% ggplot() + geom_line(aes(x = E, y = rho, colour = variable), size = 1.5) + 
  facet_wrap(~ iter) + ggtitle(paste0("Predictive ability; sampled every ", 
    ctl$samp_freq, ' values')) + ylim(c(-.25, 1)) + 
  geom_hline(yintercept = 0, lty = 2) +
  ggsave(filename = paste0("figs//", make_filename(ctl_in = ctl),
    '_samp_freq', ctl$samp_freq, ".png"))

#-----------------------
#Plot the actual data
ff <- make_filename(ctl_in = ctl)
load(paste0('output//dat_', ff, '.Rdata'))

outs[[2]] %>% ggplot(aes(x = time, y = value)) + geom_line(aes(colour = variable)) +
  facet_wrap(~ pars, scales = 'free') + 
  ggsave(filename = paste0("figs//", make_filename(ctl_in = ctl),
    '_simdata', ".png"))

#-----------------------
#Sample every 50 times
ctl$samp_freq <- 50
samp50 <- run_simplex(ctl_in = ctl, ncores = 6)
samp50 %>% ggplot() + geom_line(aes(x = E, y = rho, colour = variable)) + 
  facet_wrap(~ iter) + ggtitle(paste0("Predictive ability; sampled every ", 
    ctl$samp_freq, ' values')) + ylim(c(-.25, 1)) + 
  geom_hline(yintercept = 0, lty = 2)
  ggsave(filename = paste0("figs//", make_filename(ctl_in = ctl),
    '_samp_freq', ctl$samp_freq, ".png"))

#-----------------------
#Sample every 100 times
ctl$samp_freq <- 100
samp100 <- run_simplex(ctl_in = ctl, ncores = 6)
samp100 %>% ggplot() + geom_line(aes(x = E, y = rho, colour = variable)) + 
  facet_wrap(~ iter) + ggtitle(paste0("Predictive ability; sampled every ", 
    ctl$samp_freq, ' values')) + ylim(c(-.25, 1)) + 
  geom_hline(yintercept = 0, lty = 2)
  ggsave(filename = paste0("figs//", make_filename(ctl_in = ctl),
    '_samp_freq', ctl$samp_freq, ".png"))



samp50 %>% ggplot() + geom_line(aes(x = E, y = rho, colour = variable)) + 
  facet_wrap(~ iter)



#-----------------------
#Identify nonlinearity


#To do
####Detect chaos in time series


#----------------------------------------------------------------------------------------
#Plot the time series in ggplot
samps$samps  %>% ggplot(aes(x = time, y = value)) +
    geom_line(aes(colour = variable)) + facet_wrap(~ pars, scales = 'free') 

#----------------------------------------------------------------------------------------
generate_data(nsamples = 6, seed = 301, ncores = 6)

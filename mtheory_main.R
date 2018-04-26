#----------------------------------------------------------------------------------------
#Set working directory and load packages
setwd("C://Users//Peter//Dropbox//postdoc//calcofi")
setwd("/Users/peterkuriyama/Dropbox/postdoc/calcofi")

library(devtools)
library(deSolve)
library(parallel)

#----------------------------------------------------------------------------------------
#Simulate three species time series

#Hastings and Powell 1991

#Example three species time series
#From deSolve Vignette
#Use package deSolve
#Initilalize population levels
parameters <- c(a = -8/3, b = -10, c = 28)
state <- c(X = 1, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters){
	with(as.list(c(state, parameters)), {
		#rate of change
		dX <- a * X + Y * Z
		dY <- b * (Y - Z)
		dZ <- -X * Y + c * Y - Z 

		#Return rate of change
		list(c(dX, dY, dZ))
	})
}

set.seed(301)
times <- seq(0, 100, by = 0.01)
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)


#----------------------------------------------------------------------------------------
#PRedprey example from 
# https://stackoverflow.com/questions/22159830/correct-use-of-desolve-in-ecological-modelling-of-a-predator-prey-system

predprey_FuncResp  <- function(t, y, parms) {
    
    with(as.list(c(y,parms)), {
    print(p0)
         dpdt <- (a*n0*p0)/(v+n0) - c*p0
         dndt <- r*n0*(1-n0/k) - (s*n0*p0)/(v+n0)
         return(list(c(dndt, dpdt)))
    })
}

parms <- c(a=.12, c=.06 , r=.1,s=.1,k=100, v=40)
Tmax = 1200 # time horizon  
TimeStep = 1 # integration time step
Time <- seq(0, Tmax, by = TimeStep)
LV.out <- ode(c(n0 = 50, p0 = 15), Time, predprey_FuncResp,parms)
par(las=1,bty="l")
matplot(LV.out[,1],LV.out[,-1], type='l', xlab="time", ylab="density")

library(deSolve)
predprey_FuncResp  <- function(t, y, parms) {
    with(as.list(c(y,parms)), {
         dpdt <- (a*n0*p0)/(v+n0) - c*p0
         dndt <- r*n0*(1-n0/k) - (s*n0*p0)/(v+n0)
         return(list(c(dndt, dpdt)))
    })
}
parms <- c(a=.12, c=.06 , r=.1,s=.1,k=100, v=40)
Tmax = 1200 # time horizon  
TimeStep = 1 # integration time step
Time <- seq(0, Tmax, by = TimeStep)
LV.out <- ode(c(n0 = 50, p0 = 15), Time, predprey_FuncResp,parms)
par(las=1,bty="l")
matplot(LV.out[,1],LV.out[,-1], type='l', xlab="time", ylab="density")


#----------------------------------------------------------------------------------------
#Table from Perretti
#a1: 3.5-6.5
#a2: 0.07-0.13
#b1: 2.1-3.9
#b2: 1.4-2.6
#d1: .28-.52
#d2: 0.007-0.013
#F: 0.025 to 0.075

#Specify f1 and f2
a1 <- 5
a2 <- .10
b1 <- 2.8
b2 <- 2
u <- .1


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

set.seed(300)
a1s <- runif(5, min = 3.5, max = 6.5)
a2s <- runif(5, min = .07, max = .13)
b1s <- runif(5, min = 2.1, max = 3.9)
b2s <- runif(5, min = 1.4, max = 2.6)
d1s <- runif(5, min = .28, max = .52)
d2s <- runif(5, min = .007, max = .013)
F <- runif(5, min = .025, max = .075)

#Create a data frame
pars <- data.frame(a1s, a2s, b1s, b2s, d1s, d2s, F)

#Specify as a list
samps <- mclapply(1:5, FUN = function(xx){
  temp <- pars[xx, ]
  parameters <- c(a1 = temp$a1s, a2 = temp$a2s, b1 = temp$b1s,
    b2 = temp$b2s, d1 = temp$d1s, d2 = temp$d2s, F = temp$F)

  state <- c(X = 1.5, Y = 1, Z = 1)
  times <- seq(0, 100, by = .01)
  out <- ode(y = state, times = times, func = hastings, parms = parameters)
}, mc.cores = 5)

out <- samps[[5]]
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")
# mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)



#Specify parameters and starting states
parameters <- c(a1 = 5, a2 = .1, b1 = 2.8, b2 = 2, 
  d1 = .4, d2 = .01, F = .04)
state <- c(X = 1.5, Y = 1, Z = 1)

times <- seq(0, 500, by = 0.01)
out <- ode(y = state, times = times, func = hastings, parms = parameters)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)




scho
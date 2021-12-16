#' @title Run Simulations
#' 
#' @description This is a file generating Monte Carlo simulated data with gen.data() function in Simulation2.R
#'
#' @import Simulation2.R

# -------------------#
# Housekeeping
rm(list=ls())
cat("\014") 
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo

# -------------------#
#Set working directory
setwd("/Users/hoeffding/Desktop/CHS2010/")
set.seed(5)
date()

# --------------------------------------#
# Load Packages
pkg<-list("numDeriv")
lapply(pkg, require, character.only=T)
functions<-list("Simulation2.R")
lapply(functions, source)
rm(pkg,functions)
#source('D:/Onedrive - University College London/CHS2010/Simulation2.R')
# -------------------#


# Indeces
M       <- 20   # Number of measurements. In theory, M is allowed to vary by type a=1,2,3 and factor k=N,C.
N       <- 2200 # Number of individuals.
Time    <- 8    # Number of periods.
S       <- 2    # Number of stages in childhood development.
k       <- 2    # Dimension of skill vector (cognitive and non-cognitive).
N.theta <- 5    # Dimension of latent state vector: stock of skills (cognitive and non-cognitive), 
                # investment in skills (cognitive and non-cognitive, assumed equal), and 
                # parental skills (cognitive and non-cognitive).

#' Input parameters
phi        <- array(1, dim = c(k,1,S))
gamma      <- array(0.5, dim = c(k,N.theta,S))
delta.eta  <- array(2, dim = c(1,1,S))
miu        <- list(array(1, dim = c(N,k,Time,M)), array(2, dim = c(N,1,Time,M)), array(3, dim = c(N,k,M)))
alpha      <- list(array(1, dim = c(N,k,Time,M)), array(2, dim = c(N,1,Time,M)), array(3, dim = c(N,k,M)))
lambda     <- 1.5

#' Number of simulations
n.sim      <- 100

#' Simulate and compute the means
#' Each element represents means of each simulation
my.outcome <- lapply(1:n.sim, function(t) NA)

for (i in 1:n.sim){
  
 # Generate simulated data
 fake <- gen.data(phi       = phi, 
                  gamma     = gamma,
                  delta.eta = delta.eta, 
                  Time      = 8, 
                  N         = N, 
                  M         = M, 
                  miu       = miu, 
                  alpha     = alpha, 
                  lambda    = lambda, 
                  n.stage   = S, 
                  rn.seed   = i)[[1]]
 
 
 #my.outcome[[i]] <- unname(colMeans(fake))
}

#' Unlist and generate a matrix of means
#' Each column represents means of each simulation
mean.matrix <- matrix(unlist(my.outcome), ncol = n.sim)



#' 











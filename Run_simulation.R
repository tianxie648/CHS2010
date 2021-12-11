#' @title Run Simulations
#' 
#' @description This is a file generating Monte Carlo simulated data with gen.data() function in Simulation2.R
#'
#' @import Simulation2.R


source('D:/Onedrive - University College London/CHS2010/Simulation2.R')

#' Input parameters
phi        <- array(1, dim = c(2,1,2))
gamma      <- array(0.5, dim = c(2,5,2))
delta.eta  <- array(2, dim = c(1,1,2))
miu        <- list(array(1, dim = c(2200,2,8,20)), array(2, dim = c(2200,1,8,20)), array(3, dim = c(2200,2,20)))
alpha      <- list(array(1, dim = c(2200,2,8,20)), array(2, dim = c(2200,1,8,20)), array(3, dim = c(2200,2,20)))
lambda     <- 1.5

#' Number of simulations
n.sim      <- 100

#' Simulate and compute the means
#' Each element represents means of each simulation
my.outcome <- lapply(1:n.sim, function(t) NA)

for (i in 1:n.sim){
 fake <- gen.data(phi       = phi, 
                  gamma     = gamma,
                  delta.eta = delta.eta, 
                  Time      = 8, 
                  N         = 2200, 
                  M         = 20, 
                  miu       = miu, 
                  alpha     = alpha, 
                  lambda    = lambda, 
                  n.stage   = 2, 
                  rn.seed   = i)
 
 my.outcome[[i]] <- unname(colMeans(fake))
}

#' Unlist and generate a matrix of means
#' Each column represents means of each simulation
mean.matrix <- matrix(unlist(my.outcome), ncol = n.sim)




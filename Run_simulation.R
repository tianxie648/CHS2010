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
functions<-list("Simulation2.R","Reshape.R")
lapply(functions, source)
rm(pkg,functions)
#source('D:/Onedrive - University College London/CHS2010/Simulation2.R')
# -------------------#


# Indeces
M       <- matrix(c(3,2,7,6,12,
                    3,5,7,6,12,
                    2,8,6,6,12,
                    4,5,14,6,12,
                    3,5,11,6,12,
                    3,5,11,6,12,
                    3,5,10,6,12,
                    3,5,10,6,12),nrow = 5)   # Number of measurements. Each column is the number of measurements for each period t. Each row is by order: Child Cognitive, Child Noncognitive, Investment, Parental Cognitive, Parental Noncognitive
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
miu        <- 1
alpha      <- 2
lambda     <- 1.5

#' Number of simulations
n.sim      <- 100

#' Simulate and compute the means
#' Each element represents means of each simulation
my.outcome <- lapply(1:n.sim, function(t) NA)

for (i in 1:n.sim){
  
 # Generate simulated data
 my.data <- gen.data(phi       = phi, 
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
 fake <- my.data[[1]]
 fake.list <- reshape.sim(data = fake, T = 8)
 
 #my.outcome[[i]] <- unname(colMeans(fake))
}

#' Unlist and generate a matrix of means
#' Each column represents means of each simulation
mean.matrix <- matrix(unlist(my.outcome), ncol = n.sim)



#' 











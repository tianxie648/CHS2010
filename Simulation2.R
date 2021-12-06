#' @title Monte Carlo Simulation
#' 
#' @description Generate Monte Carlo simulations as in 4.1 (CHS2010).
#' 
#' @param phi         Array. 2x1xn.stage numeric. The parameter in the production function in (4,1) (CHS2010)
#' @param gamma       Array. 2x5xn.stage numeric. The coefficient in the production function in (4.1) (CHS2010)
#' @param delta.eta   Array. 1x1xn.stage numeric. The standard deviation in the production function in (4,1) (CHS2010)
#' @param T           Integer. Number of time periods.
#' @param N           Integer. Number of individuals.
#' @param M           Integer. Number of measurements. Assume the number of all measurements is the same 20.
#' @param miu         List of 3. The first element is a Nx2xTxM array, the \miu in (3.1). The second element is a Nx1xTxM array, the \miu in (3.2). The third element is a Nx2xM array, the \miu in (3.3). 
#' @param alpha       List of 3. (No normalization in simulations) The first element is a Nx2xTxM array, the \alpha in (3.1). The second element is a Nx1xTxM array, the \alpha in (3.2). The third element is a Nx2xM array, the \alpha in (3.3).
#' @param lambda      Numeric. The square root of diagonal entries of \Lambda in Page 905. Assumed to be the same across all measurements.
#' @param n.stage     Integer. Number of stages.  
#' @param rn.seed     Integer. Random seed.
#' 
#' @return Data frame. A data frame of simulated data.
#' 
#' @author
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords
#' @import 

stage.t <- function(t){
  
  # The function returns the development stage corresponding to time period t.
  
  # Set the first 4 periods as Stage 1, the last 4 periods as Stage 2
  s <- 0
  if (t<= 4){
    s <- 1
  } else {
    s <- 2
  }
  return(s)
}


gen.data <- function(phi, gamma, delta.eta, T=8, N=2200, M = 20, miu, alpha, lambda, n.stage = 2, rn.seed){
  
  # ----------------------------------------------------- #
  # Generate parental skills and investments, from arbitrary distributions
  set.seed(rn.seed)
  theta.CP <- exp(rnorm(N))
  theta.NP <- exp(rnorm(N))
  Invest.t <- exp(matrix(rnorm(N*T), nrow = N))
  # ------------------------- #
  # Generate children's skills
  # Note: the initial skills are 1
  child.skill <- lapply(1:T, function(x) matrix(1, nrow = N, ncol = 2))
  
  for (t in 1:(T-1)){
    stage       <- stage.t(t)
    phi.s       <- phi[,,stage]
    gamma.s     <- gamma[,,stage]
    delta.eta.s <- delta.eta[,,stage]
    
    eta.C              <- rnorm(N, mean = 0, sd = delta.eta.s)
    theta.C            <- ( (matrix(cbind(child.skill[[t]], Invest.t[,t], theta.CP, theta.NP),nrow = N) ^ phi.s[1]) %*% matrix(gamma.s[1,])) ^ (1/phi.s[1]) * exp(eta.C)
    eta.N              <- rnorm(N, mean = 0, sd = delta.eta.s)
    theta.N            <- ( (matrix(cbind(child.skill[[t]], Invest.t[,t], theta.CP, theta.NP),nrow = N) ^ phi.s[2]) %*% matrix(gamma.s[1,])) ^ (1/phi.s[2]) * exp(eta.N)
    child.skill[[t+1]] <- cbind(theta.C, theta.N)
  }
  
  # ------------------------------------------ #
  # Generate measurements of patental skills
  # Note: Variance of errors are sssumed to be the same across all measurements
  miu.3   <- miu[[3]]
  alpha.3 <- alpha[[3]]
  
  Z.3.C <- miu.3[,1,] + alpha.3[,1,] * log(theta.CP) + matrix(rnorm(N*M,mean = 0, sd = lambda),ncol = M)
  Z.3.N <- miu.3[,2,] + alpha.3[,2,] * log(theta.NP) + matrix(rnorm(N*M,mean = 0, sd = lambda),ncol = M)
  # ---------------------------------------- #
  # Generate measurements of investments and children's skills
  # Note: Variance of errors are sssumed to be the same across all measurements
  miu.1   <- miu[[1]]
  miu.2   <- miu[[2]]
  alpha.1 <- alpha[[1]]
  alpha.2 <- alpha[[2]]
  
  Z.1.C <- lapply(1:T, function(t) miu.3[,1,t,] + alpha.3[,1,t,] * log(child.skill[[t]][,1]) + matrix(rnorm(N*M,mean = 0, sd = lambda),ncol = M))
  Z.1.N <- lapply(1:T, function(t) miu.3[,2,t,] + alpha.3[,2,t,] * log(child.skill[[t]][,2]) + matrix(rnorm(N*M,mean = 0, sd = lambda),ncol = M))
  Z.2   <- lapply(1:T, function(t) miu.2[,1,t,] + alpha.2[,1,t,] * log(Invest.t[,t]) + matrix(rnorm(N*M,mean = 0, sd = lambda),ncol = M))

  Z.1.C <- matrix(unlist(Z.1.C), nrow = N)
  Z.1.N <- matrix(unlist(Z.1.N), nrow = N)
  Z.2   <- matrix(unlist(Z.2), nrow = N)
  
  # -------------------------------------- #
  #Generate the data frame
  result <- data.frame(Z.1.C, Z.1.N, Z.2, Z.3.C, Z.3.N)
  return(result)
}










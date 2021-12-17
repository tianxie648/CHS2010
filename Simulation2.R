#' @title Monte Carlo Simulation
#' 
#' @description Generate Monte Carlo simulations as in 4.1 (CHS2010).
#' 
#' @param phi         Numeric array of dimension 2x1xn.stages. The parameter in the production function in (4.1) (CHS2010)
#' @param gamma       Numeric array of dimension 2x5xn.stages. The coefficient in the production function in (4.1) (CHS2010)
#' @param delta.eta   Numeric array of dimension 1x1xn.stages. The standard deviation in the production function in (4,1) (CHS2010). Observe that we allow the variance to change between stages but not across skills.
#' @param Time        Integer. Number of time periods.
#' @param N           Integer. Number of individuals.
#' @param M           Numeric matrix of dimension 5xTime. The number of measurements for each factor at each period. Each column of the matrix represents each period. Each row is by order: Child Cognitive, Child Noncognitive, Investment, Parental Cognitive, Parental Noncognitive
#' @param miu         Numeric. \eqn{\miu} in (3.1)-(3.3). Assumed to be the same across all measurements 
#' @param alpha       Numeric. (No normalization in simulations) The \eqn{\alpha} in (3.1) - (3.3). Assumed to be the same across all measurements.
#' @param lambda      Numeric. The square root of diagonal entries of \eqn{\Lambda} in Page 905. Assumed to be the same across all measurements.
#' @param n.stage     Integer. Number of stages in childhood development.
#' @param rn.seed     Integer.Seed for the random number generator.
#' 
#' @return List. A list of simulated data.
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


gen.data <- function(phi, gamma, delta.eta, Time=8, N=2200, M, miu, alpha, lambda, n.stage = 2, rn.seed){
  
  # ----------------------------------------------------- #
  # Generate parental skills and investments, from arbitrary distributions
  set.seed(rn.seed)
  theta.CP <- exp(rnorm(N))
  theta.NP <- exp(rnorm(N))
  Invest.t <- exp(matrix(rnorm(N*Time), nrow = N))
  # ------------------------- #
  # Generate children's skills
  # Note: the initial skills are 1
  child.skill <- lapply(1:Time, function(x) matrix(1, nrow = N, ncol = 2))
  
  for (t in 1:(Time-1)){
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
  miu.3   <- miu
  alpha.3 <- alpha
  
  Z.3.C <- miu.3 + alpha.3 * log(theta.CP) + matrix(rnorm(N*M[4,1],mean = 0, sd = lambda),ncol = M[4,1])
  Z.3.N <- miu.3 + alpha.3 * log(theta.NP) + matrix(rnorm(N*M[5,1],mean = 0, sd = lambda),ncol = M[5,1])
  # ---------------------------------------- #
  # Generate measurements of investments and children's skills
  # Note: Variance of errors are asssumed to be the same across all measurements
  miu.1   <- miu
  miu.2   <- miu
  alpha.1 <- alpha
  alpha.2 <- alpha
  
  Z.1.C <- lapply(1:Time, function(t) miu.1 + alpha.1 * log(child.skill[[t]][,1]) + matrix(rnorm(N*M[1,t],mean = 0, sd = lambda),ncol = M[1,t]))
  Z.1.N <- lapply(1:Time, function(t) miu.1 + alpha.1 * log(child.skill[[t]][,2]) + matrix(rnorm(N*M[2,t],mean = 0, sd = lambda),ncol = M[2,t]))
  Z.2   <- lapply(1:Time, function(t) miu.2 + alpha.2 * log(Invest.t[,t]) + matrix(rnorm(N*M[3,t],mean = 0, sd = lambda),ncol = M[3,t]))

  # The following step exports the results as vectors. We note that
  # the next objects are matrices with N rows and Time*M columns.
  # However, the order matters. For example, the first two columns
  # of Z.1.C are two measurements for the stock of cognitive skill
  # in time t=1. Therefore, it's ordered in pairs: for each t, m=1,...M.
  Z.1.C.M <- matrix(unlist(Z.1.C), nrow = N)
  Z.1.N.M <- matrix(unlist(Z.1.N), nrow = N)
  Z.2.M   <- matrix(unlist(Z.2), nrow = N)
  
  # -------------------------------------- #
  #Generate the data frame
  dat <- data.frame(Z.1.C.M, Z.1.N.M, Z.2.M, Z.3.C, Z.3.N)
  result <- list(dat, Z.1.C, Z.1.N, Z.2, Z.3.C, Z.3.N)
  return(result)
}










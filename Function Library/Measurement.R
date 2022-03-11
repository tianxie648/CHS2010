#' @title Estimates of Factor Loadings
#' 
#' @description Compute the factor loadings as in 3.1 (CHS2010).
#' 
#' @param Z    List. The first object is the matrix of observed data. Each row represents an individual. The rest objects are Z.1.C.M, Z.1.N.M, Z.2.M, Z.3.C, Z.3.N.
#' @param M    Numeric. matrix of dimension 5xTime. The number of measurements for each factor at each period. Each column of the matrix represents each period. Each row is by order: Child Cognitive, Child Noncognitive, Investment, Parental Cognitive, Parental Noncognitive
#' @param ... Extra arguments.
#' 
#' @return 
#' 
#' @author 
#' @references 
#' "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords
#' @import


Measurement <- function(Z, M){
  
  Time <- dim(M)[2]
  
  # Normalization condition 1: E(\theta) = 0 
  # (Mean of abilities is zero) 
  # - Identify the constant
  
  # Estimate the constant:
  mu <- unname(colMeans(Z[[1]],dims=1))
  
  
  
  # Normalization condition 2: \alpha_{a,k,t,1} = 0 
  # (The first measurements have unit coefficient) 
  #- Identify coefficients
  
  # Generate a list to store coefficients
  # e.g. A[[1]] is a list for all coefficients for cognitive
  # A[[1]][[1]] is a vector for all coefficients for cognitive in time = 1
  A <- lapply(1:5, function(x) lapply(1:Time, function(t) rep(1,M[x,t])))
  
  # Reshape the data
  Z.time <- lapply(1:5, function(x) lapply(1:Time, function(t) NA))
}


separate.data <- function(Z,M,x,t){
  
  # From data Z, pick the measurements for x in time t
  
  end <- sum(M[x,1:t])
  start <- end - M[x,t] + 1
  
  Z.x.t <- Z[[x+1]][,start:end]
  return(Z.x.t)
}



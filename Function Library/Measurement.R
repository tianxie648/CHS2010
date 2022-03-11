#' @title Estimates of Factor Loadings
#' 
#' @description Compute the factor loadings as in 3.1 (CHS2010).
#' 
#' @param Z    List. The first object is the matrix of observed data. Each row represents an individual. The rest objects are Z.1.C.M, Z.1.N.M, Z.2.M, Z.3.C, Z.3.N.
#' @param M    Numeric. matrix of dimension 5xTime. The number of measurements for each factor at each period. Each column of the matrix represents each period. Each row is by order: Child Cognitive, Child Noncognitive, Investment, Parental Cognitive, Parental Noncognitive
#' @param ... Extra arguments.
#' 
#' @return A   List. The estimated factor loadings. A[[i]][[t]][,k] is for factor i, time t, the k-th measurement, and is a column \eqn{(\mu_k,\alpha_k)^T}.
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
  
  # Normalization condition 2: \alpha_{a,k,t,1} = 0 
  # (The first measurements have unit coefficient) 
  #- Identify coefficients
  
  # Reshape the data
  # Z[[x]][[t]] is the measurements for x in time t
  Z.time <- lapply(1:5, function(x) lapply(1:Time, function(t) separate.data(Z,M,x,t)))
  
  # Generate a list of coefficients
  # e.g. A[[1]] is a list for all coefficients for cognitive
  # A[[1]][[1]] is a matrix for all coefficients for cognitive in time = 1, the k-th column (mu_k, alpha_k)
  A <- lapply(1:5, function(x) lapply(1:(Time-1), function(t) coefficient.Z(Z.time,M,x,t)))
  
  # Compute the coefficients for the last period separately
  for (i in 1:5){
    mu.Time <- unname(colMeans(Z.time[[i]][[Time]],dims=1))
    alpha.Time <- unlist(lapply(1:M(i,Time), function(k) cov(Z.time[[i]][[Time]][,k],Z.time[[i]][[Time-1]][,1])/ 
                                      cov(Z.time[[i]][[Time]][,1],Z.time[[i]][[Time-1]][,1])))
    A[[i]][[Time]] <- t(matrix(cbind(mu.Time,alpha.Time), nrow=M(i,Time)))
  }
  
  return(A)
}


separate.data <- function(Z,M,x,t){
  
  # From data Z, pick the measurements for x in time t
  
  end <- sum(M[x,1:t])
  start <- end - M[x,t] + 1
  
  Z.x.t <- Z[[x+1]][,start:end]
  return(Z.x.t)
}

coefficient.Z <- function(Z.time,M,x,t){
  
  # From reshaped data Z.time, calculate the coefficient for x in time t
  
  mu <- unname(colMeans(Z.time[[x]][[t]],dims=1))
  
  alpha <- unlist(lapply(1:M(x,t), function(k) cov(Z.time[[x]][[t]][,k],Z.time[[x]][[t+1]][,1])/ 
                           cov(Z.time[[x]][[t]][,1],Z.time[[x]][[t+1]][,1])))
  mu.alpha <- t(matrix(cbind(mu,alpha),nrow=M(x,t)))
  
  return(mu.alpha)
}



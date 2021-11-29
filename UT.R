#' @title Unscented Transform
#' 
#' @description Compute the integration (e.g. mean and variance) with a monomial rule.
#' 
#' @param n Integer. The dimension of integrated variable, i.e. number of factors.
#' @param m Integer. The dimension of approximation points. According to the appendix of CHS2010, \eqn{m = 2*n + 1}.
#' @param a Numeric. A column vector of updated mean of the factor.
#' @param k Numeric. The parameter kappa in the unscented transform. This is set to 2 by default.
#' @param P Matrix. Updated variance of the factor.
#' @param f Function. The integrated function, as in the form \eqn{\int f(\theta_t)\phi(\theta_t ; a, P)d\theta_t}
#' @return Numeric. The approximated integral value. 
#' 
#' @import
#' @importFrom 
#' @export
#' 


UT <- function(n,m,a,k,P,f){
  
  # Compute the square root of the positive definite (n * n) matrix P
  # The square root function is not finished.
  #sqrtmP <- sqrtm(P)
  sqrtmP <- sqrt(P)
  # A matrix (m * n) of approximation points, every row is one point
  asigma <- matrix(rbind(t(a), t(matrix(rep(a,n),ncol=n) + sqrt(n+k) * t(sqrtmP)),
                  t(matrix(rep(a,n),ncol=n) - sqrt(n+k) * t(sqrtmP))))
  
  # A column vector (m) of approximation weights
  wsigma <- matrix(rbind(k/(n+k), rep(1/(2*(n+k)),2*n)))
  
  # Compute the approximated integral value
  awsigma <- Reduce('+', lapply(1:m, function(s) wsigma[s,1] * f(t(asigma[s,]))))
  
  return(awsigma)
}




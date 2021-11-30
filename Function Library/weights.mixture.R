#' @title Updated weights for the factor mixture elements
#' 
#' @description Calculates the weights in time t that define the mixture of multivariate normal distributions. See equation (A6.4e) in Appendix A6 (CHS2010).
#' 
#' @param z Numeric. Vector at which we evaluate the density.
#' @param z.hat List. Each element in the list is the predicted measurement by the lth element of the mixture. See equation (A6.3) in Appendix A6 (CHS2010).
#' @param F.covar List. Each element in the list is given by equation (A6.4d). See appendix A6 in CHS (2010).
#' @param w0 Numeric. A vector of weights in \eqn{t-1} used to update weights in t.
#' 
#' @return Numeric. An \eqn{L}-dimensional vector containing \eqn{\tau_{\,l,t}}, \eqn{1\le l\le L}.
#' 
#' @author Maurcio Olivares
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords Nonlinear Filtering Unscented Transform

weights.mixture <- function(z,z.hat,F.covar,W0){
  
  # Number of marginal densities in the mixture distributions.
  S <- length(z.hat)
  # Calculate the marginal densities that form the mixture distribution
  marginal.densities <- unlist(lapply(1:S, function(s) pmvnorm(c(1,1),mean = z.hat[[s]],sigma = F.covar[[s]])))
  denumerator        <- sum(marginal.densities)
  updated.weights    <- marginal.densities/denumerator
  
  # Export results
  return(updated.weights)
}



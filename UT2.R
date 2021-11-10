#' @title Unscented Transform.
#' 
#' @description Compute the integration (e.g. mean and variance) with a monomial rule as in Appendix A6 (CHS2010).
#' 
#' @param n Integer. The dimension of integrated variable, i.e. number of factors.
#' @param m Integer. The dimension of approximation points. According to the appendix of CHS2010, \eqn{m = 2*n + 1}.
#' @param a Numeric. A column vector of updated mean of the factor.
#' @param k Numeric. The parameter kappa in the unscented transform. This is set to 2 by default.
#' @param P Matrix. Updated variance of the factor.
#' @param f Function. Observe that \eqn{f: \mathbb{R}^{N_{\theta}}\to\mathbb{R}^{N_{\theta}}}. This is the integrated function, as in the form \eqn{\int f(\theta_t)\phi(\theta_t ; a, P)d\theta_t}.
#' @param type String. The distribution of the error term in the transition equation (A6.1).
#' @param ... Extra arguments passed to the function f.
#' 
#' @return List. A list containing a_{t+1,t} and Sigma_{t+1,t}.
#' 
#' @author Maurcio Olivares
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords Nonlinear Filtering Unscented Transform


UT2 <- function(n,m,a,k=2,P,f,
                type = c( "normal", "lognormal", "exponential" ),...){
  
  # Calculate covariance matrix H. The main text assumes that
  # innivations are normally distributed and serially independent
  # across elements in \theta (e.g., equation 4.1), so H would be
  # a diagonal matrix with unknown shock variances along the diagonal.
  type = match.arg( type )
  
  # generate Y0
  if(type == "normal"){
    H <- diag(1,2)
  }
  else if(type == "lognormal"){
    H <- H <- diag(1,2)
  }
  else if(type == "exponential"){
    H <- H <- diag(1,2)
  } else {
    stop( paste( "Unrecognized type '", type, "'", sep="" ) )
  }
  
  
  # Approximate weights, a vector of size m. Observe that the weights
  # are the same for all the l elements in the mixture distribution.
  app.weights   <- c( k/(n+k), rep(1/(2*(n+k)),2*n) )
  # A matrix (m * n) of approximation points. Each column l, l=1,...,N_\theta, 
  # corresponds to x_{n,l,t,t} with n = 0,..., 2N_\theta.
  eval.points   <- rbind(a,do.call(rbind,lapply(1:2, function(s) a+sqrt(n+k)*sqrt(P)[s,])),
                  do.call(rbind,lapply(1:2, function(s) a-sqrt(n+k)*sqrt(P)[s,])))
  # Evaluate the function at x_{l,t,t} for all l=1,...,N_\theta. Returns an (m*n) matrix.
  f.eval.points <- apply(eval.points, 1, f)
  
  
  # Approximate a_{t+1,t}.
  a.update  <- app.weights*t(f.eval.points)
  
  # Approximate Sigma_{t+1,t}. 
  # Step 1. Define the centered moments.
  centered  <- t(f.eval.points)-a.update
  # Step 2. Create a list with all the elements in the summation
  sigma.t.l <- lapply(1:m, function(s) app.weights[s]*centered[s,]%*%t(centered[s,]))
  # Sum them all using the definition on page 20 in the Appendix
  Sigma.update   <- Reduce(`+`,sigma.t.l) + H

  # Return a_{t+1,t} and Sigma_{t,t} as a list
  object_results<-list() #Generates an empty list to collect all the required info
  object_results$a <- a.update
  object_results$S <- Sigma.update
  return(object_results)  
}




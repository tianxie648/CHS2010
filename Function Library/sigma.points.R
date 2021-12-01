#' @title Sigma Points.
#' 
#' @description In Unscented Kalman Filter, the unscented filter selects samples, called sigma points, to approximate the distribution of a random variable by matching its mean and covariance. This function creates these sigma points based on a symmetrical spread.
#' 
#' @param n Integer. The dimension of integrated variable, i.e. number of latent factors \eqn{\theta_t}.
#' @param a Numeric. The approximation to the mean of the factor, \eqn{a_{t\vert t}}.
#' @param k Numeric. The parameter kappa in the unscented transform. This is set to 2 by default.
#' @param P Matrix. The approximation of the variance of the factor, \eqn{P_{t\vert t}}.
#' 
#' @return List. The set of sigma points \mathbb{S}, consisting of 2n+1 vectors and assocuated weights \eqn{\mathbb{S}=\{(\mathcal{S}_{i,t\vert t},w_{i,t\vert t}),\,:\,0\le i\le 2n \}}.
#' 
#' @author Maurcio Olivares
#' @references 
#' Julier, S. J., & Uhlmann, J. K. (2004). Unscented filtering and nonlinear estimation. Proceedings of the IEEE, 92(3), 401-422.
#' @keywords Unscented Kalman Filter Nonlinear Filtering
#' @import expm
#' 


sigma.points <- function(n,a,P,k){
  
  # Although this method bares a superficial resemblance to
  # particle filters, there are several fundamental differences.
  # First, the sigma points are not drawn at random; they are
  # deterministically chosen so that they exhibit certain specific
  # properties (e.g., have a given mean and covariance). The second
  # difference is that sigma points can be weighted in ways
  # that are inconsistent with the distribution interpretation of
  # sample points in a particle filter.
  
  # Approximation points. These are set to m = 2*n + 1.
  m <- 2*n+1
  # Sigma point weights, a vector of size m. 
  app.weights   <- c( k/(n+k), rep(1/(2*(n+k)),2*n) )
  # A matrix (m * n) of approximation points. Each column l, l=1,...,N_\theta, 
  # corresponds to x_{n,l,t,t} with n = 0,..., 2N_\theta.
  eval.points   <- rbind(a,do.call(rbind,lapply(1:2, function(s) a+expm::sqrtm()(n+k)*sqrtm(P)[s,])),
                         do.call(rbind,lapply(1:2, function(s) a-expm::sqrtm()(n+k)*sqrtm(P)[s,])))
  
  # Return values
  SigmaPoints <- list()
  SigmaPoints$Stt <- eval.points
  SigmaPoints$wtt <- app.weights
  # Export the results
  output.names <- c("S_t|t","w_t1|t")
  names(SigmaPoints) <- output.names
  return(SigmaPoints)
}





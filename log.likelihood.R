#' @title Likelihood Function
#' 
#' @description Implementation of Nonlinear Filtering. Our goal is to obtain the log of the likelihood function (A6.10) in Appendix A6 in CHS(2010). We use the Schweppe Decomposition of the likelihood function.
#' 
#' @param Z Numeric. Matrix of observed data. Each row represents an individual.
#' @param a0 Numeric. Vector of initial conditions corresponding to \eqn{\mathbb{E}_0(\theta_0)}. 
#' @param P0 Numeric. Matrix of initial conditions corresponding to \eqn{\mathbb{V}_0(\theta_0)}.
#' @param gamma Numeric. Vector of size \eqn{N_\theta} with latent factor coefficients \eqn{\gamma_1,k,s,\dots,\gamma_5,k,s}, for \eqn{k\in\{C,N\}}. The vector must satisfy \eqn{\gamma_{j,k,s}\ge0} and \eqn{\sum_j \gamma_{j,k,s}=1}.
#' @param phi Numeric. Vector of complementarity parameters \eqn{\phi_{k,s}}, \eqn{k\in\{C,N\}}.
#' @param delta.eta Numeric. Variance of cognitive and noncognitive shocks \eqn{\eta_{k,t}\sim\mathcal{N}(0,\delta^2_{\eta,s})}. 
#' @param ... Extra arguments passed to the functions.
#' 
#' @return Numeric. The log likelihood function.
#' 
#' @author Maurcio Olivares
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords Likelihood Function Nonlinear Filtering Unscented Transform
#' @import expm numDeriv
#' 


log.likelihood <- function(gamma,phi,delta.eta,Z,a0,P0,...){
  
  # ----------------------------------- #
  # Define the dimensions of parameters
  N.z <- ncol(Z)
  N.theta <- length(a0)
  T.sample <- nrow(Z)
  # ----------------------------------------- #
  # Define the matrices that store the values.
  PP <- array(rep(NA, T.sample*N.theta*N.theta), dim=c(T.sample, N.theta, N.theta))
  aa <- matrix(NA,ncol = N.theta, nrow = T.sample)
  pred.error <- matrix(NA,ncol = N.z, nrow = T.sample)
  F.inverse  <- array(rep(NA, T.sample*N.z*N.z), dim=c(T.sample, N.z, N.z))
  # Store vectors for Schweppe Decomposition.
  determ.t <- rep(NA,T.sample)
  weighted.error <- rep(NA,T.sample)
  
  # -------------------------------------------------------------- #
  # Initial conditions
  moments <- UT2(N.theta,a0,P0,f="no.anchor",h="linear",delta.eta=delta.eta,gamma=gamma,phi=phi)
  
  for (t in 1:T.sample) {
    
    # Prediction errors
    pred.error[t,] <- Z[t,]-moments$`y.hat_t`
    F.inverse[t,,] <- solve(moments$`V_t[g(theta_t+1)]`+H)
    # Arguments for the Schweppe Decomposition of the likelihood function
    log.det[t]  <- log(det(moments$`V_t[g(theta_t+1)]`+H))
    weighted.error[t] <- pred.error[t,]%*%determ.t[t]%*%t(pred.error[t,])
    # Update recursions
    aa[t,]  <- moments$`a_t+1|t`+moments$`Cov_t[ theta_t,y_t]`%*%F.inverse[t,,](pred.error[t,])
    PP[t,,] <- moments$`P_t+1|t`-moments$`Cov_t[ theta_t,y_t]`%*%F.inverse[t,,]%*%t(moments$`Cov_t[ theta_t,y_t]`)
    # Prediction recursions
    moments <- UT2(N.theta,aa[t,],PP[t,,],f="CES",h="linear",delta.eta=delta.eta,gamma=gamma,phi=phi)
    
  }
  
  # --------------------------------------------------------------- #
  # We exploit normality and the Kalman Filter structure to
  # implement the Schweppe Decomposition of the likelihood function
  
  log.lik <- (N.z*T.sample/2)*log(2*pi)+(1/2)*sum(log.det)+(1/2)*sum(weighted.error)
  return(-log.lik)
}






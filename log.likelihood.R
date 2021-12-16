#' @title Likelihood Function
#' 
#' @description Implementation of Nonlinear Filtering. Our goal is to obtain the log of the likelihood function (A6.10) in Appendix A6 in CHS(2010). We use the Schweppe Decomposition of the likelihood function.
#' 
#' @param psi Numeric. Vector of dimension (k x N.theta x S + k x S + k x S), where k is the skill (cognitive or non-cognitive), N.theta is the number of latent coefficients, and S is the number of stages in childhood developent. It is worth mentioning that the order in psi matters: the first (k x N.theta x S) entries are \eqn{(\gamma_{1,C,1},\gamma_{1,N,1},\gamma_{2,C,1},\gamma_{2,N,1},\dots,\gamma_{5,C,2},\gamma_{5,N,2})}. The next \eqn{k\times S} entries are \eqn{(\phi_{C,1},\phi_{N,1},\phi_{C,2},\phi_{N,2})}, and the last \eqn{k\times S} entries are \eqn{(\delta^2_{C,1},\delta^2_{N,1},\delta^2_{C,2},\delta^2_{N,2})}. 
#' @param Z Numeric. Matrix of observed data. Each row represents an individual.
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


log.likelihood <- function(psi,Z,...){
  
  # -------------------------------------------------------------------------------- #
  # Rearrange the parameters to feed the functions. Let S be the number of stages in
  # childhood development. Then, the functions are given by
  #
  # phi:       Array of dimensions k*1*S. For S=s, phi[,,s] is the 
  #            Vector of complementarity parameters \eqn{\phi_{k,s}}, \eqn{k\in\{C,N\}}.
  # gamma:     Array of dimensions k*N.theta*S. For S=s, gamma[,,s] is the matrix of
  #            latent coefficients \eqn{\gamma_1,k,s,\dots,\gamma_5,k,s}, for \eqn{k\in\{C,N\}}. 
  #            The row-vector must satisfy \eqn{\gamma_{j,k,s}\ge0} and \eqn{\sum_j \gamma_{j,k,s}=1}.
  # delta.eta  Variance of cognitive and noncognitive shocks \eqn{\eta_{k,t}\sim\mathcal{N}(0,\delta^2_{\eta,s})}.
  phi        <- array(psi[(N.theta*k*S+1):(N.theta*k*S+k*S)], dim = c(k,1,S))
  gamma      <- array(psi[1:(N.theta*k*S)], dim = c(k,N.theta,S))
  delta.eta  <- array(psi[tail(psi,k*S)], dim = c(k,1,S))
  
  # ------------------------------------------------ #
  # Initial moment conditions
  # a0:  Vector of initial conditions corresponding
  #      to \eqn{\mathbb{E}_0(\theta_0)}. 
  # P0:  Matrix of initial conditions corresponding 
  #      to \eqn{\mathbb{V}_0(\theta_0)}.
  # ------------------------------------------------ #
  a0 <- rep(1,N.theta)
  P0 <- diag(1,ncol = N.theta)
  
  # ----------------------------------- #
  # Define the dimensions of parameters
  N.z <- ncol(Z)
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
    moments <- UT2(N.theta,aa[t,],PP[t,,],f="no.anchor",h="linear",delta.eta=delta.eta,gamma=gamma,phi=phi)
    
  }
  
  # --------------------------------------------------------------- #
  # We exploit normality and the Kalman Filter structure to
  # implement the Schweppe Decomposition of the likelihood function
  
  log.lik <- (N.z*T.sample/2)*log(2*pi)+(1/2)*sum(log.det)+(1/2)*sum(weighted.error)
  return(-log.lik)
}






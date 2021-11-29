#' @title Unscented Transform.
#' 
#' @description Compute the integration (e.g. mean and variance) with a monomial rule as in Appendix A6 (CHS2010).
#' 
#' @param n Integer. The dimension of integrated variable, i.e. number of latent factors \eqn{\theta_t}.
#' @param a Numeric. A column vector of updated mean of the factor.
#' @param k Numeric. The parameter kappa in the unscented transform. This is set to 2 by default.
#' @param P Matrix. Updated variance of the factor.
#' @param f Function. Transition equation in the state space representation, equation (A6.1). Observe that \eqn{f: \mathbb{R}^{N_{\theta}}\to\mathbb{R}^{N_{\theta}}}. This is the integrated function, as in the form \eqn{\int f(\theta_t)\phi(\theta_t ; a, P)d\theta_t}.
#' @param h Function. Measurement equation in the state space representation, equation (A6.2). Observe that \eqn{h: \mathbb{R}^{N_{\theta}}\to\mathbb{R}^{N_{z}}}, where \eqn{N_z} is the dimension of the vector of observed variables, the measurements \eqn{Z_{a,k,t,j}}.
#' @param ... Extra arguments passed to the functions f and h (e.g. latent factor parameters and elasticities).
#' 
#' @return List. A list containing \eqn{a_{t+1,t}}, \eqn{\Sigma_{t+1,t}}, \eqn{\hat{y}_t}, \eqn{\mathbb{V}_t(h(\theta_{t+1}))}, \eqn{\mathbb{C}_t(\theta_t,y_t)}, and \eqn{\mathbb{V}_t(y_{t+1})}.
#' 
#' @author Maurcio Olivares
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords Nonlinear Filtering Unscented Transform
#' @import expm


UT2 <- function(n,a,P,k=2,f="CES",h="linear",delta.eta=c(1,1),...){
  
  # Approximation points. According to the appendix of CHS2010, m = 2*n + 1.
  m <- 2*n+1
  # Approximate weights, a vector of size m. Observe that the weights
  # are the same for all the l elements in the mixture distribution.
  app.weights   <- c( k/(n+k), rep(1/(2*(n+k)),2*n) )
  # A matrix (m * n) of approximation points. Each column l, l=1,...,N_\theta, 
  # corresponds to x_{n,l,t,t} with n = 0,..., 2N_\theta.
  eval.points   <- rbind(a,do.call(rbind,lapply(1:2, function(s) a+sqrt(n+k)*sqrtm(P)[s,])),
                  do.call(rbind,lapply(1:2, function(s) a-sqrt(n+k)*sqrtm(P)[s,])))
  # Evaluate the function at x_{l,t,t} for all l=1,...,N_\theta. Returns an (m*n) matrix.
  f.eval.points <- apply(eval.points, 1, f)
  
  # -------------------------------------- #
  # Approximate a_{t+1,t}.
  a.update  <- app.weights%*%t(f.eval.points)
  
  # -------------------------------------- #
  # Approximate Sigma_{t+1,t}
  # Step 1. Define the centered moments.
  centered  <- t(apply(f.eval.points, 2, function(s) s-t(a.update)))
  # Step 2. Create a list with all the elements in the summation
  sigma.t.l <- lapply(1:m, function(s) app.weights[s]*centered[s,]%*%t(centered[s,]))
  # Sum them all using the definition on page 20 in the Appendix
  Sigma.update   <- Reduce(`+`,sigma.t.l) + diag(c(delta.eta,rep(0,n-length(delta.eta))),ncol = n)
  
  # -------------------------------------- #
  # Approximate E_t[ h(\theta_t+1) ]
  h.f.eval.points <- apply(f.eval.points, 1, h)
  y.hat <- app.weights%*%t(h.f.eval.points)
  
  # -------------------------------------- #
  # Approximate V_t[ h(\theta_t+1) ]
  centered.h  <- t(apply(h.eval.points, 2, function(s) s-t(y.hat)))
  # Step 2. Create a list with all the elements in the summation
  V.h.t.list <- lapply(1:ncol(centered.h), function(s) app.weights[s]*centered.h[s,]%*%t(centered.h[s,]))
  # Sum them all using the definition on page 20 in the Appendix
  V.h.t   <- Reduce(`+`,V.h.t.list)
  
  # -------------------------------------- #
  # Approximate Cov_t[ \theta_t,y_t ]
  Cov.y.theta.list <- lapply(1:m, function(s) app.weights[s]*centered[s,]%*%t(centered.h[s,]))
  Cov.y.theta   <- Reduce(`+`,Cov.y.theta.list)
  
  # -------------------------------------- #
  # Approximate V_t[ y_t+1 ]
  V.y.t <- V.h.t + H
  

  # Return a_{t+1,t} and Sigma_{t+1,t} as a list
  object_results<-list() #Generates an empty list to collect all the required info
  object_results$a <- a.update
  object_results$P <- Sigma.update
  object_results$y.hat <- y.hat
  bject_results$V.h.t <- V.h.t
  bject_results$Cov.y.theta <- Cov.y.theta
  bject_results$V.y.t <- V.y.t
  
  # Export Results
  output.names <- c("a_t+1|t","P_t+1|t","y.hat_t","V_t[h(theta_t+1)]","Cov_t[ theta_t,y_t]","V_t[y_t+1]")
  names(object_results) <- output.names
  return(object_results)  
}


"CES" <- function(theta,gamma=rep(1/length(theta),length(theta)),phi=c(.5,.5),delta.eta=c(1,1),stage=1){
  
  # This is the production technology in Equation (4.1) in CHS(2010).
  # The production function follows a CES specification. Generally speaking,
  # the production function is allowed to change at different childhood 
  # development stages s. The inputs are
  
  # theta.      This is the vector of latent factors in time t.
  # gamma.      The latent factor coefficients. These are quantities to
  #             be estimated by MLE and must satisfy certain properties.
  # phi.        Complementarity parameter. It has two elements, indicating
  #             whether it is a cognitive or noncognitive skill. This is a
  #             parameter to be estimated by MLE.
  # delta.eta   Variance of cognitive and noncognitive shocks. Also to be 
  #             estimated by MLE.
  # s           Development stage. It is set to 1 but the text considers two
  #             development stages.
  
  # In the paper, the only two latent factors that evolve according
  # to the nonlinear transition equation are the childhood cognitive 
  # and noncognitive factors or skills, \theta_k,t for k=C,N. The 
  # rest of the factors (parental skills and investment) do not evolve 
  # in this project. 
  
  # The weights gamma must satisfy some restrictions.
  # Checking the weights are between [0,1] and add up to 1.
  if( !( sum(gamma)==1 & all(gamma>=0) & all(gamma<=1)) ){
    print("Factor coefficients must be in the [0,1] interval and add up to one.")
    stop()
  }
  
  # Updated latent factor according to transition equation ( CES )
  theta.k <- lapply(1:2,function(s) (gamma%*%theta^(phi[s]))^(1/(phi[s]))*exp(rnorm(1,0,delta.eta[s])))
  # Export results
  theta.update <- c(unlist(theta.k),theta[3:length(theta)])
  return(theta.update)
}


"linear" <- function(theta,factor.loadings=rep(1,length(theta)),M=c(2,2,2,2,2,2),Z.mean=rep(0,length(theta))){
  
  # This corresponds to the measurement equations in equations
  # (3.1) - (3.3). It is a linear function and corresponds with 
  # equation (A6.2) in the state space representation of the model.
  # These measurements are proxying the latent factors theta_t. 
  
  # Z_1:    Teacher assesment of child cognitive and noncognitive skills
  # Z_2:    Investment on cognitive and noncognitive skills.
  # Z_3:    Parental cognitive and noncognitive endowments. 
  
  # Each measurement Z is indexed by the latent factor they are proxying 
  # (a=1,2,3), the skill (k=C,N), the time of the measurement (t). Moreover,
  # for each of those, we may have multiple measures. For example, for Z_1
  # we may have 8 different tests for cognitive skills and 3 for noncognitive.
  # Therefore, Z is subindexed by Z_a,k,t,j(a,k) where j(a,k) is allowed to
  # vary by type of latent factor and skills.
  
  # theta.           Latent Factors.
  # factor.loadings  The vector of factor loadings associated with latent factors.
  #                  The order matters: the first M_ak entries correspond with
  #                  latent factor a=1,2,3 and skill k=C,N
  # M                The number of measurements that indicate when the cutoff of 
  #                  indeces in factor.loadings are. It's a vector of size 6 because
  #                  there are six latent factors. For example, if M[5]=8 it means
  #                  that parental cognitive endowment, Z_{3,C}, has j=8 measurements.
  #                  The default indicates that we have 2 measurements for each Z_a,k.
  # Z.mean           Mean vector with the mean of each Z_{a,k,t,j}.
  
  # Measurements for teacher assesment of child cognitive skills
  Z.1.c <- Z.mean[1:M[1]]+log(theta[1])*factor.loadings[1:M[1]]+rmvnorm(1,rep(0,M[1],diag(1,M[1])))
  
  # Measurements for teacher assesment of child noncognitive skills
  Z.1.n <- Z.mean[(M[1]+1):(M[1]+M[2])]+log(theta[2])*factor.loadings[(M[1]+1):(M[1]+M[2])]+rmvnorm(1,rep(0,M[2],diag(1,M[2])))

  # Measurements for investment on cognitive skills
  Z.2.c <- Z.mean[(M[2]+1):(M[2]+M[3])]+log(theta[3])*factor.loadings[(M[2]+1):(M[2]+M[3])]+rmvnorm(1,rep(0,M[3],diag(1,M[3])))
 
  # Measurements for investment on noncognitive skills
  Z.2.n <- Z.mean[(M[3]+1):(M[3]+M[4])]+log(theta[4])*factor.loadings[(M[3]+1):(M[3]+M[4])]+rmvnorm(1,rep(0,M[4],diag(1,M[5])))
  
  # Measurements for parental cognitive skills
  Z.3.c <- Z.mean[(M[4]+1):(M[4]+M[5])]+log(theta[5])*factor.loadings[(M[4]+1):(M[4]+M[5])]+rmvnorm(1,rep(0,M[5],diag(1,M[5])))
  
  # Measurements for parental noncognitive skills
  Z.3.n <- Z.mean[(M[5]+1):(M[5]+M[6])]+log(theta[6])*factor.loadings[(M[5]+1):(M[5]+M[6])]+rmvnorm(1,rep(0,M[6],diag(1,M[6])))
  
  # Export Results
  Z <- c(Z.1.c,Z.1.n,Z.2.c,Z.2.n,Z.3.c,Z.3.n)
  return(Z)
}
















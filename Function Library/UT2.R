#' @title Unscented Transform.
#' 
#' @description Compute the integration (e.g. mean and variance) with a monomial rule as in Appendix A6 (CHS2010).
#' 
#' @param n Integer. The dimension of integrated variable, i.e. number of latent factors \eqn{\theta_t}.
#' @param t Integer. Time periods.
#' @param M Numeric. matrix of dimension 5xTime. The number of measurements for each factor at each period. Each column of the matrix represents each period. Each row is by order: Child Cognitive, Child Noncognitive, Investment, Parental Cognitive, Parental Noncognitive
#' @param a Numeric. A column vector of updated mean of the factor.
#' @param k Numeric. The parameter kappa in the unscented transform. This is set to 2 by default.
#' @param P Matrix. Updated variance of the factor.
#' @param f Type of anchoring used in the production function. It admits three values: the default option, no anchoring ("no.anchor"), linear anchoring ("linear.anchor") and nonlinear anchoring ("nonlinear.anchor"). Thus, this argument selects a type of production technology \eqn{f} depending on the type, i.e. the transition equation in the state space representation, equation (A6.1). Observe that \eqn{f: \mathbb{R}^{N_{\theta}}\to\mathbb{R}^{N_{\theta}}}. This is the integrated function, as in the form \eqn{\int f(\theta_t)\phi(\theta_t ; a, P)d\theta_t}.
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


UT2 <- function(n,t,M,a,P,f="no.anchor",h="linear",delta.eta=c(1,1),...){
  
  # Generate stage
  stage <- 1 + as.numeric(t>4)
  
  # Generate the Sigma points
  SigmaPoints <- sigma.points(n,a,P,k=2)
  # Evaluate the function at S_{i,t,t} for all i=0,...,2N_\theta. Returns an (m*n) matrix.
  f.eval.points <- apply(SigmaPoints$Stt, 1, f)
  
  # -------------------------------------- #
  # Time Update (filter process). 
  
  # Step 1. Update mean. This generates a_{t+1,t}.
  a.update  <- SigmaPoints$wtt%*%t(f.eval.points)
  # -------------------------------------- #
  # Step 2. Update variance. This generates Sigma_{t+1,t}
  # Step 2.a. Define the centered moments.
  centered  <- t(apply(f.eval.points, 2, function(s) s-t(a.update)))
  # Step 2.b. Create a list with all the elements in the summation
  sigma.t.l <- lapply(1:m, function(s) SigmaPoints$wtt[s]*centered[s,]%*%t(centered[s,]))
  # Step 2c. Sum them all using the definition on page 20 in the Appendix
  Sigma.update   <- Reduce(`+`,sigma.t.l) + diag(c(delta.eta,rep(0,n-length(delta.eta))),ncol = n)
  
  # ------------------------------------------ #
  # Measurement Update (update moments)
  
  # Approximate E_t[ h(\theta_t+1) ]
  h.f.eval.points <- apply(f.eval.points, 1, h)
  y.hat <- SigmaPoints$wtt%*%t(h.f.eval.points)
  # -------------------------------------- #
  # Approximate V_t[ h(\theta_t+1) ]
  centered.h  <- t(apply(h.eval.points, 2, function(s) s-t(y.hat)))
  # Step 2. Create a list with all the elements in the summation
  V.h.t.list <- lapply(1:ncol(centered.h), function(s) SigmaPoints$wtt[s]*centered.h[s,]%*%t(centered.h[s,]))
  # Sum them all using the definition on page 20 in the Appendix
  V.h.t   <- Reduce(`+`,V.h.t.list)
  # -------------------------------------- #
  # Approximate Cov_t[ \theta_t,y_t ]
  Cov.y.theta.list <- lapply(1:m, function(s) SigmaPoints$wtt[s]*centered[s,]%*%t(centered.h[s,]))
  Cov.y.theta   <- Reduce(`+`,Cov.y.theta.list)
  # -------------------------------------- #
  # Approximate V_t[ y_t+1 ]
  V.y.t <- V.h.t + H

  # Return moments
  object_results<-list() 
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


"no.anchor" <- function(theta,gamma,phi=c(.5,.5),delta.eta=c(1,1),stage,n.stage=2){
  
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
  for (i in 1:n.stage) {
    for (k in 1:2) {
      if( !( sum(gamma[k,,i])==1 & all(gamma[k,,i]>=0) & all(gamma[k,,i]<=1)) ){
        print("Factor coefficients must be in the [0,1] interval and add up to one.")
        stop()
      }
    }
  }
  
  # Updated latent factor according to transition equation ( CES )
  
  theta.k <- lapply(1:2,function(k) (gamma[k,,stage]%*%theta^(phi[k]))^(1/(phi[k]))*exp(rnorm(1,0,delta.eta[k])))
  # Export results
  theta.update <- c(unlist(theta.k),theta[3:length(theta)])
  return(theta.update)
}

"linear.anchor" <- function(theta,gamma,phi=c(.5,.5),delta.eta=c(1,1),stage=n.stage, alpha4=c(1,1,1)){
  
  # This is the production technology in Equation (4.1) in CHS(2010).
  # The main difference with respect to the "no.anchor" case is that
  # now the production technology takes as inputs the transformed 
  # skills, where the transformation is precisely the linear anchor.
  # For this reason, there is only one new argument in the function,
  # denoted alpha4. See function "no.anchor" for the full documentation.
  
  # alpha4      Factor loadings associated with the stocks of cognitive and 
  #             non-cognitive skills for outcome j. Thus, alpha4 would vary
  #             for each outcome Z_{4,j} of interest. Examples of outcomes
  #             include high school graduation, criminal activity, drug use, 
  #             and teenage pregnancy. In here, we assume there is only one 
  #             outcome of interest so alpha4 is a 3-dimensional vector whose
  #             elements are mu_{4,1}, alpha_{4,C,1}, and alpha_{4,N,1}. If
  #             the outcome of interest varies, so does alpha4. See Sections
  #             3.5 in CHS(2010) and Appendix A7.1 and A7.2 in CHS(2010, App)

  
  # The weights gamma must satisfy some restrictions.
  # Checking the weights are between [0,1] and add up to 1.
  for (i in 1:stage) {
    for (k in 1:2) {
      if( !( sum(gamma[k,,i])==1 & all(gamma[k,,i]>=0) & all(gamma[k,,i]<=1)) ){
        print("Factor coefficients must be in the [0,1] interval and add up to one.")
        stop()
      }
    }
  }
  
  # Transform the latent factors
  
  theta.star <- c(exp(alpha4[1]+alpha4[2]*log(theta[1])),
                  exp(alpha4[1]+alpha4[3]*log(theta[2])),
                  theta[-(1:2)])
  
  # Updated latent factor according to transition equation ( CES )
  theta.k <- lapply(1:2,function(k) -alpha4[1]/alpha4[(s+1)]+(1/alpha4[(s+1)])*(gamma[k,,stage]%*%theta.star^(phi[k]))^(1/(phi[k]))*exp(rnorm(1,0,delta.eta[k])))
  # Export results
  theta.update <- c(unlist(theta.k),theta[3:length(theta)])
  return(theta.update)
}

"nonlinear.anchor" <- function(theta,gamma=rep(1/length(theta),length(theta)),phi=c(.5,.5),delta.eta=c(1,1),stage=1, alpha4=c(1,1,1)){
  
  # This is the production technology in Equation (4.1) in CHS(2010).
  # The main difference with respect to the "no.anchor" case is that
  # now the production technology takes as inputs the transformed 
  # skills, where the transformation is precisely the linear anchor.
  # For this reason, there is only one new argument in the function,
  # denoted alpha4. See function "no.anchor" for the full documentation.
  
  # alpha4      Factor loadings associated with the stocks of cognitive and 
  #             non-cognitive skills for outcome j. Thus, alpha4 would vary
  #             for each outcome Z_{4,j} of interest. Examples of outcomes
  #             include high school graduation, criminal activity, drug use, 
  #             and teenage pregnancy. In here, we assume there is only one 
  #             outcome of interest so alpha4 is a 3-dimensional vector whose
  #             elements are mu_{4,1}, alpha_{4,C,1}, and alpha_{4,N,1}. If
  #             the outcome of interest varies, so does alpha4. See Sections
  #             3.5 in CHS(2010) and Appendix A7.1 and A7.2 in CHS(2010, App)
  
  
  # The weights gamma must satisfy some restrictions.
  # Checking the weights are between [0,1] and add up to 1.
  if( !( sum(gamma)==1 & all(gamma>=0) & all(gamma<=1)) ){
    print("Factor coefficients must be in the [0,1] interval and add up to one.")
    stop()
  }
  # Transform the latent factors
  
  # Updated latent factor according to transition equation ( CES )
  theta.k <- lapply(1:2,function(k) (gamma%*%theta^(phi[k]))^(1/(phi[k]))*exp(rnorm(1,0,delta.eta[k])))
  # Export results
  theta.update <- c(unlist(theta.k),theta[3:length(theta)])
  return(theta.update)
}


"linear" <- function(theta,factor.loadings=rep(1,length(theta)),M,t,Z.mean=rep(0,length(theta))){
  
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
  #                  indeces in factor.loadings are. Matrix of dimension 5xTime. 
  #                  Each column of the matrix represents each period. Each row 
  #                  is by order: Child Cognitive, Child Noncognitive, Investment, 
  #                  Parental Cognitive, Parental Noncognitive
  # Z.mean           Mean vector with the mean of each Z_{a,k,t,j}.
  
  # Measurements for teacher assesment of child cognitive skills
  Z.1.c <- Z.mean[1]+log(theta[1])*factor.loadings[1]+rmvnorm(1,rep(0,M[1,t]),diag(1,M[1,t]))
  
  # Measurements for teacher assesment of child noncognitive skills
  Z.1.n <- Z.mean[2]+log(theta[2])*factor.loadings[2]+rmvnorm(1,rep(0,M[2,t]),diag(1,M[2,t]))

  # Measurements for investment
  Z.2 <- Z.mean[3]+log(theta[3])*factor.loadings[3]+rmvnorm(1,rep(0,M[3,t]),diag(1,M[3,t]))
 
  # Measurements for parental cognitive skills
  Z.3.c <- Z.mean[4]+log(theta[4])*factor.loadings[4]+rmvnorm(1,rep(0,M[4,t]),diag(1,M[4,t]))
  
  # Measurements for parental noncognitive skills
  Z.3.n <- Z.mean[5]+log(theta[5])*factor.loadings[5]+rmvnorm(1,rep(0,M[5,t]),diag(1,M[5,t]))
  
  # Export Results
  Z <- c(Z.1.c,Z.1.n,Z.2,Z.3.c,Z.3.n)
  return(Z)
}
















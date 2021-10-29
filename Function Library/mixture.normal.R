#' @title  Generate data from a mixture of multivariate normal distributions.
#'
#' @description Generate data from a mixture distribution whose \eqn{j}-th marginal element are  normally distributed with mean \eqn{a_{j,t+k,t}=\mathbb{E}(\theta_{t+k}\vert z')} and variance \eqn{\Sigma_{t+k,t}=\mathbb{V}(\theta_{t+k}\vert z')}.                
#' 
#' @param mu List. There are \eqn{L} elements in the list. Every element in the list corresponds with a \eqn{J}-dimensional mean vector \eqn{a_{l,t+k,t}}, \eqn{1\le l \le L}.
#' @param sigma List. There are \eqn{L} elements in the list. Every element in the list corresponds with a \eqn{J\times J} covariance matrix \eqn{\Sigma_{l,t+k,t}}, \eqn{1\le l \le L}.
#' @param tau Numeric. A vector of weights \eqn{\tau_{l,t}\in[0,1]} such that \eqn{\sum_{l}\tau_{l,t}=1}.
#' @param draws Numeric. Number of samples to be drawn from the mixture distribution.
#' @return Numeric. A \eqn{N\times J} matrix, where \eqn{N} is the number of draws and each row \eqn{j=1,\dots,J} is the draw from the mixture distribution.
#'
#' @author Maurcio Olivares
#' @keywords mixture distribution multivariate normal
#' @import mvtnorm
#' @importFrom stats
#' @export
#' 

mixture.normal <- function(mu,sigma,tau,draws){
  
  # Checking the weights are between [0,1] and add up to 1.
  if( !( sum(tau)==1 & all(tau>=0) & all(tau<=1)) ){
    print("Weights must be in the [0,1] interval and add up to one.")
    stop()
  }
  # Checking that elements in mean and sigma have conforming sizes.
  if( !(  sum(unlist(lapply(1:length(mu), function(i) dim(sigma[[i]])[1]==length(mu[[1]]))))==length(mu[[1]]) )){
    print("Mean vector and its corresponding variance matrix must have conforming arguments.")
    stop()
  }
  
  # Generate N draws from the mixture of multivariate normal.
  components <- sample(1:length(tau),prob=tau,size=draws,replace=TRUE)
  sim.draws  <- lapply(1:draws, function(s) rmvnorm(n = 1, mean =  mu[components][[s]], sigma=sigma[components][[s]]))
  
  # Export the results.
  return(do.call(rbind,sim.draws))
}





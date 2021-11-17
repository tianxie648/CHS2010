#' @title Monte Carlo Simulation
#' 
#' @description Generate Monte Carlo simulations as in 4.2.5 (CHS2010).
#' 
#' @param kappa.theta Numeric.
#' @param kappa.y     Numeric.
#' @param gamma1      Numeric. 
#' @param gamma3      Numeric.
#' @param phi         Numeric.
#' @param rho.y       Numeric. The coefficient in the income process as in (4.3) (CHS2010)
#' @param rho.pi      Numeric. The coefficient in the equation of motion of pi_t as in (4.4) (CHS2010)
#' @param T           Integer. Number of time periods.
#' @param N           Integer. Number of individuals.
#' @param rn.seed        Integer. Random seed.
#' 
#' @return Data frame. A data frame of simulated data.
#' 
#' @author
#' @references 
#' Supplement to "Estimating the Technology of Cognitive and Noncognitive Skill Formation": Appendix (Econometrica, Vol. 78, No.3, May 2010, 883-931)
#' @keywords
#' @import


gen.data <- function(kappa.theta, kappa.y, gamma1, gamma3, phi, rho.y, rho.pi, T=8, N=2200, rn.seed){
  set.seed(rn.seed)
  result <- data.frame(z1=z1, etc)
  return(result)
}

#' @title Reshaping function for a simulated dataset
#' 
#' @description Mapping a simulated dataset to the ideally-shaped list
#' 
#' @param data List. Particularly, this is the simulated data.
#' @param Time Integer. Number of time periods.
#' 
#' @return List. A list of measurements across all time periods.


reshape.sim <- function(data, Time = 8){
  result <- lapply(1:Time, function(t) NA)
  for (t in 1:Time){
    result[[t]] <- c(data[[2]][[t]],data[[3]][[t]],data[[4]][[t]],data[[5]],data[[6]])
  }
  return(result)
}




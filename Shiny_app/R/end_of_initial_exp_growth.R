end_of_initial_exp_growth <- function(od_values){
  # Function to identify the initial exponential growth
  drop <- FALSE
  i <- 5 # worked well 5
  increase_factor <- 5
  max_od_observed <- max(od_values[1:i])
  while (!(drop)){
    if (all(od_values[i:(i+5)] < max_od_observed * 0.8 & max_od_observed > od_values[1] * increase_factor)){
      drop <- TRUE
    } else {
      if (od_values[i] > max_od_observed){ 
        max_od_observed <- od_values[i]
        }
      i <- i+1
    }
    if (i > length(od_values)){
      increase_factor <- increase_factor - 1
      i <- 5
      if (increase_factor < 0){
        warning("increase_factor in end_of_initial_exp_growth reached zero")
      }
    }
  }
  
  return(i)
}
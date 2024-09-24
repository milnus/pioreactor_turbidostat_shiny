density_estimate_function <- function(od_values, index_for_exp_growth_end){
  # Filter out all od values below the index marking end of initial exponential growth phase.
  od_values <- od_values[index_for_exp_growth_end:length(od_values)]
  
  # Identify max OD from the 75% first of the first exponential growth
  index_75p <- ceiling(length(od_values)*0.75)
  max_od_observed <- max(od_values[1:index_75p])
  print(paste("max_od_observed:", max_od_observed))
  
  top <- FALSE
  top_cut_i <- length(od_values)
  while(!(top) | top_cut_i <= 0){
    if (od_values[top_cut_i] <= max_od_observed){
      top <- TRUE
    } else {
      top_cut_i <- top_cut_i - 1
    }
  }
  if (top_cut_i != 0){
    od_values <- od_values[1:top_cut_i]
  }
  
  return(IQR(od_values))
}
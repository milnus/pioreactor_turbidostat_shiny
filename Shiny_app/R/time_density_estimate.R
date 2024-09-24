time_density_estimate <- function(time_stamps, od_values) {
  # Function to set an outlier cutoff for time between measurements.
 
  time_diff <- c(NA, time_stamps[2:length(time_stamps)] - time_stamps[1:(length(time_stamps)-1)])
  print(paste("quantile(time_diff, 0.99):", quantile(time_diff, 0.99, na.rm = T)))
  
  time_delta <- quantile(time_diff, 0.99, na.rm = T)
  
  return(time_delta)
}
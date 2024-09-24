spline_peak_detect <- function(spline_smooted_od, time_stamps){
  # Identify the range of most point dense part
  dense_od_range <- density_estimate_function(od_values = spline_smooted_od)
  print(paste("dense_od_range:", dense_od_range))
  
  # delta <- dense_od_range/2
  delta <- dense_od_range * 0.5
  
  print(paste("delta:", delta))
  
  peaks <- peakdet(spline_smooted_od, 
                   delta, 
                   time_stamps)
  
  spline_peak_df <- do.call("rbind", peaks)
  
  # print(ggplot(data.frame(spline_smooted_od, hours = time_stamps), aes(hours, spline_smooted_od)) + 
    # geom_point() +
      # geom_point(inherit.aes = F, data = spline_peak_df, aes(pos, val), col = "orange"))
  
  return(peaks)
}
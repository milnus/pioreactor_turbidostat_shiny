# Get the mean over a window of od values, discarding outliers
roll_mean_summary <- function(hours, od_values, outliers){
  non_NA <- !is.na(od_values)
  hours_filt <- hours[non_NA & !outliers]
  od_values_filt <- od_values[non_NA & !outliers]
  
  rolling_mean <- zoo::rollmean(od_values_filt, k = 11, fill = NA, align = 'center')
  
  return(rolling_mean)
}
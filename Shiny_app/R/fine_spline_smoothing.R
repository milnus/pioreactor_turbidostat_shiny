fine_spline_smoothing <- function(hours, od_values, outliers){
  # Function to find a highly knotted spline that smooths the growth curve.
  non_NA <- !is.na(od_values)
  hours_filt <- hours[non_NA & !outliers]
  od_values_filt <- od_values[non_NA & !outliers]
  
  spline_model <- lm(od_values_filt ~ ns(hours_filt, df = 300))
  
  spline_od <- predict(spline_model, hours_filt)
  
  return(spline_od)
}
isolate_growth_curves_spline <- function(timestamps, od_readings, spline_segment_id, outlier_determination){

  # Set outlier measurements to NA
  od_readings[outlier_determination] <- NA
  # Isolate rows where there are OD readings
  timestamps <- timestamps[!is.na(od_readings)]
  spline_segment_id <- spline_segment_id[!is.na(od_readings)]
  outlier_determination <- outlier_determination[!is.na(od_readings)]
  od_readings <- od_readings[!is.na(od_readings)]
  
  # aggregate()
  growth_list <- sapply(1:max(spline_segment_id, na.rm = T), function(segment_id) data.frame('timestamp' = timestamps[spline_segment_id == segment_id & !is.na(spline_segment_id)],
                                                                                  'od_reading' = od_readings[spline_segment_id == segment_id & !is.na(spline_segment_id)]), simplify = F)

  return(growth_list)
}
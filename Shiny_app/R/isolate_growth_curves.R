# Use the peak detection to identify consecutive turbidostat growths that are not a truncated due to an outlier measurement.
isolate_growth_curves <- function(timestamps, od_readings, min_max_indication, outlier_determination){
  # Set outlier measurements to NA
  od_readings[outlier_determination] <- NA
  # Isolate rows where there are OD readings
  timestamps <- timestamps[!is.na(od_readings)]
  min_max_indication <- min_max_indication[!is.na(od_readings)]
  outlier_determination <- outlier_determination[!is.na(od_readings)]
  od_readings <- od_readings[!is.na(od_readings)]

  # Find time points for min and max
  min_max_timestamps <- timestamps[!is.na(min_max_indication)]

  if (length(min_max_timestamps) %% 2 == 0){
    # add in the last stretch of growth to keep the final exponential growth
    min_max_timestamps <- c(min_max_timestamps, max(timestamps))

    # # For now we remove and ignore the last point is we end on a minimum (even min_max_timestamps)
    # min_max_timestamps <- min_max_timestamps[1:(length(min_max_timestamps)-1)]
  }

  # Construct list to hold individual growths
  growth_list <- list()

  # Save the first part of the growth curve (lag-phase into initial exponential phase)
  growth_list[[1]] <- data.frame('timestamp' = timestamps[timestamps < min_max_timestamps[1]],
                                 'od_reading' = od_readings[timestamps < min_max_timestamps[1]])


  # Go through all consecutive minimum to maximum to extract useful growth curves
  for (i in seq(2, length(min_max_timestamps), 2)){
    # Find time stamps for consecutive minimum and maximum
    min_timestamp <- min_max_timestamps[i]
    max_timestamp <- min_max_timestamps[i+1]

    # - - Check that maximum is not an outlier measurement
    if (!outlier_determination[timestamps == max_timestamp]){
      # - - - if not the above save the OD measurements associated with the minimum to maximum growth.
      growth_list[[length(growth_list)+1]] <- data.frame('timestamp' = timestamps[min_timestamp < timestamps & timestamps < max_timestamp],
                                                         'od_reading' = od_readings[min_timestamp < timestamps & timestamps < max_timestamp])


    }
  }
  return(growth_list)
}
peak_detection_spline_workflow <- function(wide_pioreactor_od_frame){
  od_columns <- 2:ncol(wide_pioreactor_od_frame)
  wide_pioreactor_od_frame <- spline_outlier_detection(wide_pioreactor_od_frame, od_columns)
  wide_pioreactor_od_frame <- iqr_outlier_masking(wide_pioreactor_od_frame, od_columns)

  # For each reactor, identify maximums and minimums in OD data, and add a column to hold this information
  for (i in od_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[i], '\\.'))[2]
    print(paste("reactor_name:", reactor_name))
    od_values <- wide_pioreactor_od_frame[!wide_pioreactor_od_frame[i+length(od_columns)],i]
    time_stamps <- wide_pioreactor_od_frame[!wide_pioreactor_od_frame[i+length(od_columns)],1]
    time_stamps <- time_stamps[!is.na(od_values)]
    od_values <- od_values[!is.na(od_values)]

    # Find a fine grained spline that can be used to identify consecutive negative values
    spline_smooted_od <- fine_spline_smoothing(hours = wide_pioreactor_od_frame[,1],
                                               od_values = wide_pioreactor_od_frame[,i],
                                               outliers = wide_pioreactor_od_frame[,i+length(od_columns)])

    consecutive_neg_timestamps <- time_stamps[!is.na(spline_smooted_od)]
    spline_smooted_od <- spline_smooted_od[!is.na(spline_smooted_od)]

    ## Remove consecutive decreasing OD readings (using the rolling mean)
    dist_to_prev_od <- c(0, spline_smooted_od[2:length(spline_smooted_od)] - spline_smooted_od[1:(length(spline_smooted_od)-1)])
    cumcount_decrease_od <- count_consecutive_negatives(dist_to_prev_od, consecutive_neg_timestamps)
    # Binarise consecutive OD readings and add to wide_od_frame
    consecutive_neg_od_column_name <- paste0(reactor_name, 'con_neg_od', collapse = '_')
    wide_pioreactor_od_frame[,consecutive_neg_od_column_name] <- FALSE
    wide_pioreactor_od_frame[wide_pioreactor_od_frame$hours %in% cumcount_decrease_od$hours, consecutive_neg_od_column_name] <- cumcount_decrease_od$output > 4 # four was the previous

    # Copy the peak detect frame to insert the rolling od
    wide_pioreactor_od_frame_rolling_copy <- wide_pioreactor_od_frame[,c(1, i)]
    # Set outliers and consecutive negative difference measurements to NA
    wide_pioreactor_od_frame_rolling_copy[wide_pioreactor_od_frame[i+length(od_columns)] | wide_pioreactor_od_frame[,consecutive_neg_od_column_name],2] <- NA

    # Filter outliers from spline values and time stamps
    spline_smooted_od <- spline_smooted_od[consecutive_neg_timestamps %in% wide_pioreactor_od_frame_rolling_copy$hours[!(is.na(wide_pioreactor_od_frame_rolling_copy[,2]))]]
    time_stamps <- consecutive_neg_timestamps[consecutive_neg_timestamps %in% wide_pioreactor_od_frame_rolling_copy$hours[!(is.na(wide_pioreactor_od_frame_rolling_copy[,2]))]]
    
    index_for_exp_growth_end <- end_of_initial_exp_growth(od_values = spline_smooted_od)
    dense_od_range <- density_estimate_function(od_values = spline_smooted_od, index_for_exp_growth_end = index_for_exp_growth_end)
    time_delta <- time_density_estimate(time_stamps[!is.na(spline_smooted_od)], spline_smooted_od[!is.na(spline_smooted_od)])

    delta <- dense_od_range * 0.6
    print(paste("delta:", delta))
    
    spline_detected_segments <- find_increasing_segments_reset_base(measurements = spline_smooted_od,
                                                         time_stamps = time_stamps,
                                                         delta = delta, 
                                                         time_delta = time_delta,
                                                         min_segment_length = 40,
                                                         index_for_exp_growth_end)

    segments_column_name <- paste0(reactor_name, 'segments', collapse = '_')
    wide_pioreactor_od_frame[,segments_column_name] <- NA
    
    wide_pioreactor_od_frame[wide_pioreactor_od_frame$hours %in% spline_detected_segments$time_stamps, segments_column_name] <- spline_detected_segments$merged_segments
  }


  # Sort output frame
  wide_pioreactor_od_frame <- wide_pioreactor_od_frame[,c(1,
                                                          od_columns,
                                                          grep("outliers", colnames(wide_pioreactor_od_frame)),
                                                          grep("segments", colnames(wide_pioreactor_od_frame)),
                                                          grep("con_neg_od", colnames(wide_pioreactor_od_frame)))]
  return(wide_pioreactor_od_frame)
}
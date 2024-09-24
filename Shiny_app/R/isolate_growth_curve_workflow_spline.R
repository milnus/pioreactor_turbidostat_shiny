isolate_growth_curve_workflow_spline <- function(wide_pioreactor_od_frame){
  return_growth_list <- list()
  
  od_data_columns <- 2:(2+(((ncol(wide_pioreactor_od_frame)-1) %/% 4)-1))
  
  for (i in od_data_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[i], '\\.'))[2]
    isolated_growth_curves <- isolate_growth_curves_spline(timestamps = wide_pioreactor_od_frame[,1],
                                                    od_readings = wide_pioreactor_od_frame[,i],
                                                    spline_segment_id = wide_pioreactor_od_frame[,i+length(od_data_columns)*2],
                                                    outlier_determination = wide_pioreactor_od_frame[,i+length(od_data_columns)])
    
    # Remove incomplete measurements/NAs
    isolated_growth_curves <- lapply(isolated_growth_curves, function(x) x[complete.cases(x),])
    
    # Remove empty exponential growth series
    max_segment_number <- length(isolated_growth_curves)

    empty_growth <- c()
    for (j in 1:max_segment_number){
      if(nrow(isolated_growth_curves[[j]]) < 10){
        empty_growth <- c(empty_growth, -j)
      }
    }
    if (!is.null(empty_growth)){
      isolated_growth_curves
      isolated_growth_curves <- isolated_growth_curves[empty_growth]
    }
    
    return_growth_list[[reactor_name]] <- isolated_growth_curves
  }
  
  return(return_growth_list)
}
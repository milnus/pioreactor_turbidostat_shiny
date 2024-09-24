# Run through each pioreactor in dataset to isolate growth curves
isolate_growth_curve_workflow <- function(wide_pioreactor_od_frame){
  print("[START] isolate_growth_curve_workflow")
  return_growth_list <- list()

  od_data_columns <- 2:(2+(((ncol(wide_pioreactor_od_frame)-1) %/% 4)-1))

  for (i in od_data_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[i], '\\.'))[2]
    isolated_growth_curves <- isolate_growth_curves(timestamps = wide_pioreactor_od_frame[,1],
                                                    od_readings = wide_pioreactor_od_frame[,i],
                                                    min_max_indication = wide_pioreactor_od_frame[,i+length(od_data_columns)*2],
                                                    outlier_determination = wide_pioreactor_od_frame[,i+length(od_data_columns)])

    # Remove empty or low measurement exponential growth series
    empty_growth <- c()
    for (i in 1:length(isolated_growth_curves)){
      if(nrow(isolated_growth_curves[[i]]) < 5){
        empty_growth <- c(empty_growth, -i)
      }
    }
    if (!is.null(empty_growth)){
      isolated_growth_curves <- isolated_growth_curves[empty_growth]
    }
    return_growth_list[[reactor_name]] <- isolated_growth_curves

  }
  return(return_growth_list)
}
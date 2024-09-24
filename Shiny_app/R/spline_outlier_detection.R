spline_outlier_detection <- function(wide_pioreactor_od_frame, od_columns){
  # Function to use spline and residual statistics to identify outliers
  # For each reactor identify outliers and add a column indicating these
  for (column_i in od_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[column_i], '\\.'))[2]
    outlier_column_name <- paste0(reactor_name, '.outliers')
    wide_pioreactor_od_frame[,outlier_column_name] <- FALSE
    
    # condense column for NA's
    data_oi <- wide_pioreactor_od_frame[,c(1,column_i)]
    data_oi <- data_oi[!is.na(data_oi[,2]),]
    
    # Construct a spline for the od values
    spline_regression <- lm(data_oi[,2] ~ ns(data_oi[,1], df = 200))
    
    spline_od <- predict(spline_regression, data_oi[,1])
    
    spline_residuals <- data_oi[,2] - spline_od
    mean_spline_od <- mean(spline_residuals)
    sd_spline_od <- sd(spline_residuals)
    
    # Find outliers
    # plot(data_oi[,1], data_oi[,2], pch = ifelse(abs(spline_residuals) > mean_spline_od + 1.96 * sd_spline_od, 17, 20))
    # lines(data_oi[,1], spline_od, col = "red", lwd = 2)
    
    for (row_i in 1:(nrow(data_oi))){
      if (abs(spline_od[row_i] - data_oi[row_i, 2]) > mean_spline_od + 1.96 * sd_spline_od){
        wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] == data_oi[row_i,1], column_i] <- NA
        wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] == data_oi[row_i,1],
                                 outlier_column_name] <- TRUE
      }
    }
  }
  return(wide_pioreactor_od_frame)
}
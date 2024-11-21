# Detect outlisers based on a Inner Quartile Range outlier detection mechanism
iqr_outlier_masking <- function(wide_pioreactor_od_frame, od_columns){
  # Set the width of the window to search and find center of the window
  k <- 31
  k_center <- k %/% 2 +1
  
  # Check that K is odd, so that it has a center point.
  if (k %% 2 == 0){
    print('k cannot be even!')
    return (NULL)
  }
  
  # For each reactor identify outliers and add a column indicating these
  for (column_i in od_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[column_i], '\\.'))[2]
    
    # condence column for NA's
    data_oi <- wide_pioreactor_od_frame[,c(1,column_i)]
    data_oi <- data_oi[!is.na(data_oi[,2]),]
    
    # add roll to find median and IQR
    median_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = median, partial = T)
    iqr_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = IQR, partial = T)
    
    diff_col <- rep("black", nrow(data_oi))
    for (row_i in 1:(nrow(data_oi))){
      if (data_oi[row_i, 2] > median_roll[row_i] + iqr_roll[row_i] * 2){
        wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] == data_oi[row_i,1], column_i] <- NA
        diff_col[row_i] <- "red"
      }
    }
  }
  return(wide_pioreactor_od_frame)
}
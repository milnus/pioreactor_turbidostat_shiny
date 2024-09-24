# Read in the pioreactor od data
raw_pio_od_data_to_wide_frame <- function(od_readings_csv, reactors_oi = NULL){
  if(is.null(od_readings_csv)){return()}
  
  pioreactor_OD_data <- data.table::fread(od_readings_csv)
  
  # Convert time to hours
  pioreactor_OD_data$timestamp <- as.POSIXct(pioreactor_OD_data$timestamp)
  pioreactor_OD_data$hours <- difftime(pioreactor_OD_data$timestamp, min(pioreactor_OD_data$timestamp), units = 'hours')
  # pioreactor_OD_data$hours <- difftime(pioreactor_OD_data$timestamp_localtime, min(pioreactor_OD_data$timestamp_localtime), units = 'hours')
  
  # Set reactors that are of interest
  if (is.null(reactors_oi)){
    reactors_of_interest <- unique(pioreactor_OD_data$pioreactor_unit)
  }
  
  # Reshape the data into a wide format
  pioreactor_OD_data_wide <- as.data.frame(reshape(data = pioreactor_OD_data[pioreactor_OD_data$pioreactor_unit %in% reactors_of_interest,c('hours', 'pioreactor_unit', 'od_reading')],
                                                   idvar = 'hours', timevar = 'pioreactor_unit', direction = 'wide'))
  
  return(pioreactor_OD_data_wide)
}

# Filter out pioreactors based on user feedback
filter_reactors <- function(pioreactor_data, pios_of_interest = c(), filt_strat){
  # Check if filtering is required
  if (!is.null(pios_of_interest)){
    # Keep reactors selected by user:
    if (filt_strat == "Keep"){
      grep_str <- paste0(pios_of_interest, collapse = '|')
      pioreactor_data <- pioreactor_data[,c(1, grep(grep_str, names(pioreactor_data), ignore.case = T))]
    } else {
        grep_str <- paste0(pios_of_interest, collapse = '|')
        pioreactor_data <- pioreactor_data[,c(-grep(grep_str, names(pioreactor_data), ignore.case = T))]
      }
  }
  
  return(pioreactor_data)
}
# Plot data of which points were used for OD measurements
plot_utilised_data <- function(wide_pioreactor_od_frame, indivudial_growth_curves){
  print("[START] plot_utilised_data")
  
  reactor_names <- names(indivudial_growth_curves)
  # column_names <- sapply(reactor_names, function(x) paste0(x, '.used'))
  # wide_pioreactor_od_frame[,column_names] <- NA
  
  for (reactor in reactor_names){
    # Add the column to indicate use of measurements
    wide_pioreactor_od_frame[,paste0(reactor, '.used')] <- factor(x = NA, levels = 1:length(indivudial_growth_curves[[reactor]]))
    for (growth_i in 1:length(indivudial_growth_curves[[reactor]])){
      
      wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] %in% indivudial_growth_curves[[reactor]][[growth_i]][,1] , ncol(wide_pioreactor_od_frame)] <- growth_i
    }
  }
  
  od_columns <- (ncol(wide_pioreactor_od_frame)-1) / 5
  od_colnames <- names(wide_pioreactor_od_frame)
  plot_list <- list()
  for(i in 2:(od_columns+1)){
    plot_data <- data.frame('hours' = wide_pioreactor_od_frame[,1],
                            'od' = wide_pioreactor_od_frame[,i],
                            'used' = wide_pioreactor_od_frame[,i+4*od_columns])
    
    growths <- max(as.numeric(levels(plot_data$used)), na.rm = T)
    
    plot_list[[od_colnames[i]]] <- ggplot(plot_data) +
      geom_point(data = plot_data[is.na(plot_data$used),], aes(hours, od), colour = 'lightgrey') +
      geom_point(data = plot_data[!is.na(plot_data$used),], aes(hours, od, colour = used)) +
      scale_colour_manual(values = sample(rainbow(growths), 
                                          size = growths, 
                                          replace = F)) +
      theme(legend.position = 'none') +
      ggtitle(str_remove(names(wide_pioreactor_od_frame)[i], 'od_reading\\.'))
  }
  
  do.call(gridExtra::grid.arrange, c(plot_list))
  
  return(wide_pioreactor_od_frame)
}
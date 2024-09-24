# Extract mu and timepoints for bootstap resamplings.
extract_mu_bootstraps <- function(growth_list){
  growth_data <- lapply(1:length(growth_list), function(x) growth_list[[x]]$growth_data)
  growth_data <- do.call('rbind', growth_data)
  
  growth_data[,'reactor'] <- unlist(sapply(1:length(growth_list), function(x) rep(names(growth_list)[x], nrow(growth_list[[x]]$growth_data))))
  
  return(growth_data)
}

# Plot summarised growth data
plot_growth_rates <- function(summarised_data, reactor_grouping){
  if ((max(summarised_data$time_point, na.rm = T)) > 15){
    break_step <- 4
  } else {
    break_step <- 1
  }
  
  ## Create grouping column
  summarised_data$grouping <- summarised_data$reactor
  summarised_data$reactor_shape <- NA
  # Assign user defined groups
  if (nchar(reactor_grouping)){
    reactor_groups <- unlist(strsplit(reactor_grouping, split = ';'))
    reactor_groups <- sapply(reactor_groups, function(x) strsplit(x, split = ','), simplify = F)
    reactor_groups <- lapply(reactor_groups, function(x) paste0(unlist(x), collapse = '|'))
    
    for (group in reactor_groups){
      summarised_data$grouping[grepl(group, summarised_data$reactor, ignore.case = T)] <- str_replace_all(group, "\\|", ' - ')
      
      summarised_data$reactor_shape[grepl(group, summarised_data$reactor, ignore.case = T)] <- summarised_data$reactor[grepl(group, summarised_data$reactor, ignore.case = T)]
    }
  }
  
  # Set max time to x axis
  max_time <- max(summarised_data$time_point, na.rm = T)
  
  p <- ggplot(summarised_data, aes(time_point, mu)) +
    theme_prism() +
    stat_summary(mapping = aes(group = reactor, shape = reactor_shape),
                 fun = "mean", 
                 fun.min = function(z) { quantile(z,0.025) }, 
                 fun.max = function(z) { quantile(z,0.975) }, 
                 colour = "red", size = 0.5) +
    scale_shape_manual(values = c(0:14)[c(-2, -4, -9)], na.value = 1) +
    geom_smooth(mapping = aes(group = reactor),
                method = 'loess', se = F, span = 1) +
    facet_wrap(.~grouping) +
    scale_x_continuous(limits = c(0, max_time),
                       breaks = seq(0, max_time, break_step), labels = seq(0, max_time, break_step),
                       minor_breaks = seq(1, max_time, 1)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme(panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = 0.2),
          strip.text.x = element_text(hjust = 0, margin=margin(l=0))) +
    labs(x = 'Hours')
  
  
  return(p)
}
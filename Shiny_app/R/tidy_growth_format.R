# Conduct the growth modelling analysis
tidy_growth_format <- function(growth_data, bootstaps = 500){
  tidy_list_data <- list()
  growth_data_list <- list()
  for (description in 1:length(growth_data)){
    print(paste('Description is:', description))
    # Convert data to tidy format - The 2:(nrow(growth_data[[description]])-1) removes the peak and trough detected from the data
    tidy_list_data[[description]] <- data.frame("Time" = growth_data[[description]][2:(nrow(growth_data[[description]])-1),1],
                                                "Description" = 'Exponential_growth',
                                                'Replicate' = description,
                                                "Values" = growth_data[[description]][2:(nrow(growth_data[[description]])-1),2])
    
    tt_grodata <- read_data(tidy_list_data[[description]], data.format = 'col')

    tt_growth_rate <- growth.workflow(tt_grodata,
                                      fit.opt = c('s'), # Fit spline
                                      nboot.gc = bootstaps,
                                      ec50 = F,
                                      growth.thresh = 0.05, # *** Should this be increase or do we trust input data, as we have made peak detection and are running turbidostat?
                                      t0 = min(tt_grodata$time),
                                      smooth.gc = 1, suppress.messages = T)

    growth_data_list[[description]] <- table_group_growth_spline(tt_growth_rate$gcFit$gcTable)

    growth_data_list[[description]] <- data.frame('mu' = tt_growth_rate[[3]]$gcBootSplines[[1]]$mu ,
                                                  'time_point' = median(tt_growth_rate[[3]]$gcBootSplines[[1]]$raw.time),
                                                  'growth_phase' = description)

    # plot(tt_growth_rate$gcFit$gcBootSplines[[1]], combine = T, lwd = 0.2)
    
    # summarise the growth curve boot strapping
    # mean(tt_growth_rate[[3]]$gcBootSplines$`Exponential_growth | 1 | NA`$mu)
    # mu_observations <- tt_growth_rate[[3]]$gcBootSplines$`Exponential_growth | 1 | NA`$mu
    # mean(mu_observations[mu_observations > quantile(mu_observations, probs = 0.25) & mu_observations < quantile(mu_observations, probs = 0.75)])
    ### What do we want from here?! what data and fit do we want? ****
  }

  return_od_data <- do.call('rbind', tidy_list_data)

  return_growth_data <- do.call('rbind', growth_data_list)
  # return_growth_data$time_point <- factor(return_growth_data$time_point, levels = sort(unique(return_growth_data$time_point)))
  return_growth_data$time_point <- as.numeric(return_growth_data$time_point)

  return_list <- list('exponential_od_data' = return_od_data, 'growth_data' = return_growth_data)


  return(return_list)
}
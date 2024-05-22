# Read in the pioreactor od data
raw_pio_od_data_to_wide_frame <- function(od_readings_csv, reactors_oi = NULL){
  if(is.null(od_readings_csv)){return()}
  
  pioreactor_OD_data <- data.table::fread(od_readings_csv)
  
  # Convert time to hours
  pioreactor_OD_data$hours <- difftime(pioreactor_OD_data$timestamp_localtime, min(pioreactor_OD_data$timestamp_localtime), units = 'hours')
  
  # Set reactors that are of interest
  if (is.null(reactors_oi)){
    reactors_of_interest <- unique(pioreactor_OD_data$pioreactor_unit)
  }
  
  # Reshape the data into a wide format
  pioreactor_OD_data_wide <- as.data.frame(reshape(data = pioreactor_OD_data[pioreactor_OD_data$pioreactor_unit %in% reactors_of_interest,c('hours', 'pioreactor_unit', 'od_reading')],
                                                   idvar = 'hours', timevar = 'pioreactor_unit', direction = 'wide'))
  
  return(pioreactor_OD_data_wide)
}

# Function to allow user to select pioreactors of interest
user_pio_selection <- function(read_data, reactor_selection){
  if(is.null(read_data)){return()}
  
  selectInput(
    inputId = "reactor_selection",
    label = "Select Pioreactors to remove or keep based on option above\n(red plots will be removed and green will be keep in further analyses)",
    choices = str_replace(names(read_data), pattern = 'od_reading\\.', '')[-1],
    selected = reactor_selection,
    multiple = TRUE)
}

# Function for plotting all raw od readings from reactors
plot_raw_data <- function(raw_data, filter_vector, filt_strat){
  if(is.null(raw_data)){return()}
  
  print(paste("filter_vector plot_raw_data:", filter_vector))
  df_list <- apply(raw_data[,2:ncol(raw_data)], 2, function(x) data.frame('timestamp' = raw_data[,1], 'od_reading' = x))
  
  ind_plots <- sapply(1:length(df_list), function(i) plot_dataframe_raw(df_list[[i]], names(df_list)[i], filter_vector, filt_strat), simplify = F)
  do.call(gridExtra::grid.arrange, c(ind_plots))
}

# Plotting each raw od curve
plot_dataframe_raw <- function(dataframe, name, filter_vector, filt_strat) {
  if(is.null(dataframe)){return()}

  # Isolate reactor name for plot title
  plot_title <- unlist(strsplit(name, '\\.'))[2]
  
  # Check if plot is filtered
  if (any(grepl(plot_title, filter_vector, ignore.case = T))){
    if (filt_strat == 'Keep'){
      background_col <- 'green'
    } else { background_col <- "red" }
  } else { background_col <- "lightgrey" }
  
  dataframe <- dataframe[!is.na(dataframe[,2]),]
  
  p <- ggplot(dataframe, aes(x = timestamp, y = od_reading)) +
    geom_point() +
    ggtitle(plot_title) +
    theme(axis.title = element_blank(),
          panel.background = element_rect(fill = background_col, color = background_col))
  
  return(p)
}

# Filter out pioreactors based on user feedback
filter_reactors <- function(pioreactor_data, pios_of_interest = c(), filt_strat){
  # Check if filtering is required
  if (length(pios_of_interest)){
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

# Detect outlisers based on a Inner Quartile Range outlier detection mechanism
iqr_outlier_detection <- function(wide_pioreactor_od_frame, od_columns){
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
    outlier_column_name <- paste0(reactor_name, '.outliers')
    wide_pioreactor_od_frame[,outlier_column_name] <- FALSE

    # condence column for NA's
    data_oi <- wide_pioreactor_od_frame[,c(1,column_i)]
    data_oi <- data_oi[!is.na(data_oi[,2]),]

    # add roll to find median and IQR
    # median_roll <- zoo::rollmedian(data_oi[,2], align = 'center', k=k, na.pad = T, partial = T)
    median_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = median, partial = T)
    iqr_roll <- zoo::rollapply(data_oi[,2], align = 'center', fill=NA, width=k, FUN = IQR, partial = T)

    for (row_i in 1:(nrow(data_oi))){
      if (data_oi[row_i, 2] > median_roll[row_i] + iqr_roll[row_i] * 2.5){
        wide_pioreactor_od_frame[row_i, column_i] <- NA
        wide_pioreactor_od_frame[wide_pioreactor_od_frame[,1] == data_oi[row_i,1],
                                 outlier_column_name] <- TRUE
      }
    }
  }
  return(wide_pioreactor_od_frame)
}

# maximum and minimum detection algorithm from: https://gist.github.com/dgromer/ea5929435b8b8c728193
# Algorithm adapted from this blogpost: https://billauer.co.il/blog/2009/01/peakdet-matlab-octave/
peakdet <- function(v, delta, x = NULL)
{
  maxtab <- NULL
  mintab <- NULL

  if (is.null(x))
  {
    x <- seq_along(v)
  }

  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have the same length")
  }

  if (!is.numeric(delta))
  {
    stop("Input argument delta must be numeric")
  }

  if (delta <= 0)
  {
    stop("Input argument delta must be positive")
  }

  mn <- Inf
  mx <- -Inf

  mnpos <- NA
  mxpos <- NA

  lookformax <- TRUE

  for(i in seq_along(v))
  {
    this <- v[i]

    if (!is.na(this) & this > mx)
    {
      mx <- this
      mxpos <- x[i]
    }

    if (!is.na(this) & this < mn)
    {
      mn <- this
      mnpos <- x[i]
    }

    if (lookformax)
    {
      if (!is.na(this) & this < mx - delta)
      {
        maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))

        mn <- this
        mnpos <- x[i]

        lookformax <- FALSE
      }
    }
    else
    {
      if (!is.na(this) & this > mn + delta)
      {
        mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))

        mx <- this
        mxpos <- x[i]

        lookformax <- TRUE
      }
    }
  }

  list(maxtab = maxtab, mintab = mintab)
}

peak_detection_workflow <- function(wide_pioreactor_od_frame){
  od_columns <- 2:ncol(wide_pioreactor_od_frame)

  wide_pioreactor_od_frame <- iqr_outlier_detection(wide_pioreactor_od_frame, od_columns)

  # For each reactor, identify maximums and minimums in OD data, and add a column to hold this information
  for (i in od_columns){
    reactor_name <- unlist(strsplit(names(wide_pioreactor_od_frame)[i], '\\.'))[2]

    peaks <- peakdet(wide_pioreactor_od_frame[!wide_pioreactor_od_frame[i+length(od_columns)],i], .25, wide_pioreactor_od_frame$hours[!wide_pioreactor_od_frame[,i+length(od_columns)]])

    peak_column_name <- paste0(reactor_name, 'peaks', collapse = '_')

    wide_pioreactor_od_frame[,peak_column_name] <- NA


    wide_pioreactor_od_frame[wide_pioreactor_od_frame$hours %in% peaks$mintab$pos, peak_column_name] <- 'Minimum'
    wide_pioreactor_od_frame[wide_pioreactor_od_frame$hours %in% peaks$maxtab$pos, peak_column_name] <- 'Maximum'
  }

  return(wide_pioreactor_od_frame)
}

# Use the peak detection to identify consecutive turbidostat growths that are not a truncated due to an outlier measurement.
isolate_growth_curves <- function(timestamps, od_readings, min_max_indication, outlier_determination){
  # Set outlier measurements to NA
  od_readings[outlier_determination] <- NA
  # Isolate rows where there are OD readings
  timestamps <- timestamps[!is.na(od_readings)]
  min_max_indication <- min_max_indication[!is.na(od_readings)]
  outlier_determination <- outlier_determination[!is.na(od_readings)]
  od_readings <- od_readings[!is.na(od_readings)]

  # Find time points for min and max
  min_max_timestamps <- timestamps[!is.na(min_max_indication)]

  if (length(min_max_timestamps) %% 2 == 0){
    # add in the last stretch of growth to keep the final exponential growth
    min_max_timestamps <- c(min_max_timestamps, max(timestamps))

    # # For now we remove and ignore the last point is we end on a minimum (even min_max_timestamps)
    # min_max_timestamps <- min_max_timestamps[1:(length(min_max_timestamps)-1)]
  }

  # Construct list to hold individual growths
  growth_list <- list()

  # Save the first part of the growth curve (lag-phase into initial exponential phase)
  growth_list[[1]] <- data.frame('timestamp' = timestamps[timestamps < min_max_timestamps[1]],
                                 'od_reading' = od_readings[timestamps < min_max_timestamps[1]])


  # Go through all consecutive minimum to maximum to extract useful growth curves
  for (i in seq(2, length(min_max_timestamps), 2)){
    # Find time stamps for consecutive minimum and maximum
    min_timestamp <- min_max_timestamps[i]
    max_timestamp <- min_max_timestamps[i+1]

    # - - Check that maximum is not an outlier measurement
    if (!outlier_determination[timestamps == max_timestamp]){
      # - - - if not the above save the OD measurements associated with the minimum to maximum growth.
      growth_list[[length(growth_list)+1]] <- data.frame('timestamp' = timestamps[min_timestamp < timestamps & timestamps < max_timestamp],
                                                         'od_reading' = od_readings[min_timestamp < timestamps & timestamps < max_timestamp])


    }
  }
  return(growth_list)
}

# Run through each pioreactor in dataset to isolate growth curves
isolate_growth_curve_workflow <- function(wide_pioreactor_od_frame){
  return_growth_list <- list()

  od_data_columns <- 2:(2+(((ncol(wide_pioreactor_od_frame)-1) %/% 3)-1))

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

# Plot all growth rate curves
plot_growth_rates_summary_data <- function(summarised_data){
  if ((min(summarised_data$time_point) - max(summarised_data$time_point)) > 24){
    break_step <- 2
  } else {
    break_step <- 1
  }
  
  p <- ggplot(summarised_data, aes(time_point, mean_mu, colour = reactor)) +
    geom_smooth(method = 'loess', se = F, span = 1.0) +
    geom_linerange(aes(ymax = mean_mu + sd_mu, ymin = mean_mu - sd_mu)) +
    geom_point() +
    # geom_line() +
    ggprism::theme_prism() +
    scale_x_continuous(limits = c(0, max(summarised_data$time_point)),
                       breaks = seq(0, max(summarised_data$time_point), break_step), labels = seq(0, max(summarised_data$time_point), break_step))
  
  return(p)
}

# Function for plotting all individual growth curves
plot_ind_growths <- function(growth_curves){
  tt_plot_frame <- lapply(growth_curves[[1]], plot_dataframe)
  do.call(gridExtra::grid.arrange, c(tt_plot_frame))
}

# make function that plots each sub-plot of exponential growth
plot_dataframe <- function(dataframe) {
  ggplot(dataframe, aes(x = timestamp, y = od_reading)) +
    geom_point()
}

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
                                      smooth.gc = 1)

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

# Summarise mean and intervals for growth
summarise_growth_list <- function(growth_list, plot_data = F){
  return_growth_list <- list()
  # Go through each list of reactors
  for (reactor in (1:length(growth_list))){
    # Calculate statistics for growth curves
    mu_mean <- aggregate.data.frame(growth_list[[reactor]]$growth_data$mu, by = list(growth_list[[reactor]]$growth_data$growth_phase), mean)
    mu_median <- aggregate.data.frame(growth_list[[reactor]]$growth_data$mu, by = list(growth_list[[reactor]]$growth_data$growth_phase), median)
    mu_lower_bound <- aggregate.data.frame(growth_list[[reactor]]$growth_data$mu, by = list(growth_list[[reactor]]$growth_data$growth_phase), function(x) quantile(x, 0.975, na.rm=T)) #*** Use the bootstarp confidence intervall instead of sd
    mu_upper_bound <- aggregate.data.frame(growth_list[[reactor]]$growth_data$mu, by = list(growth_list[[reactor]]$growth_data$growth_phase), function(x) quantile(x, 0.025, na.rm=T)) #*** Use the bootstarp confidence intervall instead of sd
    mu_sd <- aggregate.data.frame(growth_list[[reactor]]$growth_data$mu, by = list(growth_list[[reactor]]$growth_data$growth_phase), sd) #*** Use the bootstarp confidence intervall instead of sd
    
    # Summarise data into table
    growth_df <- data.frame('time_point' = unique(growth_list[[reactor]]$growth_data$time_point),
                            'mean_mu' = mu_mean[,2],
                            'median_mu' = mu_median[,2],
                            'sd_mu' = mu_sd[,2],
                            'boot_low_bound' = mu_lower_bound[,2],
                            'boot_upper_bound' = mu_upper_bound[,2])
    
    return_growth_list[[names(growth_list)[reactor]]] <- growth_df
  }

  bound_list <- do.call('rbind', return_growth_list)
  bound_list$reactor <- unlist(sapply(1:length(return_growth_list), function(x) rep(names(return_growth_list)[x], nrow(return_growth_list[[x]]))))

  if (plot_data == T){
    ggplot(bound_list, aes(time_point, mean_mu, group = reactor)) +
      geom_line() +
      facet_wrap(.~reactor)
  }
  bound_list <- bound_list[complete.cases(bound_list),]
  return(bound_list)
}

# Extract mu and timepoints for bootstap resamplings.
extract_mu_bootstraps <- function(growth_list){
  growth_data <- lapply(1:length(growth_list), function(x) growth_list[[x]]$growth_data)
  growth_data <- do.call('rbind', growth_data)
  
  growth_data[,'reactor'] <- unlist(sapply(1:length(growth_list), function(x) rep(names(growth_list)[x], nrow(growth_list[[x]]$growth_data))))
  
  return(growth_data)
}

plot_growth_rates <- function(summarised_data){
  
  if ((max(summarised_data$time_point, na.rm = T)) > 15){
    break_step <- 2
  } else {
    break_step <- 1
  }
  
  # Set max time to x axis
  max_time <- max(summarised_data$time_point, na.rm = T)
  
  p <- ggplot(summarised_data, aes(time_point, mu)) +
    # geom_jitter(width = 0.25)+
    theme_prism() +
    stat_summary(fun = "mean", 
                 fun.min = function(z) { quantile(z,0.025) }, 
                 fun.max = function(z) { quantile(z,0.975) }, 
                 colour = "red", size = 0.8) +
    geom_smooth(method = 'loess', se = F, span = 1) +
    facet_wrap(.~reactor) +
    scale_x_continuous(limits = c(0, max_time),
                       breaks = seq(0, max_time, break_step), labels = seq(0, max_time, break_step)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme(panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = 0.2))
  
  
  return(p)
}

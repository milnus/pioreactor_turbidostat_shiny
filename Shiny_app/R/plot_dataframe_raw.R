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
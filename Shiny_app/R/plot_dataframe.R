# make function that plots each sub-plot of exponential growth
plot_dataframe <- function(dataframe) {
  ggplot(dataframe, aes(x = timestamp, y = od_reading)) +
    geom_point()
}
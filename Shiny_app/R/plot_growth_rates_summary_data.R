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
# Function for plotting all individual growth curves
plot_ind_growths <- function(growth_curves){
  tt_plot_frame <- lapply(growth_curves[[1]], plot_dataframe)
  do.call(gridExtra::grid.arrange, c(tt_plot_frame))
}
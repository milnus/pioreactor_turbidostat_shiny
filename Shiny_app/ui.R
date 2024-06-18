library(shiny)
library(ggplot2)
library(ggprism)
library(QurvE)

dataset <- data.frame(x= 1:10, y=LETTERS[1:10])

fluidPage(
  
  titlePanel("Turbidostat growth Explorer"),
  
  sidebarPanel(
    fileInput('upload', label = 'PioReactor OD table'),
    radioButtons('filt_strat', label = "Filtering strategy", choices = c('Remove', 'Keep')),
    uiOutput("UserFilters"),
    textInput("reactor_groups", "Reactor Group - Members of a ground is comma seperated and groups are semicolon seperated (example P01,P02,P03;P04,P05,P06 is two groups if three reactors)"),
    # Slider input for delta [default: 0.25]
    actionButton("process", "Process data"),
  ),
  
  mainPanel(
    plotOutput('raw_data_plot'),
    # verbatimTextOutput("reactor"),
    # verbatimTextOutput("reactor_groups"),
    downloadButton("download_growth_rate_plot", label = "Download Growth Rate Plot"),
    plotOutput('plot'),
    # Add tabbox displaying the output plots where raw and used data are visualised.
    # It should be noted that if there are too few utilised data then the delta can be lowered.
    # Make possible to download the plots.
    plotOutput('data_used_plot'),
    downloadButton("download_raw_table", label = "Download Raw Data"),
    downloadButton("download_table", label = "Download Summarised Data"),
    tableOutput("table"),
  )
)

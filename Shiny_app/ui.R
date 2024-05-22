library(shiny)
library(ggplot2)
library(ggprism)
library(QurvE)

dataset <- data.frame(x= 1:10, y=LETTERS[1:10])

fluidPage(
  
  titlePanel("Turbidostat growth Explorer"),
  
  sidebarPanel(
    
    fileInput('upload', label = 'PioReactor OD table'),
    # selectizeInput("reactor", 'Select reactors to process', c("All", names(dataset))),
    radioButtons('filt_strat', label = "Filtering strategy", choices = c('Remove', 'Keep')),
    uiOutput("UserFilters"),
    # actionButton("plot_raw", "Plot raw growth curves"),
    textInput("reactor_groups", "Reactor Group - Members of a ground is comma seperated and groups are semicolon seperated (example P01,P02,P03;P04,P05,P06 is two groups if three reactors)"),
    actionButton("process", "Process data"),
    
    # sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
    #             value=min(1000, nrow(dataset)), step=500, round=0),
    
  #   selectInput('x', 'X', names(dataset)),
  #   selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
  #   selectInput('color', 'Color', c('None', names(dataset))),
  #   
  #   checkboxInput('jitter', 'Jitter'),
  #   checkboxInput('smooth', 'Smooth'),
  #   
  #   selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
  #   selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  ),
  
  mainPanel(
    plotOutput('raw_data_plot'),
    # verbatimTextOutput("reactor"),
    # verbatimTextOutput("reactor_groups"),
    plotOutput('plot'),
    downloadButton("download_raw_table", label = "Download Raw Data"),
    downloadButton("download_table", label = "Download Summarised Data"),
    tableOutput("table"),
  )
)

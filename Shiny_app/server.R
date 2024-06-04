library(shiny)
library(ggplot2)

#### Define functions to run the pioreactor turbidostat analysis ####



#### Back-end server function ####
function(input, output) {
  
  output$reactor <- renderPrint(input$reactor)
  reactor_groups <- eventReactive(input$process, {
    input$reactor_groups
    })
  
  output$reactor_groups <- renderPrint(reactor_groups())
  
  # Event observer on the input of Pioreactor groupings
  observeEvent(input$process, {
    message(paste("Pioreactor grouping updated:", reactor_groups()))
  })
  
  observeEvent(input$process, {
    message(paste("Pioreactor input OD file:", input$upload[1,4]))
  })
  
  observeEvent(input$process, {
    message(paste("Pioreactor filtering:", input$reactor_selection))
  })
  
  #### Process the data for individual raw data plotting ####
  observeEvent(input$plot_raw, {
    message(paste("Pioreactor input OD file:", input$upload[1,4]))
  })
  
  read_data_raw <- reactive(raw_pio_od_data_to_wide_frame(input$upload[1,4]))
  
  output$UserFilters <- renderUI({
      user_pio_selection(read_data_raw(), input$reactor_selection)
  })
  
  output$raw_data_plot <- renderPlot(plot_raw_data(read_data_raw(), input$reactor_selection, input$filt_strat))
  
  
  
  #### Process the data for individual growth curve analysis ####
  ### *** TODO - Make this first a true false statement so that the process does not repeat more than once when pressing the bottom.
  read_data <- eventReactive(input$process, {
    # Read the path to the file uploaded by the user
    raw_pio_od_data_to_wide_frame(input$upload[1,4])
  })
  
  filtered_data <- reactive(filter_reactors(read_data(), input$reactor_selection, input$filt_strat))
  
  peak_detection <- reactive(peak_detection_workflow(filtered_data()))
   
  growth_curves <- reactive(isolate_growth_curve_workflow(peak_detection()))
   
  growth_curve_plot <- reactive(plot_ind_growths(growth_curves()))
   
  # tt <- reactive(renderTable(peak_detection))
   
  # output$plot <- renderPlot(growth_curve_plot())
   
  tidy_growth_result <- reactive(lapply(growth_curves(), tidy_growth_format, bootstaps=100))
   
  summarised_data <- reactive(summarise_growth_list(tidy_growth_result()))
   
  output$table <- renderTable(summarised_data())
   
  extracted_data <- reactive(extract_mu_bootstraps(tidy_growth_result()))
   
  #### Construct plot ####
  # Raw data to summarised
  output$plot <- renderPlot({
     plot_growth_rates(extracted_data(), input$reactor_groups)
   }, res = 96,)
   
   
  #### Allow download of data ####
  # Summarised data
  output$download_table <- downloadHandler(
    filename = function() {
      "Summaried_growth_rate_data.csv"
    },
    content = function(file) {
       write.table(summarised_data(), file, row.names = F, col.names = T, sep = ",")
     }
  )
   
  # Raw data values from bootstrap
  output$download_raw_table <- downloadHandler(
    filename = function() {
      "Raw_growth_rate_data_bootstrap_resamplings.csv"
    },
    content = function(file) {
       write.table(extracted_data(), file, row.names = F, col.names = T, sep = ",")
     }
  )
  
}
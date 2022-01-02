library(shiny)
#source("treatment.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output){

    # output$aff_file1 <- reactive({
    #   paste0(input$patient, "_right_wrist.csv")
    # })
    # output$aff_file2 <- reactive({
    #   paste0(input$patient, "_left_wrist.csv")
    # })
    # output$aff_filetime <- reactive({
    #   paste0(input$patient, "_time.csv")
    # })
    # 
    # output$dimimmo <- renderText({
    #   input$immobility_threshold
    # })
    # output$dimlab <- renderText({
    #   input$no_labelled_threshold
    # })
    # 
    # output$dimtree <- renderText({
    #   input$tree_number
    # })
    result = reactiveValues()
    source("treatment.R")
    observeEvent(input$patient, {
      result$res <- main_function(paste0(input$patient, "_right_wrist.csv"), paste0(input$patient, "_left_wrist.csv"), paste0(input$patient, "_time.csv"), input$immobility_threshold, input$no_labelled_threshold, input$tree_number)
    })
    observeEvent(input$immobility_threshold, {
      result$res <- main_function(paste0(input$patient, "_right_wrist.csv"), paste0(input$patient, "_left_wrist.csv"), paste0(input$patient, "_time.csv"), input$immobility_threshold, input$no_labelled_threshold, input$tree_number)
    })
    observeEvent(input$no_labelled_threshold, {
      result$res <- main_function(paste0(input$patient, "_right_wrist.csv"), paste0(input$patient, "_left_wrist.csv"), paste0(input$patient, "_time.csv"), input$immobility_threshold, input$no_labelled_threshold, input$tree_number)
    })
    observeEvent(input$tree_number, {
      result$res <- main_function(paste0(input$patient, "_right_wrist.csv"), paste0(input$patient, "_left_wrist.csv"), paste0(input$patient, "_time.csv"), input$immobility_threshold, input$no_labelled_threshold, input$tree_number)
    })
    
    output$resampling_plot <- renderPlot({
      result$res$resampling_plot
      })

    output$motion_plot <- renderPlot({
      result$res$motion_plot
      })

    output$lab_no_lab_plot <- renderPlot({
      result$res$lab_no_lab_plot
      })

    output$lab_plot <- renderPlot({
      result$res$lab_plot
      })

    output$obtained_result <- renderDataTable({
      result$res$obtained_result
      })

    output$MAE <- renderText({
      result$res$MAE
    })
})

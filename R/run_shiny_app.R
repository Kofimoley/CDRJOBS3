#' Shiny App for Viewing CDR Job Estimation Results
#'
#' Launches a Shiny app to interactively view CDR job estimation results.
#'
#' @param results A list of datasets to visualize.
#' @import shiny
#' @examples
#' run_shiny_app(results)
run_shiny_app <- function(results) {
  library(shiny)
  library(ggplot2)

  ui <- fluidPage(
    titlePanel("CDR Job Estimation Results Viewer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset:", choices = names(results)),
        selectInput("type", "Select Visualization Type:", choices = c("total_year", "cum_tech")),
        selectInput("job_metric", "Select Job Metric:", choices = c("mean_Jobs", "min_Jobs", "max_Jobs")),
        uiOutput("scenario_ui"),
        uiOutput("region_ui"),
        numericInput("ncol", "Number of Columns for Facets:", value = 2, min = 1),
        numericInput("nrow", "Number of Rows for Facets:", value = 2, min = 1)
      ),
      mainPanel(
        plotOutput("jobPlot")
      )
    )
  )

  server <- function(input, output, session) {
    selected_data <- reactive({
      results[[input$dataset]]
    })

    output$scenario_ui <- renderUI({
      selectInput("selected_scenarios", "Select Scenarios:",
                  choices = unique(selected_data()$scenario),
                  selected = unique(selected_data()$scenario),
                  multiple = TRUE)
    })

    output$region_ui <- renderUI({
      selectInput("selected_regions", "Select Regions:",
                  choices = unique(selected_data()$region),
                  selected = unique(selected_data()$region),
                  multiple = TRUE)
    })

    output$jobPlot <- renderPlot({
      filtered_data <- selected_data()
      if (!is.null(input$selected_scenarios)) {
        filtered_data <- filtered_data[filtered_data$scenario %in% input$selected_scenarios, ]
      }
      if (!is.null(input$selected_regions)) {
        filtered_data <- filtered_data[filtered_data$region %in% input$selected_regions, ]
      }

      visualize_results(
        data = filtered_data,
        type = input$type,
        job_metric = input$job_metric,
        selected_scenarios = input$selected_scenarios,
        selected_regions = input$selected_regions,
        ncol = input$ncol,
        nrow = input$nrow,
        output_path = tempdir()
      )
    })
  }

  shinyApp(ui = ui, server = server)
}

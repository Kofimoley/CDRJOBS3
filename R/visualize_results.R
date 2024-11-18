#' Visualize Results
#'
#' Creates faceted plots for CDR job estimation results.
#'
#' @param data The dataset to visualize.
#' @param type The type of visualization. One of "total_year" or "cum_tech".
#' @param job_metric The metric to visualize. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param selected_scenarios A vector of scenario names to include, or NULL for all.
#' @param selected_regions A vector of region names to include, or NULL for all.
#' @param selected_years A vector of years to include (for "total_year"), or NULL for all.
#' @param ncol Number of columns for the facets.
#' @param nrow Number of rows for the facets. If NULL, rows are determined dynamically by ggplot2.
#' @param output_path The directory where plots will be saved as PNG files.
#' @examples
#' visualize_results(data = results$Job_total_year, type = "total_year", job_metric = "mean_Jobs")
visualize_results <- function(data,
                              type = c("total_year", "cum_tech"),
                              job_metric = c("mean_Jobs", "min_Jobs", "max_Jobs"),
                              selected_scenarios = NULL,
                              selected_regions = NULL,
                              selected_years = NULL,
                              ncol = 2,
                              nrow = NULL,
                              output_path = getwd()) {
  type <- match.arg(type)
  job_metric <- match.arg(job_metric)

  # Validate output path
  if (!dir.exists(output_path)) {
    stop("The specified output_path does not exist.")
  }

  # Filter data based on user selections
  if (!is.null(selected_scenarios)) {
    data <- data[data$scenario %in% selected_scenarios, ]
  }
  if (!is.null(selected_regions)) {
    data <- data[data$region %in% selected_regions, ]
  }
  if (!is.null(selected_years) && "year" %in% colnames(data)) {
    data <- data[data$year %in% selected_years, ]
  }

  # Scale the job metrics to millions
  data[[job_metric]] <- data[[job_metric]] / 1e6

  # Ensure data is ordered by year for proper line plotting
  if (type == "total_year") {
    data <- data %>% arrange(scenario, region, year)
  }

  # Convert year to numeric to ensure proper ordering and continuity in the line plot
  if ("year" %in% colnames(data)) {
    data$year <- as.numeric(data$year)
  }

  # Define theme for all plots
  custom_theme <- theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", face = "bold", angle = 0, hjust = 0.4),
    axis.text.y = element_text(size = 8, color = "black", face = "bold"),
    axis.title.y = element_text(size = 8, color = "black", face = "bold"),
    plot.title = element_text(size = 10, color = "darkred", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "darkgreen", face = "italic"),
    plot.caption = element_text(size = 10, color = "purple", face = "italic"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

  # Plot based on type
  if (type == "cum_tech") {
    p <- ggplot(data, aes(x = technology, y = .data[[job_metric]])) +
      geom_bar(stat = "identity", fill = "lightblue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Cumulative Jobs by Technology",
           x = "Technology",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  } else if (type == "total_year") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]], group = interaction(scenario, region))) +
      geom_line(aes(group = interaction(scenario, region)), color = "blue", linewidth = 1.2) +
      geom_point(color = "blue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Total Jobs by Year",
           x = "Year",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  }

  # Save plot
  file_name <- paste0(type, "_facet_plot.png")
  ggsave(filename = file.path(output_path, file_name),
         plot = p, width = 10, height = 8)

  # Return plot for optional interactive use
  return(p)
}

#' Shiny App for Viewing CDR Job Estimation Results
#'
#' Launches a Shiny app to interactively view CDR job estimation results.
#'
#' @param results A list of datasets to visualize.
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

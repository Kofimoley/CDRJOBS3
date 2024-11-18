#' Visualize Results
#'
#' Creates plots for different types of CDR job estimation results.
#'
#' @param data The dataset to visualize.
#' @param type The type of visualization. One of "total_year" or "cum_tech".
#' @param job_metric The metric to visualize. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param selected_scenarios A vector of scenario names to include, or NULL for all.
#' @param selected_regions A vector of region names to include, or NULL for all.
#' @param selected_years A vector of years to include (for "total_year"), or NULL for all.
#' @param ncols The number of columns for sub-panels.
#' @param output_path The directory where plots will be saved. Plots are saved by default.
#' @examples
#' visualize_results(data = results$Job_total_year, type = "total_year", job_metric = "mean_Jobs")
visualize_results <- function(data,
                              type = c("total_year", "cum_tech"),
                              job_metric = c("mean_Jobs", "min_Jobs", "max_Jobs"),
                              selected_scenarios = NULL,
                              selected_regions = NULL,
                              selected_years = NULL,
                              ncols = 2,
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

  # Identify unique scenarios and regions
  scenarios <- unique(data$scenario)
  regions <- unique(data$region)

  # Determine number of rows for layout
  nrows <- ceiling(length(regions) / ncols)

  # Create a ggplot for each scenario
  for (scenario in scenarios) {
    scenario_data <- data[data$scenario == scenario, ]
    plots <- list() # Store individual region plots

    for (region in regions) {
      region_data <- scenario_data[scenario_data$region == region, ]
      if (nrow(region_data) > 0) {
        if (type == "cum_tech") {
          p <- ggplot(region_data, aes(x = technology, y = .data[[job_metric]])) +
            geom_bar(stat = "identity", fill = "lightblue") +
            labs(title = region, x = "Technology", y = job_metric) +
            custom_theme
        } else if (type == "total_year") {
          p <- ggplot(region_data, aes(x = year, y = .data[[job_metric]])) +
            geom_line(color = "blue") +
            geom_point(color = "blue") +
            labs(title = region, x = "Year", y = job_metric) +
            custom_theme
        }
        plots[[region]] <- p
      }
    }

    # Combine and save plots
    combined_plot <- gridExtra::grid.arrange(grobs = plots, ncol = ncols)
    ggsave(filename = file.path(output_path, paste0(scenario, "_", type, ".png")),
           plot = combined_plot, width = 10, height = 8)
  }
}

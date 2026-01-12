# ==============================================================================
# Main Report Generation Entry Point
# ==============================================================================

#' Generate complete OMOP delivery report
#'
#' Main entry point for report generation. Loads data, calculates metrics,
#' and generates an HTML report with embedded JavaScript and CSS.
#'
#' Workflow:
#'   1. Load and validate input data (CSV files)
#'   2. Calculate all metrics and scores
#'   3. Transform data for display
#'   4. Build HTML report from templates
#'   5. Write output file
#'
#' @param delivery_report_path Path to delivery_report.csv (local or gs:// URI)
#' @param dqd_results_path Path to dqd_results.csv (local or gs:// URI)
#' @param output_path Path for output HTML file (local or gs:// URI)
#' @return Invisible NULL (writes file as side effect)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Local files
#'   generate_omop_report(
#'     "data/delivery_report.csv",
#'     "data/dqd_results.csv",
#'     "reports/omop_report.html"
#'   )
#'
#'   # GCS paths
#'   generate_omop_report(
#'     "gs://my-bucket/delivery_report.csv",
#'     "gs://my-bucket/dqd_results.csv",
#'     "gs://my-bucket/report.html"
#'   )
#' }
generate_omop_report <- function(
  delivery_report_path,
  dqd_results_path,
  output_path
) {

  # Print header
  cat("\n")
  cat("============================================================\n")
  cat("OMOP DELIVERY REPORT GENERATOR\n")
  cat("============================================================\n\n")

  # ============================================================================
  # LOAD DATA
  # ============================================================================
  cat("Loading data files...\n")

  delivery_data <- load_data(delivery_report_path, .RAW_DELIVERY_REPORT_FILE)
  dqd_data <- load_data(dqd_results_path, .DQD_FILE)

  # Check data availability
  availability <- check_data_availability(delivery_data, dqd_data)
  has_delivery_data <- availability$has_delivery_data
  has_dqd_data <- availability$has_dqd_data

  cat("Delivery data available: ", has_delivery_data, "\n", sep = "")
  cat("DQD data available: ", has_dqd_data, "\n", sep = "")

  # Handle case where neither data source is available
  if (!has_delivery_data && !has_dqd_data) {
    stop("Both delivery report and DQD results are missing. Cannot generate report.")
  }

  # ============================================================================
  # PARSE AND CALCULATE METRICS
  # ============================================================================
  cat("Parsing delivery metrics...\n")

  # Parse delivery data (if available)
  metrics <- if (has_delivery_data) {
    # Parse metrics (type concept grouping now integrated!)
    parse_delivery_metrics(delivery_data)
  } else {
    # Create empty metrics structure
    create_empty_metrics()
  }

  # ============================================================================
  # CALCULATE DQD SCORES
  # ============================================================================
  cat("Calculating DQD scores...\n")

  # Get table groups and add "All Tables" group
  table_groups <- get_table_groups()
  all_tables <- unique(unlist(table_groups, use.names = FALSE))
  table_groups_with_all <- c(list("All Tables" = all_tables), table_groups)

  # Calculate DQD scores
  if (has_dqd_data) {
    dqd_scores <- list(
      overall = calculate_overall_dqd_score(dqd_data),
      grid = create_dqd_grid(dqd_data)
    )
    group_dqd_scores <- calculate_dqd_scores_by_group(dqd_data, table_groups_with_all)

    # Calculate DQD scores for ALL tables that have DQD data
    # Get unique table names from DQD data (convert to lowercase for consistency)
    tables_in_dqd <- unique(tolower(dqd_data$cdmTableName))
    table_dqd_scores <- calculate_dqd_scores_by_table(dqd_data, tables_in_dqd)
  } else {
    dqd_scores <- create_empty_dqd_scores()
    group_dqd_scores <- lapply(table_groups_with_all, function(x) NA_real_)
    table_dqd_scores <- list()
  }

  # ============================================================================
  # BUILD HTML REPORT
  # ============================================================================
  cat("Building HTML structure...\n")

  html <- build_complete_html_report(
    metrics = metrics,
    dqd_data = dqd_data,
    dqd_scores = dqd_scores,
    table_groups = table_groups_with_all,
    group_dqd_scores = group_dqd_scores,
    table_dqd_scores = table_dqd_scores,
    has_delivery_data = has_delivery_data,
    has_dqd_data = has_dqd_data
  )

  # ============================================================================
  # WRITE OUTPUT
  # ============================================================================
  cat("Writing HTML file...\n")

  write_file(html, output_path)

  # ============================================================================
  # DISPLAY SUMMARY
  # ============================================================================
  cat("\nReport generation complete!\n\n")

  if (has_delivery_data) {
    cat("Report Details:\n")
    cat("Site: ", metrics$metadata$site, "\n", sep = "")
    cat("Delivery Date: ", metrics$metadata$delivery_date, "\n", sep = "")
    if (has_dqd_data) {
      cat("Overall DQD Score: ", dqd_scores$overall, "%\n", sep = "")
    }
    cat("Tables Delivered: ", nrow(metrics$valid_tables), "\n", sep = "")
  }

  invisible(NULL)
}

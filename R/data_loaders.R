# ==============================================================================
# Data Loading and Validation
# ==============================================================================
# Loads delivery report and DQD data from CSV files and validates their schemas.

#' Load results CSV files
#'
#' Reads specified CSV file from local filesystem or GCS.
#' Returns NULL if file is missing (with warning).
#'
#' @param path Character path to file (local or gs:// URI)
#' @return Data frame with data, or NULL if file not found
#' @export
load_data <- function(path, results_file_type) {
  logger::log_info("Loading {results_file_type} from: {path}")

  # Load dataframe from either local or GCS file
  data <- read_csv(path)

  if (is.null(data)) {
    logger::log_warn("{results_file_type} not found")
    return(NULL)
  }

  logger::log_info("Loaded {results_file_type}: {nrow(data)} rows")

  # Validate schema
  validate_schema(data, results_file_type)

  return(data)
}

#' Validate results file schemas
#'
#' Checks that all required columns are present in specified file.
#'
#' @param data Data frame to validate
#' @param results_file_type Character containing the type of file to validate
#' @return Logical TRUE if valid (or stops with error)
validate_schema <- function(data, results_file_type) {
  if (results_file_type == .RAW_DELIVERY_REPORT_FILE) {
    required_columns <- .RAW_DELIVERY_REPORT_FILE_COLUMNS
  } else if (results_file_type == .DQD_FILE) {
    required_columns <- .DQD_FILE_COLUMNS
  } else if (results_file_type == .PASS_COMPOSITE_OVERALL_FILE) {
    required_columns <- .PASS_COMPOSITE_OVERALL_COLUMNS
  } else if (results_file_type == .PASS_COMPOSITE_COMPONENTS_FILE) {
    required_columns <- .PASS_COMPOSITE_COMPONENTS_COLUMNS
  } else {
    stop(glue::glue("Unknown results file type: {results_file_type}"))
  }

  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    error_msg <- glue::glue("{results_file_type} missing required columns: {paste(missing_columns, collapse = ', ')}")
    stop(error_msg)
  }

  return(TRUE)
}

#' Load PASS results from directory
#'
#' Loads PASS composite overall, components, and optionally table-level files
#' from a directory containing PASS output CSVs.
#'
#' @param pass_dir_path Path to directory containing PASS CSV files (local or gs:// URI)
#' @return List with overall, components, and table_scores data frames, or NULL if not found
#' @export
load_pass_results <- function(pass_dir_path) {
  if (is.null(pass_dir_path) || pass_dir_path == "") {
    logger::log_info("No PASS directory path provided")
    return(NULL)
  }

  logger::log_info("Loading PASS results from: {pass_dir_path}")

  # Ensure path ends with /
  if (!grepl("/$", pass_dir_path)) {
    pass_dir_path <- paste0(pass_dir_path, "/")
  }

  # Load composite overall file
  overall_path <- paste0(pass_dir_path, "pass_composite_overall.csv")
  overall_data <- load_data(overall_path, .PASS_COMPOSITE_OVERALL_FILE)

  if (is.null(overall_data)) {
    logger::log_warn("PASS composite overall file not found")
    return(NULL)
  }

  # Load composite components file
  components_path <- paste0(pass_dir_path, "pass_composite_components.csv")
  components_data <- load_data(components_path, .PASS_COMPOSITE_COMPONENTS_FILE)

  if (is.null(components_data)) {
    logger::log_warn("PASS composite components file not found")
    return(NULL)
  }

  # Extract weights from components data for table-level composite calculation
  metric_weights <- as.list(components_data$weight)
  names(metric_weights) <- components_data$metric

  # Load all 6 metric-level PASS overall files (contain CI bounds for each metric)
  metric_overall_data <- list()

  for (metric_name in names(.PASS_METRIC_OVERALL_FILES)) {
    filename <- .PASS_METRIC_OVERALL_FILES[[metric_name]]
    file_path <- paste0(pass_dir_path, filename)

    metric_data <- tryCatch({
      read_csv(file_path)
    }, error = function(e) {
      logger::log_info("PASS metric overall file not found: {filename} (optional)")
      NULL
    })

    if (!is.null(metric_data) && nrow(metric_data) > 0) {
      metric_overall_data[[metric_name]] <- metric_data[1, ]
      logger::log_info("Loaded {metric_name} overall data with CI bounds")
    }
  }

  # Load all 6 table-level PASS metric files
  table_level_metrics <- list()

  for (metric_name in names(.PASS_TABLE_LEVEL_FILES)) {
    file_info <- .PASS_TABLE_LEVEL_FILES[[metric_name]]
    file_path <- paste0(pass_dir_path, file_info$filename)

    metric_data <- tryCatch({
      read_csv(file_path)
    }, error = function(e) {
      logger::log_info("PASS table-level file not found: {file_info$filename} (optional)")
      NULL
    })

    if (!is.null(metric_data) && "table_name" %in% colnames(metric_data) && file_info$score_column %in% colnames(metric_data)) {
      # Extract just table_name and the score column
      table_level_metrics[[metric_name]] <- data.frame(
        table_name = metric_data$table_name,
        score = metric_data[[file_info$score_column]],
        stringsAsFactors = FALSE
      )
      logger::log_info("Loaded {metric_name} table-level scores for {nrow(table_level_metrics[[metric_name]])} tables")
    }
  }

  # Return structured PASS data
  list(
    overall = overall_data,
    components = components_data,
    metric_overall_data = metric_overall_data,
    table_level_metrics = table_level_metrics,
    metric_weights = metric_weights
  )
}

#' Check data availability flags
#'
#' Determines which data sources are available.
#'
#' @param delivery_data Delivery report data frame or NULL
#' @param dqd_data DQD results data frame or NULL
#' @param pass_data PASS results list or NULL
#' @return List with has_delivery_data, has_dqd_data, and has_pass_data flags
check_data_availability <- function(delivery_data, dqd_data, pass_data = NULL) {
  list(
    has_delivery_data = !is.null(delivery_data),
    has_dqd_data = !is.null(dqd_data),
    has_pass_data = !is.null(pass_data)
  )
}

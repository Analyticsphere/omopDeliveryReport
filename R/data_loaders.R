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

#' Check data availability flags
#'
#' Determines which data sources are available.
#'
#' @param delivery_data Delivery report data frame or NULL
#' @param dqd_data DQD results data frame or NULL
#' @return List with has_delivery_data and has_dqd_data flags
check_data_availability <- function(delivery_data, dqd_data) {
  list(
    has_delivery_data = !is.null(delivery_data),
    has_dqd_data = !is.null(dqd_data)
  )
}

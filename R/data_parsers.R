# ==============================================================================
# PARSING CONFIGURATION
# ==============================================================================

# Define parsing rules for all standard metrics
.METRIC_PARSERS <- list(
  valid_tables = list(
    pattern = "^Valid table name:",
    regex = "Valid table name: (\\w+)",
    fields = c("table_name"),
    post_process = function(df) df |> dplyr::distinct()
  ),

  invalid_tables = list(
    pattern = "^Invalid table name:",
    regex = "Invalid table name: (\\w+)",
    fields = c("table_name"),
    post_process = function(df) df |> dplyr::distinct()
  ),

  valid_row_counts = list(
    pattern = "^Valid row count:",
    regex = "Valid row count: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  ),

  invalid_row_counts = list(
    pattern = "^Invalid row count:",
    regex = "Invalid row count: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  ),

  final_row_counts = list(
    pattern = "^Final row count:",
    regex = "Final row count: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  ),

  missing_person_id = list(
    pattern = "Number of rows removed due to missing person_id values:",
    regex = "Number of rows removed due to missing person_id values: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  ),

  referential_integrity_violations = list(
    pattern = "^Person_id referential integrity violation count:",
    regex = "Person_id referential integrity violation count: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  ),

  valid_columns = list(
    pattern = "^Valid column name:",
    regex = "Valid column name: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name")
  ),

  invalid_columns = list(
    pattern = "^Invalid column name:",
    regex = "Invalid column name: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name")
  ),

  missing_columns = list(
    pattern = "^Missing column:",
    regex = "Missing column: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name")
  ),

  default_date_values = list(
    pattern = "^Date/datetime default value count:",
    regex = "Date/datetime default value count: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name"),
    value_field = "count"
  ),

  invalid_concepts = list(
    pattern = "^Invalid concept_id count:",
    regex = "Invalid concept_id count: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name"),
    value_field = "count",
    post_process = function(df) {
      df |> dplyr::filter(!stringr::str_detect(column_name, "_source_concept_id$"))
    }
  ),

  type_concepts = list(
    pattern = "^Type concept breakdown:",
    regex = "Type concept breakdown: (\\w+)",
    fields = c("table_name"),
    string_field = "type_concept",
    value_field = "count"
  ),

  source_vocabularies = list(
    pattern = "^Source vocabulary breakdown:",
    regex = "Source vocabulary breakdown: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name"),
    string_field = "vocabulary",
    value_field = "count"
  ),

  target_vocabularies = list(
    pattern = "^Target vocabulary breakdown:",
    regex = "Target vocabulary breakdown: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name"),
    string_field = "vocabulary",
    value_field = "count"
  ),

  harmonization_statuses = list(
    pattern = "^Vocab harmonization status:",
    regex = "Vocab harmonization status: (\\w+) - (.+)",
    fields = c("table_name", "status"),
    value_field = "count",
    post_process = function(df) {
      df |> dplyr::filter(!stringr::str_detect(tolower(status), "domain check"))
    }
  ),

  row_dispositions = list(
    pattern = "^Vocab harmonization row disposition:",
    regex = "Vocab harmonization row disposition: (\\w+) - (.+)",
    fields = c("table_name", "disposition"),
    value_field = "count"
  ),

  time_series = list(
    pattern = "^Time series row count:",
    regex = "Time series row count: (\\w+)\\.(\\d+)",
    fields = c("table_name", "year"),
    value_field = "count",
    post_process = function(df) {
      df |>
        dplyr::mutate(year = as.integer(year)) |>
        dplyr::arrange(table_name, year)
    }
  )
)

# ==============================================================================
# GENERIC PARSER
# ==============================================================================

#' Parse a metric using configuration
#'
#' Generic parser that handles all standard metric patterns.
#'
#' @param delivery_data Data frame with raw delivery report data
#' @param parser_config List with parsing configuration
#' @return Parsed data frame
parse_metric <- function(delivery_data, parser_config) {
  # Filter by pattern
  result <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, parser_config$pattern))

  # Return empty if no matches
  if (nrow(result) == 0) {
    return(create_empty_metric_df(parser_config))
  }

  # Extract fields from regex
  matches <- stringr::str_match(result$name, parser_config$regex)
  for (i in seq_along(parser_config$fields)) {
    result[[parser_config$fields[i]]] <- matches[, i + 1]
  }

  # Add value_as_number as count if specified
  if (!is.null(parser_config$value_field)) {
    result[[parser_config$value_field]] <- result$value_as_number
  }

  # Add value_as_string field if specified
  if (!is.null(parser_config$string_field)) {
    result[[parser_config$string_field]] <- result$value_as_string
  }

  # Select final columns
  final_cols <- c(
    parser_config$fields,
    if (!is.null(parser_config$string_field)) parser_config$string_field,
    if (!is.null(parser_config$value_field)) parser_config$value_field
  )
  result <- result |> dplyr::select(dplyr::all_of(final_cols))

  # Apply post-processing if specified
  if (!is.null(parser_config$post_process)) {
    result <- parser_config$post_process(result)
  }

  result
}

#' Create empty metric data frame with correct structure
#'
#' @param parser_config Parser configuration
#' @return Empty data frame with correct columns
create_empty_metric_df <- function(parser_config) {
  col_names <- c(
    parser_config$fields,
    if (!is.null(parser_config$string_field)) parser_config$string_field,
    if (!is.null(parser_config$value_field)) parser_config$value_field
  )

  # Remove any NULL values
  col_names <- col_names[!sapply(col_names, is.null)]

  # Create empty data frame with correct column types
  empty_df <- tibble::tibble()
  for (col in col_names) {
    if (!is.null(parser_config$value_field) && col == parser_config$value_field) {
      empty_df[[col]] <- numeric()
    } else if (col == "year") {
      empty_df[[col]] <- integer()
    } else {
      empty_df[[col]] <- character()
    }
  }
  empty_df
}

# ==============================================================================
# MAIN PARSING FUNCTION
# ==============================================================================

#' Parse delivery report metrics from CSV data (Configuration-Driven)
#'
#' Extracts all metrics from the delivery report CSV into a structured list.
#'
#' @param delivery_data Data frame with raw delivery report data
#' @return List containing all parsed metrics
#' @export
parse_delivery_metrics <- function(delivery_data) {
  metrics <- list()

  # Parse all standard metrics using configuration
  for (metric_name in names(.METRIC_PARSERS)) {
    metrics[[metric_name]] <- parse_metric(delivery_data, .METRIC_PARSERS[[metric_name]])
  }

  # Parse complex metrics that need custom handling
  # Table transitions (two-part regex)
  metrics$table_transitions <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization table transition:")) |>
    dplyr::mutate(
      source_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 2],
      target_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 3],
      count = value_as_number
    ) |>
    dplyr::select(source_table, target_table, count)

  # Same-table mappings (complex calculation)
  metrics$same_table_mappings <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization same-table mapping:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization same-table mapping: (\\w+) - ")[, 2],
      mapping = stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 1],
      source_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 2]),
      target_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 3]),
      total_rows = value_as_number,
      rows_added = total_rows * ((target_multiplier - 1) / target_multiplier)
    ) |>
    dplyr::select(table_name, mapping, source_multiplier, target_multiplier, total_rows, rows_added)

  # Metadata (special handling for date formatting)
  delivery_date_raw <- delivery_data |> dplyr::filter(name == "Delivery date") |> dplyr::pull(value_as_string)
  delivery_date_formatted <- format_date_safe(delivery_date_raw)

  processing_date_raw <- delivery_data |> dplyr::filter(name == "Delivery processing date") |> dplyr::pull(value_as_string)
  processing_date_formatted <- format_date_safe(processing_date_raw)

  metrics$metadata <- list(
    site = delivery_data |> dplyr::filter(name == "Site") |> dplyr::pull(value_as_string),
    delivery_date = delivery_date_formatted,
    processing_date = processing_date_formatted,
    delivered_cdm_version = delivery_data |> dplyr::filter(name == "Delivered CDM version") |> dplyr::pull(value_as_string),
    delivered_vocab_version = delivery_data |> dplyr::filter(name == "Delivered vocabulary version") |> dplyr::pull(value_as_string),
    standardized_cdm_version = delivery_data |> dplyr::filter(name == "Standardized to CDM version") |> dplyr::pull(value_as_string),
    standardized_vocab_version = delivery_data |> dplyr::filter(name == "Standardized to vocabulary version") |> dplyr::pull(value_as_string),
    pipeline_version = delivery_data |> dplyr::filter(name == "Pipeline file processor version") |> dplyr::pull(value_as_string),
    file_format = delivery_data |> dplyr::filter(name == "File delivery format") |> dplyr::pull(value_as_string)
  )

  # Single value metrics
  metrics$missing_person_id_count <- delivery_data |>
    dplyr::filter(name == "Number of persons with missing person_id") |>
    dplyr::pull(value_as_number)

  if (length(metrics$missing_person_id_count) == 0) {
    metrics$missing_person_id_count <- 0
  }

  # Perform type concept grouping
  metrics$type_concepts_grouped <- group_type_concepts(metrics$type_concepts)

  return(metrics)
}

#' Helper: Format date safely
#'
#' @param date_raw Raw date string
#' @return Formatted date string
format_date_safe <- function(date_raw) {
  tryCatch({
    date_obj <- as.Date(date_raw, format = "%m/%d/%y")
    if (is.na(date_obj)) {
      date_obj <- as.Date(date_raw, format = "%Y-%m-%d")
    }
    format(date_obj, "%Y-%m-%d")
  }, error = function(e) {
    date_raw
  })
}

# ==============================================================================
# TYPE CONCEPT GROUPING
# ==============================================================================

#' Apply type concept grouping rules
#'
#' Categorizes OMOP type concepts into broader groups (EHR, Claims, Disease registry,
#' Patient reported, Unlabeled, Other) based on naming patterns.
#'
#' @param type_concepts Data frame with type_concept column
#' @return Data frame with added type_group column
#' @export
group_type_concepts <- function(type_concepts) {
  type_concepts |>
    dplyr::mutate(
      type_group = dplyr::case_when(
        # Unlabeled group
        is.na(type_concept) |
          type_concept == "" |
          type_concept == "0" |
          tolower(type_concept) == "no matching concept" ~ "Unlabeled",
        # EHR group (case-sensitive)
        stringr::str_detect(type_concept, "EHR") ~ "EHR",
        # Claims group (case-insensitive)
        stringr::str_detect(tolower(type_concept), "claim") ~ "Claims",
        stringr::str_detect(tolower(type_concept), "payer system record") ~ "Claims",
        # Disease registry group
        type_concept %in% c("Registry", "Tumor Registry") ~ "Disease registry",
        # Patient reported group
        type_concept %in% c("Patient self-report", "Patient self-tested",
                            "Patient filled survey", "Survey",
                            "Patient Self-Reported Medication") ~ "Patient reported",
        # Other
        TRUE ~ "Other"
      )
    )
}

# ==============================================================================
# EMPTY METRICS STRUCTURE
# ==============================================================================

#' Create empty metrics structure
#'
#' Used when delivery data is not available. Creates a metrics list with all
#' expected fields populated with empty data structures.
#'
#' @return List with empty metric structures matching parse_delivery_metrics() output
#' @export
create_empty_metrics <- function() {
  list(
    metadata = list(
      site = "Unknown",
      delivery_date = "Unknown",
      processing_date = "Unknown",
      delivered_cdm_version = "Unknown",
      standardized_cdm_version = "Unknown",
      delivered_vocab_version = "Unknown",
      standardized_vocab_version = "Unknown",
      file_format = "unknown",
      pipeline_version = "Unknown"
    ),
    valid_tables = data.frame(table_name = character()),
    valid_row_counts = data.frame(table_name = character(), count = integer()),
    invalid_row_counts = data.frame(table_name = character(), count = integer()),
    final_row_counts = data.frame(table_name = character(), count = integer()),
    missing_person_id = data.frame(table_name = character(), count = integer()),
    missing_person_id_count = 0,
    default_date_values = data.frame(table_name = character(), column_name = character(), count = integer()),
    invalid_concepts = data.frame(table_name = character(), column_name = character(), count = integer()),
    referential_integrity_violations = data.frame(table_name = character(), count = integer()),
    invalid_columns = data.frame(table_name = character(), column_name = character()),
    missing_columns = data.frame(table_name = character(), column_name = character()),
    same_table_mappings = data.frame(table_name = character(), total_rows = integer()),
    table_transitions = data.frame(source_table = character(), target_table = character(), count = integer()),
    source_vocabularies = data.frame(table_name = character(), vocabulary = character(), count = integer()),
    target_vocabularies = data.frame(table_name = character(), vocabulary = character(), count = integer()),
    type_concepts = data.frame(table_name = character(), type_concept = character(), count = integer()),
    type_concepts_grouped = data.frame(table_name = character(), type_group = character(), count = integer()),
    harmonization_statuses = data.frame(table_name = character(), status = character(), count = integer()),
    row_dispositions = data.frame(table_name = character(), disposition = character(), count = integer()),
    time_series = data.frame(year = integer(), table_name = character(), count = integer())
  )
}

# ==============================================================================
# DQD SCORE CALCULATIONS
# ==============================================================================

#' Calculate overall DQD score
#'
#' Computes percentage of DQD checks that passed.
#' A check passes if failed == 0.
#'
#' @param dqd_data Data frame with DQD results
#' @return Numeric percentage (0-100), rounded to nearest integer
#' @export
calculate_overall_dqd_score <- function(dqd_data) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(NA_real_)
  }

  total_pass <- sum(dqd_data$failed == 0, na.rm = TRUE)
  total_checks <- nrow(dqd_data)

  if (total_checks == 0) {
    return(NA_real_)
  }

  score <- (total_pass / total_checks) * 100
  return(round(score, 0))
}

#' Calculate DQD score for one or more tables
#'
#' Base function that calculates a single DQD score for any set of tables.
#' The score represents the percentage of DQD checks that passed.
#'
#' @param dqd_data Data frame with DQD results
#' @param tables Character vector of table names (can be one or many)
#' @return Numeric percentage (0-100), rounded to nearest integer, or NA if no checks
#' @export
#'
#' @examples
#' \dontrun{
#' # Single table
#' calculate_dqd_score(dqd_data, "person")
#'
#' # Multiple tables
#' calculate_dqd_score(dqd_data, c("person", "visit_occurrence"))
#' }
calculate_dqd_score <- function(dqd_data, tables) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(NA_real_)
  }

  # Convert table names to uppercase for matching (DQD uses uppercase)
  tables_upper <- toupper(tables)

  # Filter to checks for these tables
  table_checks <- dqd_data |>
    dplyr::filter(toupper(cdmTableName) %in% tables_upper)

  if (nrow(table_checks) == 0) {
    return(NA_real_)
  }

  # Calculate pass rate
  total_pass <- sum(table_checks$failed == 0, na.rm = TRUE)
  total_checks <- nrow(table_checks)

  score <- (total_pass / total_checks) * 100
  return(round(score, 0))
}

#' Calculate DQD scores grouped by table groups
#'
#' Calculates a separate DQD score for each group of tables.
#' Returns a named list where each element is the score for that group.
#'
#' @param dqd_data Data frame with DQD results
#' @param table_groups Named list of table groups (each element is a character vector)
#' @return Named list of DQD scores (one per group)
#' @export
#'
#' @examples
#' \dontrun{
#' table_groups <- list(
#'   "Clinical" = c("person", "visit_occurrence"),
#'   "Vocabulary" = c("concept", "vocabulary")
#' )
#' calculate_dqd_scores_by_group(dqd_data, table_groups)
#' }
calculate_dqd_scores_by_group <- function(dqd_data, table_groups) {
  if (is.null(dqd_data)) {
    # Return NA for all groups
    return(lapply(table_groups, function(x) NA_real_))
  }

  lapply(table_groups, function(tables) {
    calculate_dqd_score(dqd_data, tables)
  })
}

#' Calculate DQD scores for each table individually
#'
#' Calculates a separate DQD score for each table.
#' Returns a named list where each element is the score for that table.
#'
#' @param dqd_data Data frame with DQD results
#' @param table_names Character vector of table names
#' @return Named list of DQD scores (one per table)
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_dqd_scores_by_table(dqd_data, c("person", "visit_occurrence"))
#' }
calculate_dqd_scores_by_table <- function(dqd_data, table_names) {
  if (is.null(dqd_data)) {
    # Return NA for all tables
    scores <- rep(NA_real_, length(table_names))
    names(scores) <- table_names
    return(as.list(scores))
  }

  # Calculate score for each table individually
  scores <- lapply(table_names, function(table) {
    calculate_dqd_score(dqd_data, table)
  })
  names(scores) <- table_names
  scores
}


#' Create DQD grid summary
#'
#' Aggregates DQD checks by category and context (Verification/Validation).
#'
#' @param dqd_data Data frame with DQD results
#' @return Data frame with grid structure
#' @export
create_dqd_grid <- function(dqd_data) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(tibble::tibble(
      category = character(),
      context = character(),
      Pass = integer(),
      Fail = integer(),
      Total = integer(),
      `% Pass` = numeric()
    ))
  }

  # Extract category and context from checkName
  dqd_data <- dqd_data |>
    dplyr::mutate(
      category = dplyr::case_when(
        grepl("plausible", checkName, ignore.case = TRUE) ~ "Plausibility",
        grepl("is", checkName, ignore.case = TRUE) ~ "Conformance",
        grepl("measure", checkName, ignore.case = TRUE) ~ "Completeness",
        TRUE ~ "Other"
      ),
      context = dplyr::case_when(
        grepl("Verification", context, ignore.case = TRUE) ~ "Verification",
        grepl("Validation", context, ignore.case = TRUE) ~ "Validation",
        TRUE ~ "Unknown"
      )
    )

  # Aggregate by category and context
  grid <- dqd_data |>
    dplyr::group_by(category, context) |>
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),
      Fail = sum(failed > 0, na.rm = TRUE),
      Total = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `% Pass` = round((Pass / Total) * 100, 0)
    )

  # Add totals row for each category
  category_totals <- grid |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      Pass = sum(Pass),
      Fail = sum(Fail),
      Total = sum(Total),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      context = "Total",
      `% Pass` = round((Pass / Total) * 100, 0)
    ) |>
    dplyr::select(category, context, Pass, Fail, Total, `% Pass`)

  # Add grand total rows
  grand_total_verification <- grid |>
    dplyr::filter(context == "Verification") |>
    dplyr::summarise(Pass = sum(Pass), Fail = sum(Fail), Total = sum(Total)) |>
    dplyr::mutate(category = "Total", context = "Verification", `% Pass` = round((Pass / Total) * 100, 0)) |>
    dplyr::select(category, context, Pass, Fail, Total, `% Pass`)

  grand_total_validation <- grid |>
    dplyr::filter(context == "Validation") |>
    dplyr::summarise(Pass = sum(Pass), Fail = sum(Fail), Total = sum(Total)) |>
    dplyr::mutate(category = "Total", context = "Validation", `% Pass` = round((Pass / Total) * 100, 0)) |>
    dplyr::select(category, context, Pass, Fail, Total, `% Pass`)

  grand_total_total <- category_totals |>
    dplyr::summarise(Pass = sum(Pass), Fail = sum(Fail), Total = sum(Total)) |>
    dplyr::mutate(category = "Total", context = "Total", `% Pass` = round((Pass / Total) * 100, 0)) |>
    dplyr::select(category, context, Pass, Fail, Total, `% Pass`)

  # Combine and sort
  dplyr::bind_rows(grid, category_totals, grand_total_verification, grand_total_validation, grand_total_total) |>
    dplyr::arrange(category, context)
}

#' Create empty DQD scores structure
#'
#' Used when DQD data is unavailable.
#'
#' @return List with NA overall score and empty grid
#' @export
create_empty_dqd_scores <- function() {
  list(
    overall = NA_real_,
    grid = create_dqd_grid(NULL)
  )
}

# ==============================================================================
# PASS SCORE FUNCTIONS
# ==============================================================================

#' Calculate overall PASS score
#'
#' Extracts composite PASS score with confidence intervals from PASS results.
#'
#' @param pass_data List containing PASS results (overall, components, table_scores)
#' @return List with overall_score, ci_lower, ci_upper, or NA values if unavailable
#' @export
calculate_overall_pass_score <- function(pass_data) {
  if (is.null(pass_data) || is.null(pass_data$overall) || nrow(pass_data$overall) == 0) {
    return(list(
      overall_score = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    ))
  }

  overall_row <- pass_data$overall[1, ]

  list(
    overall_score = overall_row$composite_score,
    ci_lower = overall_row$ci_95_lower,
    ci_upper = overall_row$ci_95_upper
  )
}

#' Parse PASS component breakdown
#'
#' Extracts and formats PASS component metrics with descriptions.
#' Sorts by weighted_contribution descending.
#'
#' @param pass_data List containing PASS results
#' @return Data frame with metric, description, score, weight, contribution, percent
#' @export
parse_pass_components <- function(pass_data) {
  if (is.null(pass_data) || is.null(pass_data$components) || nrow(pass_data$components) == 0) {
    return(tibble::tibble(
      metric = character(),
      description = character(),
      score = numeric(),
      weight = numeric(),
      weighted_contribution = numeric(),
      percent_contribution = numeric()
    ))
  }

  components <- pass_data$components |>
    dplyr::arrange(desc(weighted_contribution)) |>
    dplyr::mutate(
      # Add descriptions from constants
      description = dplyr::case_when(
        metric == "accessibility" ~ .PASS_METRIC_DESCRIPTIONS$accessibility,
        metric == "provenance" ~ .PASS_METRIC_DESCRIPTIONS$provenance,
        metric == "standards" ~ .PASS_METRIC_DESCRIPTIONS$standards,
        metric == "concept_diversity" ~ .PASS_METRIC_DESCRIPTIONS$concept_diversity,
        metric == "source_diversity" ~ .PASS_METRIC_DESCRIPTIONS$source_diversity,
        metric == "temporal" ~ .PASS_METRIC_DESCRIPTIONS$temporal,
        TRUE ~ ""
      )
    ) |>
    dplyr::select(metric, description, score, weight, weighted_contribution, percent_contribution)

  components
}

#' Extract table-level PASS scores
#'
#' Creates a named list of PASS scores by table name.
#'
#' @param pass_data List containing PASS results
#' @return Named list where names are table names and values are PASS scores
#' @export
extract_pass_table_scores <- function(pass_data) {
  if (is.null(pass_data) || is.null(pass_data$table_scores) || nrow(pass_data$table_scores) == 0) {
    return(list())
  }

  # Convert to named list
  scores <- as.list(pass_data$table_scores$pass_score)
  names(scores) <- tolower(pass_data$table_scores$table_name)

  scores
}

#' Create empty PASS scores structure
#'
#' Used when PASS data is unavailable.
#'
#' @return List with NA overall score and empty components
#' @export
create_empty_pass_scores <- function() {
  list(
    overall_score = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    components = tibble::tibble(
      metric = character(),
      description = character(),
      score = numeric(),
      weight = numeric(),
      weighted_contribution = numeric(),
      percent_contribution = numeric()
    )
  )
}

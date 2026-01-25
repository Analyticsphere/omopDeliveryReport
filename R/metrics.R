# ==============================================================================
# Metrics - Calculations and Formatting
# ==============================================================================
# Consolidated module combining pure calculations and display formatting.
# These functions calculate metrics and format them for display.

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Extract count for a table from metrics
#'
#' Helper to safely extract a count value from a metric data frame.
#'
#' @param metric_df Data frame with table_name and count columns
#' @param table_name Character table name to extract
#' @return Integer count (0 if not found)
#' @export
get_table_count <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  value <- metric_df |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(count)

  ifelse(length(value) > 0, value[1], 0)
}

#' Extract sum of counts for a table from metrics
#'
#' Some metrics have multiple rows per table (one row per column).
#' For example, default_date_values has one row for each date column.
#' This function sums all counts for a given table.
#'
#' Use get_table_count() for metrics with one row per table.
#' Use get_table_count_sum() for metrics with multiple rows per table.
#'
#' @param metric_df Data frame with table_name and count columns
#' @param table_name Character table name to extract
#' @return Integer sum of counts (0 if not found)
#' @export
get_table_count_sum <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  total <- metric_df |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)

  ifelse(length(total) > 0, total[1], 0)
}

#' Get the maximum count for a table across all columns
#'
#' Used for metrics where we want "rows affected" not "total values".
#' For example, if a table has 2 date columns both with default dates,
#' we want the max (rows affected) not the sum (total default values).
#'
#' @param metric_df Data frame with columns: table_name, column_name, count
#' @param table_name Character table name to filter by
#' @return Integer maximum count, or 0 if no data
#' @export
get_table_count_max <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  filtered <- metric_df |>
    dplyr::filter(table_name == !!table_name)

  # Return 0 if no rows match (avoids warning from max on empty set)
  if (nrow(filtered) == 0) return(0)

  maximum <- filtered |>
    dplyr::summarise(maximum = max(count, na.rm = TRUE)) |>
    dplyr::pull(maximum)

  ifelse(length(maximum) > 0 && !is.infinite(maximum), maximum[1], 0)
}

#' Check if a table participates in vocabulary harmonization
#'
#' @param table_name Character table name
#' @return Logical TRUE if table is harmonized
#' @export
is_harmonized_table <- function(table_name) {
  table_name %in% .HARMONIZED_TABLES
}

# ==============================================================================
# CALCULATION FUNCTIONS
# ==============================================================================

#' Calculate percentage with safe division
#'
#' Safely calculates percentage, returning 0 if denominator is 0 or NA.
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @return Numeric percentage (0-100), returns 0 if denominator is 0
#' @export
calculate_percentage <- function(numerator, denominator) {
  if (denominator == 0 || is.na(denominator) || is.na(numerator)) return(0)
  (numerator / denominator) * 100
}

#' Calculate vocabulary harmonization impact
#'
#' Determines the net row change due to vocabulary harmonization.
#' Formula: (same_table_result_rows - valid_rows) + transitions_in
#'
#' - same_table_result_rows: Rows remaining after same-table vocab mappings (includes 1:N expansion)
#' - valid_rows: Valid input rows (baseline, excludes quality issues)
#' - transitions_in: Rows received from other tables via cross-table mappings
#'
#' @param same_table_rows Integer rows after same-table mappings
#' @param valid_rows Integer valid input rows
#' @param transitions_in Integer rows received from other tables
#' @return Integer harmonization impact (negative = rows lost, positive = rows gained)
#' @export
calculate_harmonization <- function(same_table_rows, valid_rows, transitions_in) {
  (same_table_rows - valid_rows) + transitions_in
}

#' Calculate quality issues count
#'
#' Sums invalid rows and rows with missing person_id.
#'
#' @param invalid_rows Integer count of invalid rows
#' @param missing_rows Integer count of rows with missing person_id
#' @return Integer total quality issues
#' @export
calculate_quality_issues <- function(invalid_rows, missing_rows) {
  invalid_rows + missing_rows
}

#' Calculate expected final row count
#'
#' Determines expected final row count after quality filtering and harmonization.
#' Formula: initial_rows - quality_issues + harmonization
#' For non-harmonized tables, harmonization is 0.
#'
#' @param initial_rows Integer initial row count
#' @param quality_issues Integer quality issues to subtract
#' @param harmonization Integer harmonization impact
#' @return Integer expected final rows
#' @export
calculate_expected_final_rows <- function(initial_rows, quality_issues, harmonization) {
  initial_rows - quality_issues + harmonization
}

#' Calculate row-per-patient metric
#'
#' Computes average rows per patient for a given table.
#'
#' @param row_count Integer total rows
#' @param patient_count Integer total patients
#' @return Numeric rows per patient (2 decimal places), returns 0 if no patients
#' @export
calculate_row_per_patient <- function(row_count, patient_count) {
  if (patient_count == 0 || is.na(patient_count) || is.na(row_count)) return(0)
  round(row_count / patient_count, 2)
}

#' Calculate total number of participants
#'
#' Sums all rows in person table (valid + invalid) to get participant count.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @return Integer participant count
#' @export
calculate_num_participants <- function(metrics) {
  valid <- get_table_count(metrics$valid_row_counts, "person")
  invalid <- get_table_count(metrics$invalid_row_counts, "person")
  valid + invalid
}

#' Calculate total rows removed due to missing person_id
#'
#' Sums rows removed across all tables due to missing person_id values.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @return Integer total rows removed
#' @export
calculate_total_rows_removed <- function(metrics) {
  if (is.null(metrics$missing_person_id) || nrow(metrics$missing_person_id) == 0) {
    return(0)
  }

  total <- sum(metrics$missing_person_id$count, na.rm = TRUE)
  return(total)
}

# ==============================================================================
# FORMATTING FUNCTIONS
# ==============================================================================

#' Format numeric value for display
#'
#' Formats numbers with thousands separators for readability.
#'
#' @param value Numeric value
#' @param big_mark Character to use as thousands separator (default: ",")
#' @return Character formatted number
#' @export
#' @examples
#' format_number(1000)      # "1,000"
#' format_number(1234567)   # "1,234,567"
format_number <- function(value, big_mark = ",") {
  if (is.na(value) || is.null(value)) return("0")
  format(value, big.mark = big_mark, scientific = FALSE)
}

#' Calculate percentage and format for display
#'
#' Combines calculation and formatting in one step for convenience.
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @param decimal_places Integer number of decimal places (default: 1)
#' @return Character formatted percentage (e.g., "25.5")
#' @export
format_percentage <- function(numerator, denominator, decimal_places = 1) {
  pct <- calculate_percentage(numerator, denominator)
  format(round(pct, decimal_places), nsmall = decimal_places)
}

#' Determine DQD score class for styling
#'
#' Maps DQD scores to CSS classes for color-coded display.
#' - >= 95%: "good" (green)
#' - >= 85%: "fair" (yellow)
#' - < 85%: "poor" (red)
#' - NA: "neutral" (gray)
#'
#' @param score Numeric DQD score (0-100) or NA
#' @return Character CSS class name ("good", "fair", "poor", or "neutral")
#' @export
get_dqd_score_class <- function(score) {
  if (is.na(score)) {
    return("neutral")
  } else if (score >= 95) {
    return("good")
  } else if (score >= 85) {
    return("fair")
  } else {
    return("poor")
  }
}

#' Determine status badge based on delivery status
#'
#' @param delivered Logical whether table was delivered
#' @return List with status text and CSS class
#' @export
get_status_badge <- function(delivered) {
  if (delivered) {
    list(text = "Delivered", class = "delivered")
  } else {
    list(text = "Not Delivered", class = "not-delivered")
  }
}

#' Determine metric card class based on value and threshold
#'
#' Determines styling based on whether zero is considered good or bad.
#'
#' @param value Numeric value to evaluate
#' @param zero_is_good Logical whether zero is good (TRUE) or bad (FALSE)
#' @return Character CSS class ("success", "warning", or "neutral")
#' @export
get_metric_class <- function(value, zero_is_good = TRUE) {
  if (is.na(value)) return("neutral")

  if (zero_is_good) {
    if (value == 0) return("success")
    return("warning")
  } else {
    if (value > 0) return("success")
    return("neutral")
  }
}

#' Format harmonization value for display with sign
#'
#' Formats harmonization values with appropriate signs and styling.
#' - Positive values: "+X" (green)
#' - Negative values: "-X" (red)
#' - Zero: "0" (neutral)
#' - Non-harmonized tables: "--" (neutral)
#'
#' @param harmonization Integer harmonization value
#' @param is_harmonized_table Logical whether table participates in harmonization
#' @return List with display text and CSS class
#' @export
format_harmonization_display <- function(harmonization, is_harmonized_table) {
  if (!is_harmonized_table) {
    return(list(text = "--", class = "harmonization-neutral"))
  }

  if (harmonization > 0) {
    return(list(
      text = paste0("+", format_number(harmonization)),
      class = "harmonization-positive"
    ))
  } else if (harmonization < 0) {
    return(list(
      text = format_number(harmonization),
      class = "harmonization-negative"
    ))
  } else {
    return(list(
      text = "0",
      class = "harmonization-neutral"
    ))
  }
}

#' Format quality issues for display with sign
#'
#' Formats quality issue counts with minus sign if issues exist.
#'
#' @param quality_issues Integer quality issues count
#' @return List with display text and CSS class
#' @export
format_quality_issues_display <- function(quality_issues) {
  if (quality_issues > 0) {
    return(list(
      text = paste0("-", format_number(quality_issues)),
      class = "harmonization-negative"
    ))
  } else {
    return(list(
      text = format_number(quality_issues),
      class = "harmonization-neutral"
    ))
  }
}

# ==============================================================================
# INDIVIDUAL METRIC CALCULATORS
# ==============================================================================
# These functions calculate specific metrics for a table with alert logic.
# Each returns a structured list with values, percentages, and alert flags.

#' Calculate default date metric for a table
#'
#' Uses MAX across date columns, not SUM, to represent "rows affected"
#' rather than "total default date values". This prevents percentages > 100%
#' when multiple date columns have defaults.
#'
#' Vocabulary tables are excluded from alerts as they often have standard
#' default dates by design (e.g., valid_start_date = 1900-01-01).
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with rows, percent, and has_alert
#' @export
calculate_default_date_metric <- function(table_name, metrics) {
  # Use max instead of sum to get "rows with at least one default date"
  rows <- get_table_count_max(metrics$default_date_values, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)
  percent <- calculate_percentage(rows, final_rows)
  threshold <- .ALERT_THRESHOLDS$default_dates_pct

  # Check if this is a vocabulary table (they have default dates by design)
  vocab_tables <- .TABLE_GROUPS[["Vocabulary"]]
  is_vocab_table <- table_name %in% vocab_tables

  # Only alert if above threshold, table has data, and is not a vocabulary table
  has_alert <- percent > threshold && final_rows > 0 && !is_vocab_table

  list(
    rows = rows,
    percent = percent,
    has_alert = has_alert
  )
}

#' Calculate invalid concepts metric for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with rows, percent, and has_alert
#' @export
calculate_invalid_concepts_metric <- function(table_name, metrics) {
  rows <- get_table_count_sum(metrics$invalid_concepts, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)
  percent <- calculate_percentage(rows, final_rows)
  threshold <- .ALERT_THRESHOLDS$invalid_concepts_count

  # Alert if any invalid concepts (threshold is 0)
  has_alert <- rows > threshold && final_rows > 0

  list(
    rows = rows,
    percent = percent,
    has_alert = has_alert
  )
}

#' Calculate missing person_id metric for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with rows, percent, and has_alert
#' @export
calculate_missing_person_metric <- function(table_name, metrics) {
  # Special handling for person table
  rows <- if (table_name == "person") {
    metrics$missing_person_id_count
  } else {
    get_table_count(metrics$missing_person_id, table_name)
  }

  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  invalid_rows <- get_table_count(metrics$invalid_row_counts, table_name)
  initial_rows <- valid_rows + invalid_rows + rows

  percent <- calculate_percentage(rows, initial_rows)

  # Alert if any rows missing person_id
  has_alert <- rows > 0

  list(
    rows = rows,
    percent = percent,
    has_alert = has_alert
  )
}

#' Calculate invalid rows metric for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with rows, percent, and has_alert
#' @export
calculate_invalid_rows_metric <- function(table_name, metrics) {
  rows <- get_table_count(metrics$invalid_row_counts, table_name)
  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  missing_person <- if (table_name == "person") {
    metrics$missing_person_id_count
  } else {
    get_table_count(metrics$missing_person_id, table_name)
  }
  initial_rows <- valid_rows + rows + missing_person

  percent <- calculate_percentage(rows, initial_rows)

  # Alert if any invalid rows
  has_alert <- rows > 0

  list(
    rows = rows,
    percent = percent,
    has_alert = has_alert
  )
}

#' Calculate referential integrity metric for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with rows, percent, and has_alert
#' @export
calculate_ref_integrity_metric <- function(table_name, metrics) {
  rows <- get_table_count(metrics$referential_integrity_violations, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)
  percent <- calculate_percentage(rows, final_rows)

  # Alert if any referential integrity violations
  has_alert <- rows > 0 && final_rows > 0

  list(
    rows = rows,
    percent = percent,
    has_alert = has_alert
  )
}

#' Calculate count metrics for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @param harmonization Integer harmonization impact
#' @return List with count values and validation status
#' @export
calculate_count_metrics <- function(table_name, metrics, harmonization = 0) {
  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  invalid_rows <- get_table_count(metrics$invalid_row_counts, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)

  missing_rows <- if (table_name == "person") {
    metrics$missing_person_id_count
  } else {
    get_table_count(metrics$missing_person_id, table_name)
  }

  initial_rows <- valid_rows + invalid_rows + missing_rows
  quality_issues <- calculate_quality_issues(invalid_rows, missing_rows)

  # Check if counts are valid (for harmonized tables)
  # This compares expected vs actual final row counts
  expected_final <- calculate_expected_final_rows(initial_rows, quality_issues, harmonization)

  # Validate: expected should match actual final row count
  is_valid <- (expected_final == final_rows)
  has_mismatch_alert <- !is_valid

  list(
    valid = valid_rows,
    invalid = invalid_rows,
    missing = missing_rows,
    initial = initial_rows,
    final = final_rows,
    quality_issues = quality_issues,
    expected_final = expected_final,
    is_valid = is_valid,
    has_mismatch_alert = has_mismatch_alert
  )
}

#' Calculate harmonization metric for a table
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @return List with harmonization values and display info
#' @export
calculate_harmonization_metric <- function(table_name, metrics) {
  is_harmonized <- is_harmonized_table(table_name)

  if (!is_harmonized) {
    return(list(
      value = 0,
      is_harmonized = FALSE,
      same_table_rows = 0,
      transitions_in = 0,
      rows_out = 0,
      display = list(text = "--", class = "harmonization-neutral")
    ))
  }

  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)

  # Get same-table mapping results (uses total_rows column)
  same_table_rows <- if (is.null(metrics$same_table_mappings) || nrow(metrics$same_table_mappings) == 0) {
    0
  } else {
    total <- metrics$same_table_mappings |>
      dplyr::filter(table_name == !!table_name) |>
      dplyr::summarise(total = sum(total_rows, na.rm = TRUE)) |>
      dplyr::pull(total)
    ifelse(length(total) > 0, total[1], 0)
  }

  # Get transitions
  transitions <- metrics$table_transitions |>
    dplyr::filter(source_table == !!table_name | target_table == !!table_name)

  transitions_in <- transitions |>
    dplyr::filter(target_table == !!table_name, source_table != !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  transitions_in <- ifelse(length(transitions_in) > 0, transitions_in[1], 0)

  rows_out <- transitions |>
    dplyr::filter(source_table == !!table_name, target_table != !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  rows_out <- ifelse(length(rows_out) > 0, rows_out[1], 0)

  # Calculate harmonization impact
  value <- calculate_harmonization(same_table_rows, valid_rows, transitions_in)
  display <- format_harmonization_display(value, TRUE)

  list(
    value = value,
    is_harmonized = TRUE,
    same_table_rows = same_table_rows,
    transitions_in = transitions_in,
    rows_out = rows_out,
    display = display
  )
}

# ==============================================================================
# COMPREHENSIVE TABLE METRICS CALCULATOR (SINGLE SOURCE OF TRUTH)
# ==============================================================================

#' Calculate ALL metrics for a table (Single Source of Truth)
#'
#' This is the canonical function for calculating all table metrics.
#' All functions should use this data rather than performing their own calculations.
#'
#' @param table_name Character table name
#' @param metrics List from parse_delivery_metrics()
#' @param dqd_score Numeric DQD score (or NA)
#' @return List with ALL table metrics (raw values + formatted strings + alerts)
#' @export
calculate_table_metrics <- function(table_name, metrics, dqd_score = NA) {

  # Calculate each metric using individual calculators
  default_dates <- calculate_default_date_metric(table_name, metrics)
  invalid_concepts <- calculate_invalid_concepts_metric(table_name, metrics)
  missing_person <- calculate_missing_person_metric(table_name, metrics)
  invalid_rows_metric <- calculate_invalid_rows_metric(table_name, metrics)
  ref_integrity <- calculate_ref_integrity_metric(table_name, metrics)

  # Calculate harmonization first (needed for count metrics)
  harmonization <- calculate_harmonization_metric(table_name, metrics)

  # Calculate counts (uses harmonization value)
  counts <- calculate_count_metrics(table_name, metrics, harmonization$value)

  # Check if this table should show mismatch alert
  tables_without_alert <- get_tables_without_mismatch_alert()
  should_show_mismatch <- !(table_name %in% tables_without_alert)

  # Aggregate alert status - THIS FIXES THE BUG!
  # Only include mismatch alert if this table should show it
  has_any_alert <- (
    default_dates$has_alert ||
    invalid_concepts$has_alert ||
    missing_person$has_alert ||
    invalid_rows_metric$has_alert ||
    ref_integrity$has_alert ||
    (counts$has_mismatch_alert && should_show_mismatch)
  )

  # Delivery status
  delivered <- table_name %in% metrics$valid_tables$table_name

  # Format displays
  quality_issues_display <- format_quality_issues_display(counts$quality_issues)

  # Return comprehensive structure
  list(
    name = table_name,
    delivered = delivered,

    # Counts (include full counts object for nested access)
    counts = counts,
    initial_rows = counts$initial,
    valid_rows = counts$valid,
    invalid_rows = counts$invalid,
    missing_person_id_rows = counts$missing,
    final_rows = counts$final,
    quality_issues = counts$quality_issues,
    expected_final = counts$expected_final,
    counts_valid = counts$is_valid,

    # Quality metrics
    default_dates = default_dates,
    invalid_concepts = invalid_concepts,
    missing_person = missing_person,
    invalid_rows_metric = invalid_rows_metric,
    ref_integrity = ref_integrity,

    # Harmonization
    harmonization = harmonization,

    # DQD
    dqd_score = dqd_score,
    dqd_class = get_dqd_score_class(dqd_score),

    # Alerts (BUG FIX - properly calculated!)
    has_any_alert = has_any_alert,
    row_class = if (has_any_alert) "row-warning" else "",

    # Formatted displays
    quality_issues_display = quality_issues_display$text,
    quality_issues_class = quality_issues_display$class
  )
}

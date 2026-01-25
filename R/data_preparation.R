# ==============================================================================
# Data Preparation - Aggregation and Template Formatting
# ==============================================================================
# Prepares structured data for JSON serialization and HTML template rendering.

# ==============================================================================
# CORE TABLE METRICS AGGREGATION
# ==============================================================================

#' Prepare data for a single table
#'
#' Aggregates all metrics for one table into a comprehensive data structure.
#' This is the base function - use prepare_tables_data() for multiple tables.
#'
#' @param table_name Character table name
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @param dqd_score Numeric DQD score for this table (or NA)
#' @return List with all table metrics and calculated values
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare data for one table
#' person_data <- prepare_table_data("person", metrics, dqd_score = 95)
#' }
prepare_table_data <- function(table_name, metrics, dqd_score) {

  # Extract basic counts
  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  invalid_rows <- get_table_count(metrics$invalid_row_counts, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)

  # Special handling for person table missing person_id
  missing_rows <- if (table_name == "person") {
    metrics$missing_person_id_count
  } else {
    get_table_count(metrics$missing_person_id, table_name)
  }

  # Quality metrics
  referential_integrity_violations <- get_table_count(metrics$referential_integrity_violations, table_name)
  default_date_rows <- get_table_count_sum(metrics$default_date_values, table_name)
  invalid_concept_rows <- get_table_count_sum(metrics$invalid_concepts, table_name)

  # Calculate derived counts
  initial_rows <- valid_rows + invalid_rows + missing_rows
  quality_issues <- calculate_quality_issues(invalid_rows, missing_rows)

  # Vocabulary harmonization metrics
  # Note: same_table_mappings uses 'total_rows' not 'count'
  same_table_result_rows <- if (is.null(metrics$same_table_mappings) || nrow(metrics$same_table_mappings) == 0) {
    0
  } else {
    total <- metrics$same_table_mappings |>
      dplyr::filter(table_name == !!table_name) |>
      dplyr::summarise(total = sum(total_rows, na.rm = TRUE)) |>
      dplyr::pull(total)
    ifelse(length(total) > 0, total[1], 0)
  }

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
  is_harmonized <- is_harmonized_table(table_name)
  harmonization <- if (is_harmonized) {
    calculate_harmonization(same_table_result_rows, valid_rows, transitions_in)
  } else {
    0
  }

  # Calculate percentages
  default_date_percent <- calculate_percentage(default_date_rows, final_rows)
  invalid_concept_percent <- calculate_percentage(invalid_concept_rows, final_rows)
  missing_person_id_percent <- calculate_percentage(missing_rows, initial_rows)
  referential_integrity_percent <- calculate_percentage(referential_integrity_violations, final_rows)
  invalid_rows_percent <- calculate_percentage(invalid_rows, initial_rows)

  # Extract related data
  type_concepts <- metrics$type_concepts_grouped |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group, desc(count)) |>
    dplyr::mutate(type_group = as.character(type_group))

  invalid_columns <- metrics$invalid_columns |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(column_name)
  invalid_columns <- if (length(invalid_columns) == 0) list() else as.list(invalid_columns)

  missing_columns <- metrics$missing_columns |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(column_name)
  missing_columns <- if (length(missing_columns) == 0) list() else as.list(missing_columns)

  source_vocab <- metrics$source_vocabularies |>
    dplyr::filter(table_name == !!table_name)

  target_vocab <- metrics$target_vocabularies |>
    dplyr::filter(table_name == !!table_name)

  harmonization_statuses <- metrics$harmonization_statuses |>
    dplyr::filter(table_name == !!table_name)

  dispositions <- metrics$row_dispositions |>
    dplyr::filter(table_name == !!table_name)

  same_table_mappings <- metrics$same_table_mappings |>
    dplyr::filter(table_name == !!table_name)

  # Delivery status
  delivered <- table_name %in% metrics$valid_tables$table_name

  # Validation: check if type concepts sum to final rows
  type_concept_total <- type_concepts |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  type_concept_total <- ifelse(length(type_concept_total) > 0, type_concept_total[1], 0)

  has_transitions <- (transitions_in > 0 || same_table_result_rows > 0)

  # Determine expected final count and validation status
  if (type_concept_total > 0) {
    expected_final <- type_concept_total
    counts_valid <- (type_concept_total == final_rows)
  } else if (valid_rows == 0 && !has_transitions) {
    expected_final <- 0
    counts_valid <- (final_rows == 0)
  } else {
    expected_final <- final_rows
    counts_valid <- TRUE
  }

  # Skip validation warnings for non-harmonized tables
  if (!is_harmonized) {
    counts_valid <- TRUE
  }

  # Return comprehensive table data
  list(
    name = table_name,
    delivered = delivered,
    valid_rows = valid_rows,
    invalid_rows = invalid_rows,
    initial_rows = initial_rows,
    final_rows = final_rows,
    missing_person_id_rows = missing_rows,
    referential_integrity_violations = referential_integrity_violations,
    harmonization = harmonization,
    transitions_in = transitions_in,
    rows_out = rows_out,
    same_table_result_rows = same_table_result_rows,
    default_date_rows = default_date_rows,
    invalid_concept_rows = invalid_concept_rows,
    default_date_percent = default_date_percent,
    invalid_concept_percent = invalid_concept_percent,
    missing_person_id_percent = missing_person_id_percent,
    referential_integrity_percent = referential_integrity_percent,
    invalid_rows_percent = invalid_rows_percent,
    quality_issues = quality_issues,
    type_concepts = type_concepts,
    invalid_columns = invalid_columns,
    missing_columns = missing_columns,
    source_vocabularies = source_vocab,
    target_vocabularies = target_vocab,
    transitions = transitions,
    harmonization_statuses = harmonization_statuses,
    dispositions = dispositions,
    same_table_mappings = same_table_mappings,
    dqd_score = dqd_score,
    counts_valid = counts_valid,
    expected_final = expected_final
  )
}

#' Prepare data for multiple tables
#'
#' Convenience function to prepare data for multiple tables at once.
#' Returns a named list where each element contains the prepared data for one table.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @param table_dqd_scores Named list of DQD scores per table
#' @return Named list of table data structures (one per table)
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare data for all valid tables
#' tables_data <- prepare_tables_data(metrics, table_dqd_scores)
#' }
prepare_tables_data <- function(metrics, table_dqd_scores) {
  # Get all unique tables from valid_tables
  all_tables <- unique(metrics$valid_tables$table_name)

  # Prepare data for each table
  table_data_list <- lapply(all_tables, function(table_name) {
    dqd_score <- table_dqd_scores[[table_name]]
    if (is.null(dqd_score)) dqd_score <- NA_real_

    prepare_table_data(table_name, metrics, dqd_score)
  })

  names(table_data_list) <- all_tables
  table_data_list
}

# ==============================================================================
# GROUP-LEVEL AGGREGATION
# ==============================================================================

#' Prepare report with all data (delivery report, DQD, etc.)
#'
#' Aggregates data by groups, calculates statistics, and prepares
#' structured data for JSON serialization.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @param table_groups Named list of table groups from get_table_groups()
#' @param group_dqd_scores Named list of DQD scores per group
#' @param table_dqd_scores Named list of DQD scores per table
#' @return List containing all pre-calculated report data
#' @export
prepare_report_data <- function(metrics, table_groups, group_dqd_scores, table_dqd_scores) {

  # Prepare group-level data
  groups_data <- list()

  for (group_name in names(table_groups)) {
    group_tables <- table_groups[[group_name]]

    # Get data for each table in group
    table_data_list <- lapply(group_tables, function(tbl) {
      prepare_table_data(tbl, metrics, table_dqd_scores[[tbl]])
    })
    names(table_data_list) <- group_tables

    # Group-level vocabularies
    group_source_vocab <- metrics$source_vocabularies |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(vocabulary) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(desc(count))

    group_target_vocab <- metrics$target_vocabularies |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(vocabulary) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(desc(count))

    # Group-level transitions
    group_transitions <- metrics$table_transitions |>
      dplyr::filter(source_table %in% group_tables | target_table %in% group_tables)

    # Group-level type concepts - DETAILED (with type_concept field)
    group_type_concepts_detailed <- metrics$type_concepts_grouped |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(type_group, type_concept) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

    # Aggregate by type_group only and ensure all groups present - SUMMARY
    group_type_concepts_summary <- group_type_concepts_detailed |>
      dplyr::group_by(type_group) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      fill_missing_type_groups()

    # Add percentages to summary
    total_type_concept_count <- sum(group_type_concepts_summary$count, na.rm = TRUE)
    group_type_concepts_summary <- group_type_concepts_summary |>
      dplyr::mutate(
        percent = if (total_type_concept_count > 0) (count / total_type_concept_count) * 100 else 0
      )

    # Order detailed type concepts by canonical group, then count descending
    group_type_concepts <- group_type_concepts_detailed |>
      dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
      dplyr::arrange(type_group, desc(count)) |>
      dplyr::mutate(type_group = as.character(type_group))

    # Calculate transition statistics for this group
    total_transition_rows <- sum(group_transitions$count, na.rm = TRUE)
    same_table_transitions <- group_transitions |>
      dplyr::filter(source_table == target_table)
    cross_table_transitions <- group_transitions |>
      dplyr::filter(source_table != target_table)
    same_table_count <- sum(same_table_transitions$count, na.rm = TRUE)
    cross_table_count <- sum(cross_table_transitions$count, na.rm = TRUE)

    groups_data[[group_name]] <- list(
      tables = table_data_list,
      dqd_score = group_dqd_scores[[group_name]],
      source_vocabularies = group_source_vocab,
      target_vocabularies = group_target_vocab,
      transitions = group_transitions,
      transition_stats = list(
        total_rows = total_transition_rows,
        same_table_count = same_table_count,
        cross_table_count = cross_table_count
      ),
      type_concepts = group_type_concepts,
      type_concepts_summary = group_type_concepts_summary
    )
  }

  # Overall transitions for vocabulary harmonization section
  overall_transitions <- metrics$table_transitions

  # Overall harmonization statuses (summarize across all tables)
  overall_harmonization_statuses <- metrics$harmonization_statuses |>
    dplyr::group_by(status) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count))

  # Overall type concepts - DETAILED (with type_concept field)
  overall_type_concepts_detailed <- metrics$type_concepts_grouped |>
    dplyr::group_by(type_group, type_concept) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Aggregate by type_group only and ensure all groups present - SUMMARY
  overall_type_concepts_summary <- overall_type_concepts_detailed |>
    dplyr::group_by(type_group) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    fill_missing_type_groups()

  # Add percentages to overall summary
  total_overall_count <- sum(overall_type_concepts_summary$count, na.rm = TRUE)
  overall_type_concepts_summary <- overall_type_concepts_summary |>
    dplyr::mutate(
      percent = if (total_overall_count > 0) (count / total_overall_count) * 100 else 0
    )

  # Order detailed type concepts by canonical group, then count descending
  overall_type_concepts <- overall_type_concepts_detailed |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group, desc(count)) |>
    dplyr::mutate(type_group = as.character(type_group))

  # Calculate overall transition statistics
  total_overall_transition_rows <- sum(overall_transitions$count, na.rm = TRUE)
  overall_same_table_transitions <- overall_transitions |>
    dplyr::filter(source_table == target_table)
  overall_cross_table_transitions <- overall_transitions |>
    dplyr::filter(source_table != target_table)
  overall_same_table_count <- sum(overall_same_table_transitions$count, na.rm = TRUE)
  overall_cross_table_count <- sum(overall_cross_table_transitions$count, na.rm = TRUE)

  # Calculate total initial dataset size (for harmonization context)
  total_initial_rows <- sum(metrics$valid_row_counts$count, na.rm = TRUE) +
                        sum(metrics$invalid_row_counts$count, na.rm = TRUE)

  # Return structured data
  list(
    groups = groups_data,
    overall_transitions = overall_transitions,
    overall_transition_stats = list(
      total_rows = total_overall_transition_rows,
      same_table_count = overall_same_table_count,
      cross_table_count = overall_cross_table_count
    ),
    harmonization_statuses = overall_harmonization_statuses,
    total_initial_rows = total_initial_rows,
    overall_type_concepts = overall_type_concepts,
    overall_type_concepts_summary = overall_type_concepts_summary
  )
}

#' Fill missing type concept groups with zero counts
#'
#' Ensures all type concept groups are present with zero counts for missing groups.
#' This ensures consistent display even when some groups have no data.
#'
#' @param type_concept_data Data frame with type_group and count columns
#' @return Data frame with all type groups present
#' @export
fill_missing_type_groups <- function(type_concept_data) {
  all_groups <- get_type_concept_group_order()

  # Create a complete template with all groups
  complete_groups <- tibble::tibble(
    type_group = all_groups,
    count = 0
  )

  # Merge with actual data, keeping all groups
  result <- complete_groups |>
    dplyr::left_join(
      type_concept_data |> dplyr::select(type_group, count),
      by = "type_group",
      suffix = c("_template", "_actual")
    ) |>
    dplyr::mutate(
      count = dplyr::coalesce(count_actual, count_template)
    ) |>
    dplyr::select(-count_template, -count_actual)

  # Ensure proper ordering
  result |>
    dplyr::mutate(type_group = factor(type_group, levels = all_groups)) |>
    dplyr::arrange(type_group) |>
    dplyr::mutate(type_group = as.character(type_group))
}

# ==============================================================================
# TEMPLATE-SPECIFIC DATA PREPARERS
# ==============================================================================

#' Prepare data for overview section template
#'
#' Prepares all variables needed for the overview section template,
#' including formatting and warning indicators.
#'
#' @param metrics List of metrics from parse_delivery_metrics()
#' @param dqd_scores List with DQD scores
#' @param num_participants Integer participant count
#' @param total_rows_removed Integer rows removed count
#' @param has_delivery_data Logical whether delivery data is available
#' @param has_dqd_data Logical whether DQD data is available
#' @return List of template variables
#' @export
prepare_overview_data <- function(metrics, dqd_scores, num_participants, total_rows_removed, has_delivery_data, has_dqd_data) {
  # Format displays
  tables_delivered <- if (has_delivery_data) as.character(nrow(metrics$valid_tables)) else "N/A"
  participants_display <- if (has_delivery_data) format_number(num_participants) else "N/A"
  missing_person_display <- if (has_delivery_data) as.character(metrics$missing_person_id_count) else "N/A"
  rows_removed_display <- if (has_delivery_data) format_number(total_rows_removed) else "N/A"

  # Warning classes and icons
  if (has_delivery_data) {
    missing_warning <- if (metrics$missing_person_id_count > 0) " warning" else " success"
    missing_icon <- if (metrics$missing_person_id_count > 0) '<span class="warning-icon">⚠️</span>' else '<span class="success-icon">✓</span>'
    rows_warning <- if (total_rows_removed > 0) " warning" else " success"
    rows_icon <- if (total_rows_removed > 0) '<span class="warning-icon">⚠️</span>' else '<span class="success-icon">✓</span>'
    person_word <- if (metrics$missing_person_id_count == 1) "Person" else "Persons"
  } else {
    missing_warning <- " neutral"
    missing_icon <- ""
    rows_warning <- " neutral"
    rows_icon <- ""
    person_word <- "Persons"
  }

  # DQD score
  dqd_class <- get_dqd_score_class(dqd_scores$overall)
  dqd_score_display <- if (is.na(dqd_scores$overall)) "N/A" else paste0(dqd_scores$overall, "%")

  list(
    tables_delivered = tables_delivered,
    participants_display = participants_display,
    dqd_class = dqd_class,
    dqd_score_display = dqd_score_display,
    missing_warning = missing_warning,
    missing_icon = missing_icon,
    missing_person_display = missing_person_display,
    person_word = person_word,
    rows_warning = rows_warning,
    rows_icon = rows_icon,
    rows_removed_display = rows_removed_display
  )
}

#' Prepare DQD grid rows data for template
#'
#' Transforms DQD grid data into row structures for template rendering.
#'
#' @param grid Data frame with DQD grid data
#' @return List of row data for rendering
#' @export
prepare_dqd_grid_rows <- function(grid) {
  if (nrow(grid) == 0) {
    return(list())
  }

  # Reshape for display
  grid_wide <- grid |>
    tidyr::pivot_wider(names_from = context, values_from = c(Pass, Fail, Total, `% Pass`))

  categories <- c("Plausibility", "Conformance", "Completeness", "Total")

  rows_data <- lapply(categories, function(cat_name) {
    row_data <- grid_wide |> dplyr::filter(category == cat_name)

    if (nrow(row_data) == 0) return(NULL)

    row_class <- if (cat_name == "Total") ' class="total-row"' else ''

    list(
      row_class = row_class,
      category = cat_name,
      pass_verification = ifelse(is.na(row_data$Pass_Verification), 0, row_data$Pass_Verification),
      fail_verification = ifelse(is.na(row_data$Fail_Verification), 0, row_data$Fail_Verification),
      total_verification = ifelse(is.na(row_data$Total_Verification), 0, row_data$Total_Verification),
      percent_pass_verification = ifelse(is.na(row_data$`% Pass_Verification`), "0%", paste0(row_data$`% Pass_Verification`, "%")),
      pass_validation = ifelse(is.na(row_data$Pass_Validation), 0, row_data$Pass_Validation),
      fail_validation = ifelse(is.na(row_data$Fail_Validation), 0, row_data$Fail_Validation),
      total_validation = ifelse(is.na(row_data$Total_Validation), 0, row_data$Total_Validation),
      percent_pass_validation = ifelse(is.na(row_data$`% Pass_Validation`), "0%", paste0(row_data$`% Pass_Validation`, "%")),
      pass_total = ifelse(is.na(row_data$Pass_Total), 0, row_data$Pass_Total),
      fail_total = ifelse(is.na(row_data$Fail_Total), 0, row_data$Fail_Total),
      total_total = ifelse(is.na(row_data$Total_Total), 0, row_data$Total_Total),
      percent_pass_total = paste0(ifelse(is.na(row_data$`% Pass_Total`), 0, row_data$`% Pass_Total`), "%")
    )
  })

  # Filter out NULL entries
  Filter(Negate(is.null), rows_data)
}

#' Prepare data for time series section template
#'
#' Calculates year ranges and formats time series data for template rendering.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @return List with template variables or NULL if no data
#' @export
prepare_time_series_data <- function(metrics) {
  if (is.null(metrics) || is.null(metrics$time_series) || nrow(metrics$time_series) == 0) {
    return(NULL)
  }

  # Extract delivery date and calculate year ranges
  delivery_date_str <- metrics$metadata$delivery_date

  # Parse delivery date to get year
  delivery_year <- tryCatch({
    if (is.na(delivery_date_str) || delivery_date_str == "Unknown" || delivery_date_str == "") {
      as.integer(format(Sys.Date(), "%Y"))
    } else {
      as.integer(format(as.Date(delivery_date_str), "%Y"))
    }
  }, error = function(e) {
    as.integer(format(Sys.Date(), "%Y"))
  })

  # Calculate year ranges
  recent_start_year <- delivery_year - 15
  recent_end_year <- delivery_year - 1
  historical_start_year <- 1970
  historical_end_year <- delivery_year

  # Convert time series data to JSON for JavaScript
  time_series_json <- jsonlite::toJSON(metrics$time_series, dataframe = "rows")

  list(
    recent_start_year = recent_start_year,
    recent_end_year = recent_end_year,
    historical_start_year = historical_start_year,
    historical_end_year = historical_end_year,
    delivery_year = delivery_year,
    time_series_json = as.character(time_series_json)
  )
}

#' Prepare data for vocabulary harmonization section template
#'
#' Aggregates source and target vocabularies for top 10 display.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @return List with source and target vocabulary data
#' @export
prepare_vocab_harmonization_data <- function(metrics) {
  if (is.null(metrics) || is.null(metrics$source_vocabularies) || is.null(metrics$target_vocabularies)) {
    return(list(
      source_vocab_rows = list(),
      target_vocab_rows = list(),
      delivered_vocab_version = "Unknown",
      standardized_vocab_version = "Unknown"
    ))
  }

  # Overall source vocabularies (top 10)
  overall_source_vocab <- metrics$source_vocabularies |>
    dplyr::group_by(vocabulary) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count)) |>
    head(10)

  source_vocab_rows <- if (nrow(overall_source_vocab) > 0) {
    lapply(1:nrow(overall_source_vocab), function(i) {
      list(
        vocabulary = overall_source_vocab$vocabulary[i],
        count = format(overall_source_vocab$count[i], big.mark = ",")
      )
    })
  } else {
    list()
  }

  # Overall target vocabularies (top 10)
  overall_target_vocab <- metrics$target_vocabularies |>
    dplyr::group_by(vocabulary) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count)) |>
    head(10)

  target_vocab_rows <- if (nrow(overall_target_vocab) > 0) {
    lapply(1:nrow(overall_target_vocab), function(i) {
      list(
        vocabulary = overall_target_vocab$vocabulary[i],
        count = format(overall_target_vocab$count[i], big.mark = ",")
      )
    })
  } else {
    list()
  }

  list(
    source_vocab_rows = source_vocab_rows,
    target_vocab_rows = target_vocab_rows,
    delivered_vocab_version = metrics$metadata$delivered_vocab_version,
    standardized_vocab_version = metrics$metadata$standardized_vocab_version
  )
}

#' Prepare data for delivery report section template
#'
#' Formats table data with all display formatting, warnings, and icons.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @param table_groups Named list of table groups
#' @param group_dqd_scores Named list of DQD scores per group
#' @param num_participants Integer number of participants
#' @return List with dropdown options and group contents data
#' @export
prepare_delivery_report_data <- function(metrics, table_groups, group_dqd_scores, num_participants) {
  # Prepare dropdown options
  dropdown_options_data <- lapply(names(table_groups), function(group_name) {
    list(
      group_name = group_name,
      selected = if (group_name == "Clinical Data") " selected" else ""
    )
  })

  # Prepare each table group
  group_contents_data <- lapply(names(table_groups), function(group_name) {
    group_tables <- table_groups[[group_name]]
    group_id <- gsub(" ", "-", tolower(group_name))
    display_style <- if (group_name == "Clinical Data") "" else "display: none;"

    # Prepare DQD note
    group_dqd_score <- group_dqd_scores[[group_name]]
    dqd_note <- if (!is.na(group_dqd_score)) {
      sprintf('<p class="dqd-score-text"><strong>Data Quality Score for this group:</strong> <span class="dqd-inline">%s%%</span></p>',
              group_dqd_score)
    } else {
      '<p class="dqd-score-text"><strong>Data Quality Score:</strong> <span class="text-muted">Not available</span></p>'
    }

    # Prepare type concept subheader
    type_concept_subheader <- if (group_name == "All Tables") {
      "All Tables"
    } else {
      sprintf("%s Tables", group_name)
    }

    # Prepare table rows
    table_rows_data <- lapply(group_tables, function(tbl) {
      prepare_delivery_table_row(tbl, metrics, num_participants)
    })

    list(
      group_name = group_name,
      group_id = group_id,
      display_style = display_style,
      dqd_note = dqd_note,
      type_concept_subheader = type_concept_subheader,
      table_rows_data = table_rows_data
    )
  })

  list(
    dropdown_options_data = dropdown_options_data,
    group_contents_data = group_contents_data
  )
}

#' Prepare formatted data for a single table row in delivery report template
#'
#' Formats all fields for a single table row with appropriate styling classes.
#'
#' @param table_name Character table name
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @param num_participants Integer number of participants
#' @return List with all formatted fields for template
#' @export
prepare_delivery_table_row <- function(table_name, metrics, num_participants) {
  # Use the comprehensive calculator - SINGLE SOURCE OF TRUTH!
  # This eliminates ~100 lines of duplicate calculation code and fixes the bug
  table_metrics <- calculate_table_metrics(table_name, metrics, NA)

  # Calculate row per patient
  row_per_patient <- calculate_row_per_patient(table_metrics$final_rows, num_participants)

  # Determine status badge
  status_badge <- get_status_badge(table_metrics$delivered)

  # Build specific warning icons based on alert types
  warning_icons <- character(0)

  # Skip row count mismatch alert for pipeline-derived and vocabulary tables
  tables_without_alert <- get_tables_without_mismatch_alert()
  if (table_metrics$counts$has_mismatch_alert && !(table_name %in% tables_without_alert)) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Row count mismatch">🧮</span>')
  }

  if (table_metrics$default_dates$percent > 1 && table_metrics$final_rows > 0) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Default/placeholder dates">📅</span>')
  }

  if (table_metrics$invalid_concepts$rows > 0 && table_metrics$final_rows > 0) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Invalid concept IDs">📖</span>')
  }

  if (table_metrics$invalid_rows_metric$rows > 0 && table_metrics$final_rows > 0) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Invalid data type specifications">🧩</span>')
  }

  if (table_metrics$missing_person$rows > 0 && table_metrics$final_rows > 0) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Missing Connect ID">👤</span>')
  }

  if (table_metrics$ref_integrity$rows > 0 && table_metrics$final_rows > 0) {
    warning_icons <- c(warning_icons, '<span class="warning-icon" title="Referential integrity violations">🧑‍🧒</span>')
  }

  all_warnings <- if (length(warning_icons) > 0) {
    paste0(" ", paste(warning_icons, collapse = " "))
  } else {
    ""
  }

  # Format displays for template
  list(
    table_name = table_name,
    row_class = table_metrics$row_class,
    all_warnings = all_warnings,
    status_text = status_badge$text,
    status_class = status_badge$class,
    initial_rows_formatted = format_number(table_metrics$initial_rows),
    quality_issues_display = table_metrics$quality_issues_display,
    quality_issues_class = table_metrics$quality_issues_class,
    harmonization_display = table_metrics$harmonization$display$text,
    harmonization_class = table_metrics$harmonization$display$class,
    final_rows_formatted = format_number(table_metrics$final_rows),
    row_per_patient = sprintf("%.2f", row_per_patient)
  )
}

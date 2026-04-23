# ==============================================================================
# Tests for Pure Calculation Functions (metrics.R)
# ==============================================================================

library(testthat)

# ==============================================================================
# calculate_percentage()
# ==============================================================================

test_that("calculate_percentage returns correct values", {
  expect_equal(calculate_percentage(25, 100), 25)
  expect_equal(calculate_percentage(1, 2), 50)
  expect_equal(calculate_percentage(0, 100), 0)
  expect_equal(calculate_percentage(100, 100), 100)
  expect_equal(calculate_percentage(75, 50), 150)  # Can exceed 100%
})

test_that("calculate_percentage handles edge cases", {
  # Division by zero
  expect_equal(calculate_percentage(10, 0), 0)
  expect_equal(calculate_percentage(0, 0), 0)

  # NA values
  expect_equal(calculate_percentage(NA, 100), 0)
  expect_equal(calculate_percentage(10, NA), 0)
  expect_equal(calculate_percentage(NA, NA), 0)

  # Negative values (should still calculate)
  expect_equal(calculate_percentage(-10, 100), -10)
  expect_equal(calculate_percentage(10, -100), -10)
})

# ==============================================================================
# calculate_harmonization()
# ==============================================================================

test_that("calculate_harmonization returns correct values", {
  # Net positive impact (more rows gained than lost)
  expect_equal(calculate_harmonization(120, 100, 10), 30)

  # Net negative impact (more rows lost)
  expect_equal(calculate_harmonization(80, 100, 10), -10)

  # No change
  expect_equal(calculate_harmonization(100, 100, 0), 0)

  # Only same-table changes (no transitions)
  expect_equal(calculate_harmonization(150, 100, 0), 50)

  # Only transitions (no same-table changes)
  expect_equal(calculate_harmonization(100, 100, 25), 25)
})

test_that("calculate_harmonization handles edge cases", {
  # All zeros
  expect_equal(calculate_harmonization(0, 0, 0), 0)

  # Large values
  expect_equal(calculate_harmonization(1000000, 500000, 250000), 750000)
})

# ==============================================================================
# calculate_quality_issues()
# ==============================================================================

test_that("calculate_quality_issues sums correctly", {
  expect_equal(calculate_quality_issues(10, 5), 10)
  expect_equal(calculate_quality_issues(0, 0), 0)
  expect_equal(calculate_quality_issues(100, 0), 100)
  expect_equal(calculate_quality_issues(0, 50), 0)
})

test_that("calculate_quality_issues handles edge cases", {
  # Large missing-person counts should not affect quality issues
  expect_equal(calculate_quality_issues(1000000, 500000), 1000000)

  # Missing rows are ignored even when invalid rows are zero
  expect_equal(calculate_quality_issues(0, 10), 0)
})

# ==============================================================================
# calculate_expected_final_rows()
# ==============================================================================

test_that("calculate_expected_final_rows calculates correctly", {
  # Standard case: initial - quality + harmonization
  expect_equal(calculate_expected_final_rows(100, 10, 5), 95)

  # No quality issues or harmonization
  expect_equal(calculate_expected_final_rows(100, 0, 0), 100)

  # Only quality issues (no harmonization)
  expect_equal(calculate_expected_final_rows(100, 10, 0), 90)

  # Only harmonization (no quality issues)
  expect_equal(calculate_expected_final_rows(100, 0, 20), 120)

  # Negative harmonization
  expect_equal(calculate_expected_final_rows(100, 10, -5), 85)
})

test_that("calculate_expected_final_rows handles edge cases", {
  # All zeros
  expect_equal(calculate_expected_final_rows(0, 0, 0), 0)

  # Result could be negative (data quality catastrophe)
  expect_equal(calculate_expected_final_rows(100, 150, -50), -100)
})

test_that("calculate_count_metrics uses valid plus invalid rows as initial source of truth", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = "person",
      count = 10,
      stringsAsFactors = FALSE
    ),
    invalid_row_counts = data.frame(
      table_name = "person",
      count = 2,
      stringsAsFactors = FALSE
    ),
    final_row_counts = data.frame(
      table_name = "person",
      count = 7,
      stringsAsFactors = FALSE
    ),
    missing_person_id = data.frame(
      table_name = character(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    ),
    missing_person_id_count = 3,
    connect_exclusion_rule_rows = data.frame(
      table_name = character(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    ),
    identifier_not_in_connect_rows = data.frame(
      table_name = character(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    ),
    connect_patient_counts = list(
      connect_not_in_delivery = NA_real_,
      delivery_not_in_connect = NA_real_
    )
  )

  result <- calculate_count_metrics("person", metrics, harmonization = 0)

  expect_equal(result$initial, 12)
  expect_equal(result$invalid, 2)
  expect_equal(result$missing, 3)
  expect_equal(result$quality_issues, 2)
  expect_equal(result$participant_filter, 3)
  expect_equal(result$expected_final, 7)
  expect_true(result$is_valid)
})

test_that("calculate_count_metrics includes participant filtering separately from quality issues", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = "person",
      count = 110,
      stringsAsFactors = FALSE
    ),
    invalid_row_counts = data.frame(
      table_name = "person",
      count = 0,
      stringsAsFactors = FALSE
    ),
    final_row_counts = data.frame(
      table_name = "person",
      count = 64,
      stringsAsFactors = FALSE
    ),
    missing_person_id = data.frame(
      table_name = "person",
      count = 1,
      stringsAsFactors = FALSE
    ),
    missing_person_id_count = 1,
    connect_exclusion_rule_rows = data.frame(
      table_name = "person",
      count = 44,
      stringsAsFactors = FALSE
    ),
    identifier_not_in_connect_rows = data.frame(
      table_name = "person",
      count = 1,
      stringsAsFactors = FALSE
    ),
    connect_patient_counts = list(
      connect_not_in_delivery = 561,
      delivery_not_in_connect = 2
    )
  )

  result <- calculate_count_metrics("person", metrics, harmonization = 0)

  expect_equal(result$quality_issues, 0)
  expect_equal(result$participant_filter, 46)
  expect_equal(result$expected_final, 64)
  expect_true(result$is_valid)
})

test_that("calculate_harmonization_metric excludes participant filtering from harmonization", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = "drug_exposure",
      count = 1000,
      stringsAsFactors = FALSE
    ),
    connect_exclusion_rule_rows = data.frame(
      table_name = "drug_exposure",
      count = 100,
      stringsAsFactors = FALSE
    ),
    identifier_not_in_connect_rows = data.frame(
      table_name = "drug_exposure",
      count = 20,
      stringsAsFactors = FALSE
    ),
    connect_patient_counts = list(
      connect_not_in_delivery = NA_real_,
      delivery_not_in_connect = NA_real_
    ),
    same_table_mappings = data.frame(
      table_name = "drug_exposure",
      mapping = "Vocab harmonization same-table mapping: drug_exposure - 1:1",
      source_multiplier = 1,
      target_multiplier = 1,
      total_rows = 880,
      rows_added = 0,
      stringsAsFactors = FALSE
    ),
    table_transitions = data.frame(
      source_table = "procedure_occurrence",
      target_table = "drug_exposure",
      count = 15,
      stringsAsFactors = FALSE
    )
  )

  result <- calculate_harmonization_metric("drug_exposure", metrics)

  expect_equal(result$rows_available_for_harmonization, 880)
  expect_equal(result$value, 15)
  expect_equal(result$rows_added_from_mappings, 0)
})

test_that("calculate_harmonization_metric uses parsed 1:N mapping counts as source of truth", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = "measurement",
      count = 62073,
      stringsAsFactors = FALSE
    ),
    missing_person_id = data.frame(
      table_name = "measurement",
      count = 0,
      stringsAsFactors = FALSE
    ),
    connect_exclusion_rule_rows = data.frame(
      table_name = "measurement",
      count = 22471,
      stringsAsFactors = FALSE
    ),
    identifier_not_in_connect_rows = data.frame(
      table_name = "measurement",
      count = 91,
      stringsAsFactors = FALSE
    ),
    same_table_mappings = data.frame(
      table_name = c("measurement", "measurement"),
      mapping = c(
        "Vocab harmonization same-table mapping: measurement - 1:1",
        "Vocab harmonization same-table mapping: measurement - 1:2"
      ),
      source_multiplier = c(1, 1),
      target_multiplier = c(1, 2),
      total_rows = c(39510, 2),
      rows_added = c(0, 1),
      stringsAsFactors = FALSE
    ),
    table_transitions = data.frame(
      source_table = c("procedure_occurrence", "measurement"),
      target_table = c("measurement", "observation"),
      count = c(4, 1),
      stringsAsFactors = FALSE
    )
  )

  result <- calculate_harmonization_metric("measurement", metrics)

  expect_equal(result$rows_available_for_harmonization, 39511)
  expect_equal(result$rows_added_from_mappings, 1)
  expect_equal(result$rows_moved_out, 0)
  expect_equal(result$rows_copied_out, 1)
  expect_equal(result$value, 5)
})

test_that("prepare_delivery_table_row shows participant filter and adjusted harmonization counts", {
  delivery_data <- read.csv("../../inst/ref/delivery_report.csv", stringsAsFactors = FALSE)
  metrics <- parse_delivery_metrics(delivery_data)
  num_participants <- calculate_num_participants(metrics)

  person_row <- prepare_delivery_table_row("person", metrics, num_participants)
  procedure_row <- prepare_delivery_table_row("procedure_occurrence", metrics, num_participants)
  procedure_metrics <- calculate_table_metrics("procedure_occurrence", metrics, NA)
  measurement_data <- prepare_table_data("measurement", metrics, NA)

  expect_equal(person_row$quality_issues_display, "0")
  expect_equal(person_row$participant_filter_display, "-46")
  expect_equal(procedure_row$quality_issues_display, "-2")
  expect_equal(procedure_row$participant_filter_display, "-5,815")
  expect_equal(procedure_row$harmonization_display, "-375")
  expect_true(procedure_metrics$counts_valid)
  expect_equal(procedure_metrics$expected_final, 9562)
  expect_equal(procedure_metrics$harmonization$rows_added_from_mappings, 0)
  expect_equal(procedure_metrics$harmonization$rows_moved_out, 375)
  expect_equal(procedure_metrics$harmonization$rows_copied_out, 0)
  expect_equal(measurement_data$rows_moved_out, 0)
  expect_equal(measurement_data$rows_copied_out, 1)
})

test_that("prepare_delivery_table_row shows Connect participant warning icon when identifier is not in Connect", {
  metrics <- create_empty_metrics()
  metrics$valid_tables <- data.frame(table_name = "person", stringsAsFactors = FALSE)
  metrics$valid_row_counts <- data.frame(table_name = "person", count = 10, stringsAsFactors = FALSE)
  metrics$invalid_row_counts <- data.frame(table_name = "person", count = 0, stringsAsFactors = FALSE)
  metrics$final_row_counts <- data.frame(table_name = "person", count = 9, stringsAsFactors = FALSE)
  metrics$missing_person_id_count <- 0
  metrics$identifier_not_in_connect_rows <- data.frame(
    table_name = "person",
    count = 1,
    stringsAsFactors = FALSE
  )

  row <- prepare_delivery_table_row("person", metrics, num_participants = 10)

  expect_equal(row$row_class, "row-warning")
  expect_match(row$all_warnings, "🔎", fixed = TRUE)
})

# ==============================================================================
# calculate_row_per_patient()
# ==============================================================================

test_that("calculate_row_per_patient calculates correctly", {
  expect_equal(calculate_row_per_patient(100, 50), 2.0)
  expect_equal(calculate_row_per_patient(100, 100), 1.0)
  expect_equal(calculate_row_per_patient(50, 100), 0.5)
  expect_equal(calculate_row_per_patient(333, 100), 3.33)
  expect_equal(calculate_row_per_patient(0, 100), 0.0)
})

test_that("calculate_row_per_patient handles edge cases", {
  # Zero patients (division by zero)
  expect_equal(calculate_row_per_patient(100, 0), 0)
  expect_equal(calculate_row_per_patient(0, 0), 0)

  # NA values
  expect_equal(calculate_row_per_patient(NA, 100), 0)
  expect_equal(calculate_row_per_patient(100, NA), 0)
  expect_equal(calculate_row_per_patient(NA, NA), 0)

  # Very small ratio
  expect_equal(calculate_row_per_patient(1, 1000), 0.0)  # Rounds to 0.00
})

test_that("calculate_row_per_patient returns 2 decimal places", {
  result <- calculate_row_per_patient(100, 3)
  expect_equal(result, 33.33)

  result <- calculate_row_per_patient(1, 3)
  expect_equal(result, 0.33)
})

# ==============================================================================
# calculate_num_participants()
# ==============================================================================

test_that("calculate_num_participants sums valid and invalid counts", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = c("person", "visit_occurrence"),
      count = c(100, 500)
    ),
    invalid_row_counts = data.frame(
      table_name = c("person", "visit_occurrence"),
      count = c(10, 50)
    )
  )

  expect_equal(calculate_num_participants(metrics), 110)
})

test_that("calculate_num_participants handles missing data", {
  # Person not in valid_row_counts
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = character(0),
      count = numeric(0)
    ),
    invalid_row_counts = data.frame(
      table_name = c("person"),
      count = c(10)
    )
  )

  expect_equal(calculate_num_participants(metrics), 10)

  # Person not in invalid_row_counts
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = c("person"),
      count = c(100)
    ),
    invalid_row_counts = data.frame(
      table_name = character(0),
      count = numeric(0)
    )
  )

  expect_equal(calculate_num_participants(metrics), 100)
})

test_that("calculate_num_participants handles no person table", {
  metrics <- list(
    valid_row_counts = data.frame(
      table_name = character(0),
      count = numeric(0)
    ),
    invalid_row_counts = data.frame(
      table_name = character(0),
      count = numeric(0)
    )
  )

  expect_equal(calculate_num_participants(metrics), 0)
})

# ==============================================================================
# calculate_total_rows_removed()
# ==============================================================================

test_that("calculate_total_rows_removed sums all rows", {
  metrics <- list(
    missing_person_id = data.frame(
      table_name = c("visit_occurrence", "condition_occurrence", "drug_exposure"),
      count = c(10, 20, 15)
    )
  )

  expect_equal(calculate_total_rows_removed(metrics), 45)
})

test_that("calculate_total_rows_removed handles empty data", {
  metrics <- list(
    missing_person_id = data.frame(
      table_name = character(0),
      count = numeric(0)
    )
  )

  expect_equal(calculate_total_rows_removed(metrics), 0)

  # NULL case
  metrics <- list(
    missing_person_id = NULL
  )

  expect_equal(calculate_total_rows_removed(metrics), 0)
})

test_that("calculate_total_rows_removed handles NA values", {
  metrics <- list(
    missing_person_id = data.frame(
      table_name = c("visit_occurrence", "condition_occurrence"),
      count = c(10, NA)
    )
  )

  # na.rm = TRUE should handle this
  expect_equal(calculate_total_rows_removed(metrics), 10)
})

# ==============================================================================
# Helper Functions
# ==============================================================================

test_that("get_table_count extracts correct value", {
  metric_df <- data.frame(
    table_name = c("person", "visit_occurrence", "condition_occurrence"),
    count = c(100, 500, 300)
  )

  expect_equal(get_table_count(metric_df, "person"), 100)
  expect_equal(get_table_count(metric_df, "visit_occurrence"), 500)
  expect_equal(get_table_count(metric_df, "condition_occurrence"), 300)
})

test_that("get_table_count handles missing table", {
  metric_df <- data.frame(
    table_name = c("person"),
    count = c(100)
  )

  expect_equal(get_table_count(metric_df, "nonexistent_table"), 0)
})

test_that("get_table_count handles NULL or empty data", {
  expect_equal(get_table_count(NULL, "person"), 0)

  empty_df <- data.frame(
    table_name = character(0),
    count = numeric(0)
  )
  expect_equal(get_table_count(empty_df, "person"), 0)
})

test_that("get_table_count_sum sums multiple rows", {
  metric_df <- data.frame(
    table_name = c("person", "person", "visit_occurrence"),
    column_name = c("birth_datetime", "death_datetime", "visit_start_datetime"),
    count = c(10, 20, 5)
  )

  expect_equal(get_table_count_sum(metric_df, "person"), 30)
  expect_equal(get_table_count_sum(metric_df, "visit_occurrence"), 5)
})

test_that("get_table_count_sum handles missing table", {
  metric_df <- data.frame(
    table_name = c("person"),
    count = c(100)
  )

  expect_equal(get_table_count_sum(metric_df, "nonexistent_table"), 0)
})

test_that("get_table_count_max returns maximum across columns", {
  metric_df <- data.frame(
    table_name = c("person", "person", "visit_occurrence"),
    column_name = c("birth_datetime", "death_datetime", "visit_start_datetime"),
    count = c(10, 20, 5)
  )

  expect_equal(get_table_count_max(metric_df, "person"), 20)
  expect_equal(get_table_count_max(metric_df, "visit_occurrence"), 5)
})

test_that("get_table_count_max handles missing table", {
  metric_df <- data.frame(
    table_name = c("person"),
    column_name = c("birth_datetime"),
    count = c(100)
  )

  expect_equal(get_table_count_max(metric_df, "nonexistent_table"), 0)
})

test_that("get_table_count_max handles empty data frame", {
  metric_df <- data.frame(
    table_name = character(),
    column_name = character(),
    count = integer()
  )

  expect_equal(get_table_count_max(metric_df, "person"), 0)
})

test_that("is_harmonized_table identifies correctly", {
  # Should be true for clinical tables
  expect_true(is_harmonized_table("visit_occurrence"))
  expect_true(is_harmonized_table("condition_occurrence"))
  expect_true(is_harmonized_table("drug_exposure"))
  expect_true(is_harmonized_table("measurement"))

  # Should be false for non-clinical tables
  expect_false(is_harmonized_table("person"))
  expect_false(is_harmonized_table("location"))
  expect_false(is_harmonized_table("concept"))
  expect_false(is_harmonized_table("nonexistent_table"))
})

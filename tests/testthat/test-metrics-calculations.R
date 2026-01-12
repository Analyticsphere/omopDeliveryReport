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
  expect_equal(calculate_quality_issues(10, 5), 15)
  expect_equal(calculate_quality_issues(0, 0), 0)
  expect_equal(calculate_quality_issues(100, 0), 100)
  expect_equal(calculate_quality_issues(0, 50), 50)
})

test_that("calculate_quality_issues handles edge cases", {
  # Large numbers
  expect_equal(calculate_quality_issues(1000000, 500000), 1500000)

  # One negative (shouldn't happen in practice, but test anyway)
  expect_equal(calculate_quality_issues(-5, 10), 5)
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

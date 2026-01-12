# ==============================================================================
# Tests for DQD Score Calculations (data_parsers.R)
# ==============================================================================

library(testthat)
library(dplyr)

# ==============================================================================
# Helper: Create sample DQD data
# ==============================================================================

create_sample_dqd_data <- function() {
  data.frame(
    checkName = c(
      "isPlausibleValueLow",
      "isPlausibleValueHigh",
      "measurePersonCompleteness",
      "isPlausibleTemporalAfter",
      "measureValueCompleteness"
    ),
    cdmTableName = c("PERSON", "PERSON", "VISIT_OCCURRENCE", "VISIT_OCCURRENCE", "MEASUREMENT"),
    failed = c(0, 0, 0, 5, 0),  # 4 pass, 1 fail
    context = c("Validation", "Validation", "Verification", "Verification", "Verification"),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# calculate_overall_dqd_score()
# ==============================================================================

test_that("calculate_overall_dqd_score returns correct percentage", {
  dqd_data <- create_sample_dqd_data()

  # 4 pass, 1 fail = 80%
  expect_equal(calculate_overall_dqd_score(dqd_data), 80)
})

test_that("calculate_overall_dqd_score handles all passing", {
  dqd_data <- data.frame(
    checkName = c("check1", "check2", "check3"),
    cdmTableName = c("PERSON", "PERSON", "PERSON"),
    failed = c(0, 0, 0),
    context = c("Validation", "Validation", "Validation"),
    stringsAsFactors = FALSE
  )

  expect_equal(calculate_overall_dqd_score(dqd_data), 100)
})

test_that("calculate_overall_dqd_score handles all failing", {
  dqd_data <- data.frame(
    checkName = c("check1", "check2", "check3"),
    cdmTableName = c("PERSON", "PERSON", "PERSON"),
    failed = c(1, 5, 10),
    context = c("Validation", "Validation", "Validation"),
    stringsAsFactors = FALSE
  )

  expect_equal(calculate_overall_dqd_score(dqd_data), 0)
})

test_that("calculate_overall_dqd_score handles NULL or empty data", {
  expect_equal(calculate_overall_dqd_score(NULL), NA_real_)

  empty_dqd <- data.frame(
    checkName = character(0),
    cdmTableName = character(0),
    failed = numeric(0),
    context = character(0),
    stringsAsFactors = FALSE
  )
  expect_equal(calculate_overall_dqd_score(empty_dqd), NA_real_)
})

test_that("calculate_overall_dqd_score rounds to nearest integer", {
  dqd_data <- data.frame(
    checkName = c("check1", "check2", "check3"),
    cdmTableName = c("PERSON", "PERSON", "PERSON"),
    failed = c(0, 0, 1),  # 2/3 = 66.666...%
    context = c("Validation", "Validation", "Validation"),
    stringsAsFactors = FALSE
  )

  expect_equal(calculate_overall_dqd_score(dqd_data), 67)
})

# ==============================================================================
# calculate_dqd_score() - single or multiple tables
# ==============================================================================

test_that("calculate_dqd_score works for single table", {
  dqd_data <- create_sample_dqd_data()

  # PERSON has 2 checks, both pass = 100%
  expect_equal(calculate_dqd_score(dqd_data, "person"), 100)

  # VISIT_OCCURRENCE has 2 checks, 1 pass, 1 fail = 50%
  expect_equal(calculate_dqd_score(dqd_data, "visit_occurrence"), 50)

  # MEASUREMENT has 1 check, 1 pass = 100%
  expect_equal(calculate_dqd_score(dqd_data, "measurement"), 100)
})

test_that("calculate_dqd_score is case-insensitive", {
  dqd_data <- create_sample_dqd_data()

  expect_equal(calculate_dqd_score(dqd_data, "person"), 100)
  expect_equal(calculate_dqd_score(dqd_data, "PERSON"), 100)
  expect_equal(calculate_dqd_score(dqd_data, "Person"), 100)
})

test_that("calculate_dqd_score works for multiple tables", {
  dqd_data <- create_sample_dqd_data()

  # PERSON (2 pass) + VISIT_OCCURRENCE (1 pass, 1 fail) = 3 pass / 4 total = 75%
  expect_equal(calculate_dqd_score(dqd_data, c("person", "visit_occurrence")), 75)

  # All tables: 4 pass / 5 total = 80%
  expect_equal(
    calculate_dqd_score(dqd_data, c("person", "visit_occurrence", "measurement")),
    80
  )
})

test_that("calculate_dqd_score handles non-existent table", {
  dqd_data <- create_sample_dqd_data()

  expect_equal(calculate_dqd_score(dqd_data, "nonexistent_table"), NA_real_)
})

test_that("calculate_dqd_score handles NULL or empty data", {
  expect_equal(calculate_dqd_score(NULL, "person"), NA_real_)

  empty_dqd <- data.frame(
    checkName = character(0),
    cdmTableName = character(0),
    failed = numeric(0),
    context = character(0),
    stringsAsFactors = FALSE
  )
  expect_equal(calculate_dqd_score(empty_dqd, "person"), NA_real_)
})

# ==============================================================================
# calculate_dqd_scores_by_group()
# ==============================================================================

test_that("calculate_dqd_scores_by_group returns named list of scores", {
  dqd_data <- create_sample_dqd_data()

  table_groups <- list(
    "Clinical" = c("person", "visit_occurrence"),
    "Labs" = c("measurement")
  )

  result <- calculate_dqd_scores_by_group(dqd_data, table_groups)

  expect_type(result, "list")
  expect_equal(names(result), c("Clinical", "Labs"))

  # Clinical: 3 pass / 4 total = 75%
  expect_equal(result$Clinical, 75)

  # Labs: 1 pass / 1 total = 100%
  expect_equal(result$Labs, 100)
})

test_that("calculate_dqd_scores_by_group handles NULL data", {
  table_groups <- list(
    "Clinical" = c("person", "visit_occurrence"),
    "Labs" = c("measurement")
  )

  result <- calculate_dqd_scores_by_group(NULL, table_groups)

  expect_type(result, "list")
  expect_equal(names(result), c("Clinical", "Labs"))
  expect_true(is.na(result$Clinical))
  expect_true(is.na(result$Labs))
})

# ==============================================================================
# calculate_dqd_scores_by_table()
# ==============================================================================

test_that("calculate_dqd_scores_by_table returns named list of scores", {
  dqd_data <- create_sample_dqd_data()

  table_names <- c("person", "visit_occurrence", "measurement")

  result <- calculate_dqd_scores_by_table(dqd_data, table_names)

  expect_type(result, "list")
  expect_equal(names(result), table_names)

  expect_equal(result$person, 100)
  expect_equal(result$visit_occurrence, 50)
  expect_equal(result$measurement, 100)
})

test_that("calculate_dqd_scores_by_table handles NULL data", {
  table_names <- c("person", "visit_occurrence")

  result <- calculate_dqd_scores_by_table(NULL, table_names)

  expect_type(result, "list")
  expect_equal(names(result), table_names)
  expect_true(is.na(result$person))
  expect_true(is.na(result$visit_occurrence))
})

# ==============================================================================
# create_dqd_grid()
# ==============================================================================

test_that("create_dqd_grid creates correct structure", {
  dqd_data <- data.frame(
    checkName = c(
      "isPlausibleValueLow",           # Conformance
      "isPlausibleTemporalAfter",      # Plausibility
      "measurePersonCompleteness"      # Completeness
    ),
    cdmTableName = c("PERSON", "PERSON", "PERSON"),
    failed = c(0, 1, 0),  # 2 pass, 1 fail
    context = c("Validation", "Validation", "Verification"),
    stringsAsFactors = FALSE
  )

  result <- create_dqd_grid(dqd_data)

  # Should have category and context columns
  expect_true("category" %in% colnames(result))
  expect_true("context" %in% colnames(result))
  expect_true("Pass" %in% colnames(result))
  expect_true("Fail" %in% colnames(result))
  expect_true("Total" %in% colnames(result))
  expect_true("% Pass" %in% colnames(result))
})

test_that("create_dqd_grid categorizes checks correctly", {
  dqd_data <- data.frame(
    checkName = c(
      "isPlausibleValueLow",           # Contains "plausible" -> Plausibility
      "isStandardValidConcept",        # Contains "is" -> Conformance
      "measurePersonCompleteness"      # Contains "measure" -> Completeness
    ),
    cdmTableName = c("PERSON", "PERSON", "PERSON"),
    failed = c(0, 0, 0),
    context = c("Validation", "Validation", "Verification"),
    stringsAsFactors = FALSE
  )

  result <- create_dqd_grid(dqd_data)

  # Should have all three categories
  expect_true("Plausibility" %in% result$category)
  expect_true("Conformance" %in% result$category)
  expect_true("Completeness" %in% result$category)
})

test_that("create_dqd_grid handles NULL or empty data", {
  result <- create_dqd_grid(NULL)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("category" %in% colnames(result))
})

test_that("create_dqd_grid includes totals", {
  dqd_data <- data.frame(
    checkName = c("check1", "check2", "check3", "check4"),
    cdmTableName = c("PERSON", "PERSON", "PERSON", "PERSON"),
    failed = c(0, 0, 1, 1),  # 2 pass, 2 fail
    context = c("Validation", "Validation", "Verification", "Verification"),
    stringsAsFactors = FALSE
  )

  result <- create_dqd_grid(dqd_data)

  # Should include "Total" rows
  expect_true("Total" %in% result$category)
  expect_true("Total" %in% result$context)
})

# ==============================================================================
# create_empty_dqd_scores()
# ==============================================================================

test_that("create_empty_dqd_scores returns correct structure", {
  result <- create_empty_dqd_scores()

  expect_type(result, "list")
  expect_true("overall" %in% names(result))
  expect_true("grid" %in% names(result))

  expect_true(is.na(result$overall))
  expect_s3_class(result$grid, "tbl_df")
})

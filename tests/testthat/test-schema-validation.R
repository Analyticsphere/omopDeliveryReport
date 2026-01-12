# ==============================================================================
# Tests for Schema Validation (data_loaders.R)
# ==============================================================================

library(testthat)

# ==============================================================================
# validate_schema() - Delivery Report File
# ==============================================================================

test_that("validate_schema accepts valid delivery report schema", {
  valid_data <- data.frame(
    name = c("Site", "Delivery date"),
    value_as_string = c("Test Site", "2024-01-01"),
    value_as_number = c(NA, NA),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(valid_data, .RAW_DELIVERY_REPORT_FILE))
})

test_that("validate_schema detects missing columns in delivery report", {
  # Missing value_as_number column
  invalid_data <- data.frame(
    name = c("Site"),
    value_as_string = c("Test Site"),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_schema(invalid_data, .RAW_DELIVERY_REPORT_FILE),
    "missing required columns: value_as_number"
  )
})

test_that("validate_schema detects multiple missing columns in delivery report", {
  # Missing both value columns
  invalid_data <- data.frame(
    name = c("Site"),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_schema(invalid_data, .RAW_DELIVERY_REPORT_FILE),
    "missing required columns"
  )

  # Should mention both missing columns
  expect_error(
    validate_schema(invalid_data, .RAW_DELIVERY_REPORT_FILE),
    "value_as_string"
  )

  expect_error(
    validate_schema(invalid_data, .RAW_DELIVERY_REPORT_FILE),
    "value_as_number"
  )
})

test_that("validate_schema allows extra columns in delivery report", {
  # Extra column should be fine
  data_with_extra <- data.frame(
    name = c("Site"),
    value_as_string = c("Test Site"),
    value_as_number = c(NA),
    extra_column = c("Extra data"),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(data_with_extra, .RAW_DELIVERY_REPORT_FILE))
})

# ==============================================================================
# validate_schema() - DQD File
# ==============================================================================

test_that("validate_schema accepts valid DQD schema", {
  valid_data <- data.frame(
    checkName = c("isPlausibleValueLow"),
    cdmTableName = c("PERSON"),
    failed = c(0),
    context = c("Validation"),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(valid_data, .DQD_FILE))
})

test_that("validate_schema detects missing columns in DQD file", {
  # Missing failed column
  invalid_data <- data.frame(
    checkName = c("isPlausibleValueLow"),
    cdmTableName = c("PERSON"),
    context = c("Validation"),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_schema(invalid_data, .DQD_FILE),
    "missing required columns: failed"
  )
})

test_that("validate_schema detects multiple missing columns in DQD file", {
  # Missing checkName and failed
  invalid_data <- data.frame(
    cdmTableName = c("PERSON"),
    context = c("Validation"),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_schema(invalid_data, .DQD_FILE),
    "missing required columns"
  )

  # Should mention both
  expect_error(
    validate_schema(invalid_data, .DQD_FILE),
    "checkName"
  )

  expect_error(
    validate_schema(invalid_data, .DQD_FILE),
    "failed"
  )
})

test_that("validate_schema allows extra columns in DQD file", {
  # Extra column should be fine
  data_with_extra <- data.frame(
    checkName = c("isPlausibleValueLow"),
    cdmTableName = c("PERSON"),
    failed = c(0),
    context = c("Validation"),
    extra_column = c("Extra data"),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(data_with_extra, .DQD_FILE))
})

# ==============================================================================
# validate_schema() - Error Handling
# ==============================================================================

test_that("validate_schema errors on unknown file type", {
  data <- data.frame(
    col1 = c("value"),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_schema(data, "unknown file type"),
    "Unknown results file type: unknown file type"
  )
})

# ==============================================================================
# check_data_availability()
# ==============================================================================

test_that("check_data_availability detects both files present", {
  delivery_data <- data.frame(name = "Site", value_as_string = "Test", value_as_number = NA)
  dqd_data <- data.frame(checkName = "check1", cdmTableName = "PERSON", failed = 0, context = "Validation")

  result <- check_data_availability(delivery_data, dqd_data)

  expect_true(result$has_delivery_data)
  expect_true(result$has_dqd_data)
})

test_that("check_data_availability detects delivery data missing", {
  dqd_data <- data.frame(checkName = "check1", cdmTableName = "PERSON", failed = 0, context = "Validation")

  result <- check_data_availability(NULL, dqd_data)

  expect_false(result$has_delivery_data)
  expect_true(result$has_dqd_data)
})

test_that("check_data_availability detects DQD data missing", {
  delivery_data <- data.frame(name = "Site", value_as_string = "Test", value_as_number = NA)

  result <- check_data_availability(delivery_data, NULL)

  expect_true(result$has_delivery_data)
  expect_false(result$has_dqd_data)
})

test_that("check_data_availability detects both files missing", {
  result <- check_data_availability(NULL, NULL)

  expect_false(result$has_delivery_data)
  expect_false(result$has_dqd_data)
})

# ==============================================================================
# Integration Tests with Real Data
# ==============================================================================

test_that("validate_schema works with real delivery report structure", {
  # Simulate structure from inst/ref/delivery_report.csv
  real_delivery_data <- data.frame(
    name = c(
      "Site",
      "Delivery date",
      "Valid table name: person",
      "Valid row count: person",
      "Invalid row count: person",
      "Final row count: person"
    ),
    value_as_string = c(
      "Test Site",
      "01/15/24",
      "",
      "",
      "",
      ""
    ),
    value_as_number = c(
      NA,
      NA,
      NA,
      1000,
      50,
      950
    ),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(real_delivery_data, .RAW_DELIVERY_REPORT_FILE))
})

test_that("validate_schema works with real DQD structure", {
  # Simulate structure from inst/ref/dqd_results.csv
  real_dqd_data <- data.frame(
    checkName = c(
      "isPlausibleValueLow",
      "isPlausibleValueHigh",
      "measurePersonCompleteness"
    ),
    cdmTableName = c("PERSON", "PERSON", "VISIT_OCCURRENCE"),
    failed = c(0, 0, 5),
    context = c("Validation", "Validation", "Verification"),
    # Real DQD files have many more columns
    checkLevel = c("FIELD", "FIELD", "TABLE"),
    cdmFieldName = c("year_of_birth", "year_of_birth", NA),
    stringsAsFactors = FALSE
  )

  expect_true(validate_schema(real_dqd_data, .DQD_FILE))
})

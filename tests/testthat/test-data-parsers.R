# ==============================================================================
# Tests for Data Parsing Functions (data_parsers.R)
# ==============================================================================

library(testthat)
library(dplyr)

# ==============================================================================
# group_type_concepts()
# ==============================================================================

test_that("group_type_concepts categorizes EHR correctly", {
  type_concepts <- data.frame(
    table_name = c("condition_occurrence", "drug_exposure", "measurement"),
    type_concept = c("EHR", "EHR episode entry", "EHR order list entry"),
    count = c(100, 50, 25),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, c("EHR", "EHR", "EHR"))
})

test_that("group_type_concepts is case-sensitive for EHR", {
  type_concepts <- data.frame(
    table_name = c("condition_occurrence", "drug_exposure"),
    type_concept = c("EHR", "ehr"),  # lowercase should not match
    count = c(100, 50),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group[1], "EHR")
  expect_equal(result$type_group[2], "Other")  # lowercase "ehr" -> Other
})

test_that("group_type_concepts categorizes Claims correctly", {
  type_concepts <- data.frame(
    table_name = rep("drug_exposure", 4),
    type_concept = c(
      "Pharmacy claim",
      "inpatient claim",
      "CLAIM",
      "Payer System Record"
    ),
    count = c(100, 50, 25, 10),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Claims", 4))
})

test_that("group_type_concepts is case-insensitive for Claims", {
  type_concepts <- data.frame(
    table_name = rep("drug_exposure", 3),
    type_concept = c("claim", "Claim", "CLAIM"),
    count = c(100, 50, 25),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Claims", 3))
})

test_that("group_type_concepts categorizes Disease registry correctly", {
  type_concepts <- data.frame(
    table_name = rep("condition_occurrence", 2),
    type_concept = c("Registry", "Tumor Registry"),
    count = c(100, 50),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Disease registry", 2))
})

test_that("group_type_concepts categorizes Patient reported correctly", {
  type_concepts <- data.frame(
    table_name = rep("observation", 5),
    type_concept = c(
      "Patient self-report",
      "Patient self-tested",
      "Patient filled survey",
      "Survey",
      "Patient Self-Reported Medication"
    ),
    count = rep(50, 5),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Patient reported", 5))
})

test_that("group_type_concepts categorizes Unlabeled correctly", {
  type_concepts <- data.frame(
    table_name = rep("measurement", 4),
    type_concept = c(NA, "", "0", "No matching concept"),
    count = rep(50, 4),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Unlabeled", 4))
})

test_that("group_type_concepts is case-insensitive for Unlabeled", {
  type_concepts <- data.frame(
    table_name = rep("measurement", 3),
    type_concept = c("no matching concept", "NO MATCHING CONCEPT", "No Matching Concept"),
    count = rep(50, 3),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Unlabeled", 3))
})

test_that("group_type_concepts categorizes Other correctly", {
  type_concepts <- data.frame(
    table_name = rep("observation", 3),
    type_concept = c(
      "Lab result",
      "Unknown type",
      "Custom type concept"
    ),
    count = rep(50, 3),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_equal(result$type_group, rep("Other", 3))
})

test_that("group_type_concepts handles mixed categories", {
  type_concepts <- data.frame(
    table_name = rep("condition_occurrence", 7),
    type_concept = c(
      "EHR",
      "Pharmacy claim",
      "Registry",
      "Patient self-report",
      "",
      "Other type",
      "No matching concept"
    ),
    count = rep(50, 7),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expected_groups <- c(
    "EHR",
    "Claims",
    "Disease registry",
    "Patient reported",
    "Unlabeled",
    "Other",
    "Unlabeled"
  )

  expect_equal(result$type_group, expected_groups)
})

test_that("group_type_concepts preserves original columns", {
  type_concepts <- data.frame(
    table_name = c("condition_occurrence", "drug_exposure"),
    type_concept = c("EHR", "Pharmacy claim"),
    count = c(100, 50),
    stringsAsFactors = FALSE
  )

  result <- group_type_concepts(type_concepts)

  expect_true("table_name" %in% colnames(result))
  expect_true("type_concept" %in% colnames(result))
  expect_true("count" %in% colnames(result))
  expect_true("type_group" %in% colnames(result))

  expect_equal(result$table_name, type_concepts$table_name)
  expect_equal(result$type_concept, type_concepts$type_concept)
  expect_equal(result$count, type_concepts$count)
})

# ==============================================================================
# parse_metric() - Generic Parser
# ==============================================================================

test_that("parse_metric extracts simple patterns correctly", {
  delivery_data <- data.frame(
    name = c(
      "Valid table name: person",
      "Valid table name: visit_occurrence",
      "Invalid table name: bad_table"
    ),
    value_as_string = c("", "", ""),
    value_as_number = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Valid table name:",
    regex = "Valid table name: (\\w+)",
    fields = c("table_name")
  )

  result <- parse_metric(delivery_data, parser_config)

  expect_equal(nrow(result), 2)
  expect_equal(result$table_name, c("person", "visit_occurrence"))
})

test_that("parse_metric extracts numeric values correctly", {
  delivery_data <- data.frame(
    name = c(
      "Valid row count: person",
      "Valid row count: visit_occurrence"
    ),
    value_as_string = c("", ""),
    value_as_number = c(1000, 5000),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Valid row count:",
    regex = "Valid row count: (\\w+)",
    fields = c("table_name"),
    value_field = "count"
  )

  result <- parse_metric(delivery_data, parser_config)

  expect_equal(nrow(result), 2)
  expect_equal(result$table_name, c("person", "visit_occurrence"))
  expect_equal(result$count, c(1000, 5000))
})

test_that("parse_metric extracts string values correctly", {
  delivery_data <- data.frame(
    name = c(
      "Type concept breakdown: drug_exposure",
      "Type concept breakdown: condition_occurrence"
    ),
    value_as_string = c("EHR", "Pharmacy claim"),
    value_as_number = c(100, 50),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Type concept breakdown:",
    regex = "Type concept breakdown: (\\w+)",
    fields = c("table_name"),
    string_field = "type_concept",
    value_field = "count"
  )

  result <- parse_metric(delivery_data, parser_config)

  expect_equal(nrow(result), 2)
  expect_equal(result$table_name, c("drug_exposure", "condition_occurrence"))
  expect_equal(result$type_concept, c("EHR", "Pharmacy claim"))
  expect_equal(result$count, c(100, 50))
})

test_that("parse_metric handles multiple captured groups", {
  delivery_data <- data.frame(
    name = c(
      "Valid column name: person.person_id",
      "Valid column name: visit_occurrence.visit_start_date"
    ),
    value_as_string = c("", ""),
    value_as_number = c(NA, NA),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Valid column name:",
    regex = "Valid column name: (\\w+)\\.(\\w+)",
    fields = c("table_name", "column_name")
  )

  result <- parse_metric(delivery_data, parser_config)

  expect_equal(nrow(result), 2)
  expect_equal(result$table_name, c("person", "visit_occurrence"))
  expect_equal(result$column_name, c("person_id", "visit_start_date"))
})

test_that("parse_metric returns empty data frame when no matches", {
  delivery_data <- data.frame(
    name = c("Other metric: something"),
    value_as_string = c(""),
    value_as_number = c(NA),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Valid table name:",
    regex = "Valid table name: (\\w+)",
    fields = c("table_name")
  )

  result <- parse_metric(delivery_data, parser_config)

  expect_equal(nrow(result), 0)
  expect_true("table_name" %in% colnames(result))
})

test_that("parse_metric applies post-processing function", {
  delivery_data <- data.frame(
    name = c(
      "Valid table name: person",
      "Valid table name: person",  # duplicate
      "Valid table name: visit_occurrence"
    ),
    value_as_string = c("", "", ""),
    value_as_number = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  parser_config <- list(
    pattern = "^Valid table name:",
    regex = "Valid table name: (\\w+)",
    fields = c("table_name"),
    post_process = function(df) df |> dplyr::distinct()
  )

  result <- parse_metric(delivery_data, parser_config)

  # Should remove duplicate
  expect_equal(nrow(result), 2)
  expect_equal(result$table_name, c("person", "visit_occurrence"))
})

# ==============================================================================
# create_empty_metric_df()
# ==============================================================================

test_that("create_empty_metric_df creates correct structure", {
  parser_config <- list(
    fields = c("table_name"),
    value_field = "count"
  )

  result <- create_empty_metric_df(parser_config)

  expect_equal(nrow(result), 0)
  expect_true("table_name" %in% colnames(result))
  expect_true("count" %in% colnames(result))
  expect_type(result$table_name, "character")
  expect_type(result$count, "double")
})

test_that("create_empty_metric_df handles multiple fields", {
  parser_config <- list(
    fields = c("table_name", "column_name"),
    value_field = "count"
  )

  result <- create_empty_metric_df(parser_config)

  expect_equal(nrow(result), 0)
  expect_true("table_name" %in% colnames(result))
  expect_true("column_name" %in% colnames(result))
  expect_true("count" %in% colnames(result))
})

test_that("create_empty_metric_df handles year field as integer", {
  parser_config <- list(
    fields = c("table_name", "year"),
    value_field = "count"
  )

  result <- create_empty_metric_df(parser_config)

  expect_equal(nrow(result), 0)
  expect_type(result$year, "integer")
})

# ==============================================================================
# format_date_safe()
# ==============================================================================

test_that("format_date_safe handles MM/DD/YY format", {
  expect_equal(format_date_safe("01/15/23"), "2023-01-15")
  expect_equal(format_date_safe("12/31/24"), "2024-12-31")
})

test_that("format_date_safe handles YYYY-MM-DD format", {
  expect_equal(format_date_safe("2023-01-15"), "2023-01-15")
  expect_equal(format_date_safe("2024-12-31"), "2024-12-31")
})

test_that("format_date_safe returns NA on parse error", {
  # Invalid date strings return NA when they can't be parsed
  expect_true(is.na(format_date_safe("not-a-date")))
  expect_true(is.na(format_date_safe("invalid")))
})

# ==============================================================================
# create_empty_metrics()
# ==============================================================================

test_that("create_empty_metrics returns complete structure", {
  result <- create_empty_metrics()

  expect_type(result, "list")

  # Check for key components
  expect_true("metadata" %in% names(result))
  expect_true("valid_tables" %in% names(result))
  expect_true("valid_row_counts" %in% names(result))
  expect_true("final_row_counts" %in% names(result))
  expect_true("type_concepts" %in% names(result))
  expect_true("time_series" %in% names(result))

  # Metadata should have all fields
  expect_true("site" %in% names(result$metadata))
  expect_true("delivery_date" %in% names(result$metadata))

  # Data frames should be empty but have correct structure
  expect_equal(nrow(result$valid_tables), 0)
  expect_equal(nrow(result$valid_row_counts), 0)
})

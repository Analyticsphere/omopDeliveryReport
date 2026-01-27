# ==============================================================================
# Tests for PASS Integration Functions
# ==============================================================================

library(testthat)

# ==============================================================================
# get_pass_score_class()
# ==============================================================================

test_that("get_pass_score_class returns correct classes", {
  # Excellent scores (>= 0.90)
  expect_equal(get_pass_score_class(1.0), "excellent")
  expect_equal(get_pass_score_class(0.90), "excellent")
  expect_equal(get_pass_score_class(0.95), "excellent")

  # Good scores (0.80-0.89)
  expect_equal(get_pass_score_class(0.80), "good")
  expect_equal(get_pass_score_class(0.85), "good")
  expect_equal(get_pass_score_class(0.89), "good")

  # Moderate scores (0.60-0.79)
  expect_equal(get_pass_score_class(0.60), "moderate")
  expect_equal(get_pass_score_class(0.70), "moderate")
  expect_equal(get_pass_score_class(0.79), "moderate")

  # Poor scores (0.40-0.59)
  expect_equal(get_pass_score_class(0.40), "poor")
  expect_equal(get_pass_score_class(0.50), "poor")
  expect_equal(get_pass_score_class(0.59), "poor")

  # Very poor scores (< 0.40)
  expect_equal(get_pass_score_class(0.39), "verypoor")
  expect_equal(get_pass_score_class(0.20), "verypoor")
  expect_equal(get_pass_score_class(0.0), "verypoor")

  # NA
  expect_equal(get_pass_score_class(NA), "neutral")
})

test_that("get_pass_score_class handles boundary values", {
  expect_equal(get_pass_score_class(0.89999), "good")
  expect_equal(get_pass_score_class(0.90001), "excellent")
  expect_equal(get_pass_score_class(0.79999), "moderate")
  expect_equal(get_pass_score_class(0.80001), "good")
  expect_equal(get_pass_score_class(0.59999), "poor")  # Below 0.60 threshold
  expect_equal(get_pass_score_class(0.60001), "moderate")
  expect_equal(get_pass_score_class(0.39999), "verypoor")
  expect_equal(get_pass_score_class(0.40001), "poor")
})

# ==============================================================================
# format_pass_score_display()
# ==============================================================================

test_that("format_pass_score_display formats scores correctly", {
  expect_equal(format_pass_score_display(0.75, decimals = 2), "0.75")
  expect_equal(format_pass_score_display(0.9, decimals = 2), "0.90")
  expect_equal(format_pass_score_display(1.0, decimals = 2), "1.00")
  expect_equal(format_pass_score_display(0.123456, decimals = 2), "0.12")
})

test_that("format_pass_score_display handles different decimal places", {
  expect_equal(format_pass_score_display(0.123456, decimals = 0), "0")
  expect_equal(format_pass_score_display(0.123456, decimals = 1), "0.1")
  expect_equal(format_pass_score_display(0.123456, decimals = 3), "0.123")
  expect_equal(format_pass_score_display(0.123456, decimals = 4), "0.1235")
})

test_that("format_pass_score_display includes confidence intervals when provided", {
  result <- format_pass_score_display(0.75, ci_lower = 0.70, ci_upper = 0.80, decimals = 2)
  expect_equal(result, "0.75 (0.70-0.80)")

  result <- format_pass_score_display(0.9, ci_lower = 0.85, ci_upper = 0.95, decimals = 2)
  expect_equal(result, "0.90 (0.85-0.95)")
})

test_that("format_pass_score_display handles NA values", {
  expect_equal(format_pass_score_display(NA), "N/A")

  # NA CI values should be ignored
  result <- format_pass_score_display(0.75, ci_lower = NA, ci_upper = 0.80, decimals = 2)
  expect_equal(result, "0.75")

  result <- format_pass_score_display(0.75, ci_lower = 0.70, ci_upper = NA, decimals = 2)
  expect_equal(result, "0.75")
})

test_that("format_pass_score_display handles NULL CI values", {
  result <- format_pass_score_display(0.75, ci_lower = NULL, ci_upper = NULL, decimals = 2)
  expect_equal(result, "0.75")

  result <- format_pass_score_display(0.75, ci_lower = NULL, ci_upper = 0.80, decimals = 2)
  expect_equal(result, "0.75")
})

# ==============================================================================
# calculate_overall_pass_score()
# ==============================================================================

test_that("calculate_overall_pass_score extracts scores correctly", {
  pass_data <- list(
    overall = data.frame(
      composite_score = 0.85,
      ci_95_lower = 0.80,
      ci_95_upper = 0.90
    )
  )

  result <- calculate_overall_pass_score(pass_data)
  expect_equal(result$overall_score, 0.85)
  expect_equal(result$ci_lower, 0.80)
  expect_equal(result$ci_upper, 0.90)
})

test_that("calculate_overall_pass_score handles NULL pass_data", {
  result <- calculate_overall_pass_score(NULL)
  expect_true(is.na(result$overall_score))
  expect_true(is.na(result$ci_lower))
  expect_true(is.na(result$ci_upper))
})

test_that("calculate_overall_pass_score handles missing overall data", {
  pass_data <- list(components = data.frame(metric = "test"))
  result <- calculate_overall_pass_score(pass_data)
  expect_true(is.na(result$overall_score))
  expect_true(is.na(result$ci_lower))
  expect_true(is.na(result$ci_upper))
})

# ==============================================================================
# create_empty_pass_scores()
# ==============================================================================

test_that("create_empty_pass_scores returns correct structure", {
  result <- create_empty_pass_scores()

  expect_true(is.list(result))
  expect_equal(names(result), c("overall_score", "ci_lower", "ci_upper", "components"))
  expect_true(is.na(result$overall_score))
  expect_true(is.na(result$ci_lower))
  expect_true(is.na(result$ci_upper))
  expect_true(is.data.frame(result$components))
  expect_equal(nrow(result$components), 0)
})

# ==============================================================================
# parse_pass_components()
# ==============================================================================

test_that("parse_pass_components processes components correctly", {
  pass_data <- list(
    components = data.frame(
      metric = c("accessibility", "provenance", "standards", "concept_diversity", "source_diversity", "temporal"),
      score = c(0.90, 0.85, 0.80, 0.75, 0.70, 0.65),
      weight = c(0.20, 0.20, 0.15, 0.15, 0.15, 0.15),
      weighted_contribution = c(0.18, 0.17, 0.12, 0.11, 0.10, 0.10),
      percent_contribution = c(22.5, 21.25, 15.0, 13.75, 12.5, 12.5),
      stringsAsFactors = FALSE
    )
  )

  result <- parse_pass_components(pass_data)

  # Should be sorted by weighted_contribution descending
  expect_equal(result$metric[1], "accessibility")
  expect_equal(result$metric[nrow(result)], "temporal")

  # Should have descriptions added
  expect_true("description" %in% names(result))
  expect_false(any(is.na(result$description)))
  expect_true(all(nchar(result$description) > 0))
})

test_that("parse_pass_components adds correct descriptions", {
  pass_data <- list(
    components = data.frame(
      metric = c("accessibility", "provenance"),
      score = c(0.90, 0.85),
      weight = c(0.50, 0.50),
      weighted_contribution = c(0.45, 0.42),
      percent_contribution = c(51.7, 48.3),
      stringsAsFactors = FALSE
    )
  )

  result <- parse_pass_components(pass_data)

  expect_match(result$description[result$metric == "accessibility"], "present and discoverable")
  expect_match(result$description[result$metric == "provenance"], "coding quality and traceability")
})

# ==============================================================================
# extract_pass_table_scores()
# ==============================================================================

test_that("extract_pass_table_scores extracts table scores correctly", {
  pass_data <- list(
    table_scores = data.frame(
      table_name = c("person", "visit_occurrence", "condition_occurrence"),
      pass_score = c(0.85, 0.90, 0.75),
      stringsAsFactors = FALSE
    )
  )

  result <- extract_pass_table_scores(pass_data)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_equal(result$person, 0.85)
  expect_equal(result$visit_occurrence, 0.90)
  expect_equal(result$condition_occurrence, 0.75)
})

test_that("extract_pass_table_scores handles NULL pass_data", {
  result <- extract_pass_table_scores(NULL)
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

test_that("extract_pass_table_scores handles missing table_scores", {
  pass_data <- list(overall = data.frame(composite_score = 0.85))
  result <- extract_pass_table_scores(pass_data)
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

test_that("extract_pass_table_scores handles empty table_scores", {
  pass_data <- list(
    table_scores = data.frame(
      table_name = character(0),
      pass_score = numeric(0)
    )
  )
  result <- extract_pass_table_scores(pass_data)
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

# ==============================================================================
# load_pass_results() - Integration tests with mock data
# ==============================================================================

test_that("load_pass_results handles NULL path", {
  result <- load_pass_results(NULL)
  expect_null(result)
})

test_that("load_pass_results handles empty path", {
  result <- load_pass_results("")
  expect_null(result)
})

test_that("load_pass_results adds trailing slash if missing", {
  # Create a temporary directory with test files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create minimal test files
  overall_data <- data.frame(
    composite_score = 0.85,
    standard_error = 0.02,
    ci_95_lower = 0.81,
    ci_95_upper = 0.89
  )
  write.csv(overall_data, file.path(temp_dir, "pass_composite_overall.csv"), row.names = FALSE)

  components_data <- data.frame(
    metric = c("accessibility", "provenance"),
    score = c(0.90, 0.80),
    weight = c(0.50, 0.50),
    weighted_contribution = c(0.45, 0.40),
    percent_contribution = c(52.9, 47.1)
  )
  write.csv(components_data, file.path(temp_dir, "pass_composite_components.csv"), row.names = FALSE)

  table_data <- data.frame(
    table_name = c("person"),
    table_accessibility_score = c(0.85)
  )
  write.csv(table_data, file.path(temp_dir, "pass_accessibility_table_level.csv"), row.names = FALSE)

  # Test with and without trailing slash
  result_with_slash <- load_pass_results(paste0(temp_dir, "/"))
  result_without_slash <- load_pass_results(temp_dir)

  expect_equal(result_with_slash$overall$composite_score, 0.85)
  expect_equal(result_without_slash$overall$composite_score, 0.85)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("load_pass_results loads all components correctly", {
  # Create a temporary directory with complete test files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create overall file
  overall_data <- data.frame(
    composite_score = 0.87,
    standard_error = 0.03,
    ci_95_lower = 0.81,
    ci_95_upper = 0.93
  )
  write.csv(overall_data, file.path(temp_dir, "pass_composite_overall.csv"), row.names = FALSE)

  # Create components file
  components_data <- data.frame(
    metric = c("accessibility", "provenance", "standards"),
    score = c(0.90, 0.85, 0.80),
    weight = c(0.33, 0.33, 0.34),
    weighted_contribution = c(0.30, 0.28, 0.27),
    percent_contribution = c(35.3, 32.9, 31.8)
  )
  write.csv(components_data, file.path(temp_dir, "pass_composite_components.csv"), row.names = FALSE)

  # Create table-level file
  table_data <- data.frame(
    table_name = c("person", "visit_occurrence"),
    table_accessibility_score = c(0.85, 0.90)
  )
  write.csv(table_data, file.path(temp_dir, "pass_accessibility_table_level.csv"), row.names = FALSE)

  # Load and test
  result <- load_pass_results(temp_dir)

  expect_false(is.null(result))
  expect_true(is.list(result))
  expect_equal(names(result), c("overall", "components", "table_scores"))

  # Check overall
  expect_equal(result$overall$composite_score, 0.87)
  expect_equal(result$overall$ci_95_lower, 0.81)
  expect_equal(result$overall$ci_95_upper, 0.93)

  # Check components
  expect_equal(nrow(result$components), 3)
  expect_equal(result$components$metric, c("accessibility", "provenance", "standards"))

  # Check table scores
  expect_equal(nrow(result$table_scores), 2)
  expect_equal(result$table_scores$table_name, c("person", "visit_occurrence"))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

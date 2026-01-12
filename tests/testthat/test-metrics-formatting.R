# ==============================================================================
# Tests for Formatting Functions (metrics.R)
# ==============================================================================

library(testthat)

# ==============================================================================
# format_number()
# ==============================================================================

test_that("format_number adds thousands separators", {
  expect_equal(format_number(1000), "1,000")
  expect_equal(format_number(1234567), "1,234,567")
  expect_equal(format_number(100), "100")
  expect_equal(format_number(10), "10")
  expect_equal(format_number(0), "0")
})

test_that("format_number handles edge cases", {
  expect_equal(format_number(NA), "0")
  expect_equal(format_number(NULL), "0")
})

test_that("format_number handles large numbers", {
  expect_equal(format_number(1000000), "1,000,000")
  expect_equal(format_number(999999999), "999,999,999")
})

test_that("format_number preserves negative signs", {
  expect_equal(format_number(-1000), "-1,000")
  expect_equal(format_number(-1234567), "-1,234,567")
})

# ==============================================================================
# format_percentage()
# ==============================================================================

test_that("format_percentage calculates and formats correctly", {
  expect_equal(format_percentage(25, 100), "25.0")
  expect_equal(format_percentage(1, 3), "33.3")
  expect_equal(format_percentage(50, 100), "50.0")
  expect_equal(format_percentage(0, 100), "0.0")
})

test_that("format_percentage handles decimal places", {
  expect_equal(format_percentage(1, 3, decimal_places = 0), "33")
  expect_equal(format_percentage(1, 3, decimal_places = 1), "33.3")
  expect_equal(format_percentage(1, 3, decimal_places = 2), "33.33")
})

test_that("format_percentage handles edge cases", {
  # Division by zero
  expect_equal(format_percentage(10, 0), "0.0")

  # NA values
  expect_equal(format_percentage(NA, 100), "0.0")
  expect_equal(format_percentage(10, NA), "0.0")
})

# ==============================================================================
# get_dqd_score_class()
# ==============================================================================

test_that("get_dqd_score_class returns correct classes", {
  # Good scores (>= 95%)
  expect_equal(get_dqd_score_class(100), "good")
  expect_equal(get_dqd_score_class(95), "good")
  expect_equal(get_dqd_score_class(99.5), "good")

  # Fair scores (85-94%)
  expect_equal(get_dqd_score_class(85), "fair")
  expect_equal(get_dqd_score_class(90), "fair")
  expect_equal(get_dqd_score_class(94.9), "fair")

  # Poor scores (< 85%)
  expect_equal(get_dqd_score_class(84), "poor")
  expect_equal(get_dqd_score_class(50), "poor")
  expect_equal(get_dqd_score_class(0), "poor")

  # NA
  expect_equal(get_dqd_score_class(NA), "neutral")
})

test_that("get_dqd_score_class handles boundary values", {
  expect_equal(get_dqd_score_class(94.99999), "fair")
  expect_equal(get_dqd_score_class(95.00001), "good")
  expect_equal(get_dqd_score_class(84.99999), "poor")
  expect_equal(get_dqd_score_class(85.00001), "fair")
})

# ==============================================================================
# get_status_badge()
# ==============================================================================

test_that("get_status_badge returns correct status for delivered tables", {
  result <- get_status_badge(TRUE)
  expect_equal(result$text, "Delivered")
  expect_equal(result$class, "delivered")
})

test_that("get_status_badge returns correct status for not delivered tables", {
  result <- get_status_badge(FALSE)
  expect_equal(result$text, "Not Delivered")
  expect_equal(result$class, "not-delivered")
})

# ==============================================================================
# get_metric_class()
# ==============================================================================

test_that("get_metric_class works when zero is good", {
  # Zero is good (default)
  expect_equal(get_metric_class(0, zero_is_good = TRUE), "success")
  expect_equal(get_metric_class(1, zero_is_good = TRUE), "warning")
  expect_equal(get_metric_class(100, zero_is_good = TRUE), "warning")
})

test_that("get_metric_class works when zero is bad", {
  # Zero is bad (more is better)
  expect_equal(get_metric_class(0, zero_is_good = FALSE), "neutral")
  expect_equal(get_metric_class(1, zero_is_good = FALSE), "success")
  expect_equal(get_metric_class(100, zero_is_good = FALSE), "success")
})

test_that("get_metric_class handles NA", {
  expect_equal(get_metric_class(NA, zero_is_good = TRUE), "neutral")
  expect_equal(get_metric_class(NA, zero_is_good = FALSE), "neutral")
})

# ==============================================================================
# format_harmonization_display()
# ==============================================================================

test_that("format_harmonization_display handles positive harmonization", {
  result <- format_harmonization_display(100, TRUE)
  expect_equal(result$text, "+100")
  expect_equal(result$class, "harmonization-positive")

  result <- format_harmonization_display(1500, TRUE)
  expect_equal(result$text, "+1,500")
  expect_equal(result$class, "harmonization-positive")
})

test_that("format_harmonization_display handles negative harmonization", {
  result <- format_harmonization_display(-100, TRUE)
  expect_equal(result$text, "-100")
  expect_equal(result$class, "harmonization-negative")

  result <- format_harmonization_display(-1500, TRUE)
  expect_equal(result$text, "-1,500")
  expect_equal(result$class, "harmonization-negative")
})

test_that("format_harmonization_display handles zero harmonization", {
  result <- format_harmonization_display(0, TRUE)
  expect_equal(result$text, "0")
  expect_equal(result$class, "harmonization-neutral")
})

test_that("format_harmonization_display handles non-harmonized tables", {
  result <- format_harmonization_display(100, FALSE)
  expect_equal(result$text, "--")
  expect_equal(result$class, "harmonization-neutral")

  # Should ignore the value for non-harmonized tables
  result <- format_harmonization_display(-100, FALSE)
  expect_equal(result$text, "--")
  expect_equal(result$class, "harmonization-neutral")
})

# ==============================================================================
# format_quality_issues_display()
# ==============================================================================

test_that("format_quality_issues_display adds minus sign for issues", {
  result <- format_quality_issues_display(100)
  expect_equal(result$text, "-100")
  expect_equal(result$class, "harmonization-negative")

  result <- format_quality_issues_display(1500)
  expect_equal(result$text, "-1,500")
  expect_equal(result$class, "harmonization-negative")
})

test_that("format_quality_issues_display handles zero issues", {
  result <- format_quality_issues_display(0)
  expect_equal(result$text, "0")
  expect_equal(result$class, "harmonization-neutral")
})

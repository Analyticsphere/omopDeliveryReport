# ==============================================================================
# Tests for Connect Participant Filtering
# ==============================================================================

library(testthat)

test_that("parse_delivery_metrics extracts Connect participant filtering metrics", {
  delivery_data <- data.frame(
    name = c(
      "Connect participant breakdown: Consent withdrawn status (No)",
      "Connect participant breakdown: Consent withdrawn status (Yes)",
      "Connect participant breakdown: Data destruction status (No)",
      "Connect participant breakdown: Data destruction status (Yes)",
      "Connect participant breakdown: HIPAA revoked status (No)",
      "Connect participant breakdown: HIPAA revoked status (Yes)",
      "Connect participant breakdown: Study status (Verified)",
      "Connect participant breakdown: Study status (Not Yet Verified)",
      "Connect participant breakdown: Study status (Duplicate)",
      "Number of rows removed due to Connect exclusion rules: person",
      "Number of Connect patients not in delivery",
      "Number of delivery patients not in Connect data"
    ),
    value_as_string = rep("", 12),
    value_as_number = c(93, 15, 98, 10, 92, 16, 79, 26, 3, 44, 561, 2),
    stringsAsFactors = FALSE
  )

  metrics <- parse_delivery_metrics(delivery_data)

  expect_equal(nrow(metrics$connect_participant_breakdowns), 9)
  expect_equal(get_table_count(metrics$connect_exclusion_rule_rows, "person"), 44)
  expect_equal(metrics$excluded_participants_count, 44)
  expect_equal(
    unique(metrics$connect_participant_breakdowns$breakdown_type),
    c(
      "Consent withdrawn status",
      "Data destruction status",
      "HIPAA revoked status",
      "Study status"
    )
  )
  expect_equal(metrics$connect_patient_counts$connect_not_in_delivery, 561)
  expect_equal(metrics$connect_patient_counts$delivery_not_in_connect, 2)
})

test_that("parse_delivery_metrics supports legacy delivery not in Connect metric name", {
  delivery_data <- data.frame(
    name = "Delivery patient IDs not in Connect data",
    value_as_string = "",
    value_as_number = 4,
    stringsAsFactors = FALSE
  )

  metrics <- parse_delivery_metrics(delivery_data)

  expect_equal(metrics$connect_patient_counts$delivery_not_in_connect, 4)
})

test_that("prepare_connect_filtering_data formats summary counts and orders rows", {
  metrics <- list(
    missing_person_id_count = 1,
    excluded_participants_count = 44,
    connect_participant_breakdowns = data.frame(
      breakdown_type = c(
        "Study status",
        "Study status",
        "Study status",
        "Consent withdrawn status",
        "Consent withdrawn status"
      ),
      status = c("Duplicate", "Verified", "Not Yet Verified", "Yes", "No"),
      count = c(3, 79, 26, 15, 93),
      stringsAsFactors = FALSE
    ),
    connect_exclusion_rule_rows = data.frame(
      table_name = c("person", "measurement"),
      count = c(44, 22471),
      stringsAsFactors = FALSE
    ),
    connect_patient_counts = list(
      connect_not_in_delivery = 561,
      delivery_not_in_connect = 2
    )
  )

  result <- prepare_connect_filtering_data(metrics)

  expect_true(result$has_data)
  expect_equal(result$missing_connect_id_display, "1")
  expect_equal(result$missing_connect_id_class, " warning")
  expect_equal(result$excluded_participants_display, "44")
  expect_equal(result$excluded_participants_class, " warning")
  expect_equal(result$connect_not_in_delivery_display, "561")
  expect_equal(result$connect_not_in_delivery_class, " warning")
  expect_equal(result$delivery_not_in_connect_display, "2")
  expect_equal(result$delivery_not_in_connect_class, " warning")

  expect_equal(
    vapply(result$consent_rows_data, `[[`, character(1), "status"),
    c("No", "Yes")
  )

  expect_equal(
    vapply(result$study_status_rows_data, `[[`, character(1), "status"),
    c("Verified", "Not Yet Verified", "Duplicate")
  )

  expect_equal(
    vapply(result$study_status_rows_data, `[[`, character(1), "percent_display"),
    c("73.1%", "24.1%", "2.8%")
  )
})

test_that("prepare_connect_filtering_data reports no data when metrics are absent", {
  metrics <- list(
    missing_person_id_count = NA_real_,
    excluded_participants_count = NA_real_,
    connect_participant_breakdowns = data.frame(
      breakdown_type = character(),
      status = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ),
    connect_exclusion_rule_rows = data.frame(
      table_name = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ),
    connect_patient_counts = list(
      connect_not_in_delivery = NA_real_,
      delivery_not_in_connect = NA_real_
    )
  )

  result <- prepare_connect_filtering_data(metrics)

  expect_false(result$has_data)
  expect_equal(result$missing_connect_id_display, "N/A")
  expect_equal(result$excluded_participants_display, "N/A")
  expect_equal(result$connect_not_in_delivery_display, "N/A")
  expect_equal(result$delivery_not_in_connect_display, "N/A")
})

test_that("prepare_overview_data no longer exposes overview filtering cards", {
  metrics <- list(
    valid_tables = data.frame(table_name = c("person", "measurement"), stringsAsFactors = FALSE),
    missing_person_id_count = 1,
    connect_patient_counts = list(
      connect_not_in_delivery = 561,
      delivery_not_in_connect = 2
    )
  )

  dqd_scores <- list(overall = NA_real_)
  pass_scores <- list(overall_score = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_)

  result <- prepare_overview_data(
    metrics = metrics,
    dqd_scores = dqd_scores,
    pass_scores = pass_scores,
    num_participants = 109,
    total_rows_removed = 999,
    has_delivery_data = TRUE,
    has_dqd_data = FALSE,
    has_pass_data = FALSE
  )

  expect_false("missing_person_display" %in% names(result))
  expect_false("delivery_not_in_connect_display" %in% names(result))
})

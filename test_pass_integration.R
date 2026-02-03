#!/usr/bin/env Rscript
# ==============================================================================
# End-to-End Integration Test: PASS Integration
# ==============================================================================
# Tests the complete report generation with real PASS data

library(omopDeliveryReport)

# Define input paths
delivery_report_path <- "inst/ref/delivery_report.csv"
dqd_results_path <- "inst/ref/dqd_results.csv"
pass_results_path <- "~/Development/pass/output/"
output_path <- "test_report_with_pass.html"

# Generate the report
cat("Generating OMOP delivery report with PASS integration...\n")
cat("  Delivery data:", delivery_report_path, "\n")
cat("  DQD data:", dqd_results_path, "\n")
cat("  PASS data:", pass_results_path, "\n")
cat("  Output:", output_path, "\n\n")

tryCatch({
  generate_omop_report(
    delivery_report_path = delivery_report_path,
    dqd_results_path = dqd_results_path,
    pass_results_path = pass_results_path,
    output_path = output_path
  )

  cat("\n✓ Report generated successfully!\n")
  cat("  Output file:", normalizePath(output_path), "\n")
  cat("  File size:", format(file.info(output_path)$size, big.mark = ","), "bytes\n")

  # Verify PASS data is in the HTML
  html_content <- readLines(output_path, warn = FALSE)
  html_text <- paste(html_content, collapse = "\n")

  # Check for PASS-related content
  checks <- list(
    "PASS section exists" = grepl("pass-breakdown", html_text, fixed = TRUE),
    "PASS score in overview" = grepl("PASS Quality Score", html_text, fixed = TRUE),
    "PASS full name displayed" = grepl("Profile of Analytic Suitability Score", html_text, fixed = TRUE),
    "PASS components data" = grepl("pass_components", html_text, fixed = TRUE),
    "PASS CSS classes" = grepl("pass-score", html_text, fixed = TRUE),
    "PASS JavaScript function" = grepl("getPASSClass", html_text, fixed = TRUE)
  )

  cat("\nContent verification:\n")
  for (check_name in names(checks)) {
    status <- if (checks[[check_name]]) "✓" else "✗"
    cat("  ", status, check_name, "\n")
  }

  if (all(unlist(checks))) {
    cat("\n✓ All PASS integration checks passed!\n")
  } else {
    cat("\n✗ Some PASS integration checks failed\n")
    quit(status = 1)
  }

}, error = function(e) {
  cat("\n✗ Error generating report:\n")
  cat("  ", conditionMessage(e), "\n")
  quit(status = 1)
})

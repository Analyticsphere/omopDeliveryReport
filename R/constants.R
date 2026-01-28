.RAW_DELIVERY_REPORT_FILE <- "delivery_report file"
.RAW_DELIVERY_REPORT_FILE_COLUMNS <- c(
  "name",
  "value_as_string",
  "value_as_number"
)

.DQD_FILE <- "dqd_results file"
.DQD_FILE_COLUMNS <- c(
  "checkName",
  "cdmTableName",
  "failed",
  "context"
)

.PASS_COMPOSITE_OVERALL_FILE <- "pass_composite_overall file"
.PASS_COMPOSITE_OVERALL_COLUMNS <- c(
  "composite_score",
  "standard_error",
  "ci_95_lower",
  "ci_95_upper",
  "n_metrics",
  "metrics_included",
  "weights_used"
)

.PASS_COMPOSITE_COMPONENTS_FILE <- "pass_composite_components file"
.PASS_COMPOSITE_COMPONENTS_COLUMNS <- c(
  "metric",
  "score",
  "standard_error",
  "weight",
  "weighted_contribution",
  "percent_contribution"
)

# Table-level PASS files (one per metric)
.PASS_TABLE_LEVEL_FILES <- list(
  accessibility = list(
    filename = "pass_accessibility_table_level.csv",
    score_column = "table_accessibility_score"
  ),
  provenance = list(
    filename = "pass_provenance_table_level.csv",
    score_column = "table_provenance_score"
  ),
  standards = list(
    filename = "pass_standards_table_level.csv",
    score_column = "table_standards_score"
  ),
  concept_diversity = list(
    filename = "pass_concept_diversity_table_level.csv",
    score_column = "table_diversity_score"
  ),
  source_diversity = list(
    filename = "pass_source_diversity_table_level.csv",
    score_column = "table_source_diversity_score"
  ),
  temporal = list(
    filename = "pass_temporal_table_level.csv",
    score_column = "temporal_score"
  )
)

# Metric-level PASS overall files (one per metric, contains CI bounds)
.PASS_METRIC_OVERALL_FILES <- list(
  accessibility = "pass_accessibility_overall.csv",
  provenance = "pass_provenance_overall.csv",
  standards = "pass_standards_overall.csv",
  concept_diversity = "pass_concept_diversity_overall.csv",
  source_diversity = "pass_source_diversity_overall.csv",
  temporal = "pass_temporal_overall.csv"
)

# PASS metric descriptions (one sentence each)
.PASS_METRIC_DESCRIPTIONS <- list(
  accessibility = "Measures whether clinical facts are present and discoverable in the dataset",
  provenance = "Measures coding quality and traceability to source data",
  standards = "Assesses the use of OHDSI standard concepts",
  concept_diversity = "Measures variety of clinical concepts using Shannon entropy",
  source_diversity = "Counts unique data source types",
  temporal = "Assesses temporal distribution including coverage, density, and consistency"
)

# Color scheme (colorblind-friendly)
.COLORS <- list(
  type_concepts = list(
    "EHR" = "#0073C2",
    "Claims" = "#EFC000",
    "Disease registry" = "#A95AA1",
    "Patient reported" = "#CD534C",
    "Unlabeled" = "#868686",
    "Other" = "#7AA6DC"
  ),
  tables = list(
    "condition_occurrence" = "#eab308",
    "device_exposure" = "#f87dba",
    "drug_exposure" = "#ff8d02",
    "measurement" = "#3b82f6",
    "visit_occurrence" = "#fa2b2b",
    "observation" = "#804ef5",
    "procedure_occurrence" = "#84cc16",
    "specimen" = "#1c1b1d",
    "note" = "#b5b6f5"
  ),
  dqd_scores = list(
    "good_threshold" = 95,    # >= 95% is "good"
    "fair_threshold" = 85,    # >= 85% is "fair", below is "poor"
    "good_color" = "#10b981",
    "fair_color" = "#f59e0b",
    "poor_color" = "#ef4444"
  ),
  pass_scores = list(
    "excellent_threshold" = 0.90,  # >= 0.90 is "excellent"
    "good_threshold" = 0.80,       # >= 0.80 is "good"
    "moderate_threshold" = 0.60,   # >= 0.60 is "moderate"
    "poor_threshold" = 0.40,       # >= 0.40 is "poor", below is "very poor"
    "excellent_color" = "#059669",  # emerald-600
    "good_color" = "#10b981",       # emerald-500
    "moderate_color" = "#f59e0b",   # amber-500
    "poor_color" = "#ef4444",       # red-500
    "verypoor_color" = "#991b1b"    # red-800
  )
)

# Table groupings
.TABLE_GROUPS <- list(
  "Clinical Data" = c(
    "person", "visit_occurrence", "visit_detail", "condition_occurrence",
    "drug_exposure", "procedure_occurrence", "device_exposure",
    "measurement", "observation", "death", "note", "specimen"
  ),
  "Health System" = c("location", "care_site", "provider"),
  "Healthcare Economics" = c("payer_plan_period", "cost"),
  "Derived Data" = c("observation_period", "drug_era", "condition_era", "dose_era"),
  "Vocabulary" = c(
    "concept", "vocabulary", "domain", "concept_class",
    "concept_relationship", "relationship", "concept_synonym",
    "concept_ancestor", "source_to_concept_map", "drug_strength"
  ),
  "Metadata" = c("metadata", "cdm_source"),
  "Other" = c(
    "note_nlp", "fact_relationship", "cohort", "cohort_definition",
    "episode", "episode_event", "attribute_definition"
  )
)

# Logic for grouping type concepts
.TYPE_CONCEPT_GROUPS <- list(
  ehr = list(
    patterns = "EHR",
    case_sensitive = TRUE
  ),
  claims = list(
    patterns = c("claim", "payer system record"),
    case_sensitive = FALSE
  ),
  disease_registry = list(
    exact_matches = c("Registry", "Tumor Registry"),
    case_sensitive = TRUE
  ),
  patient_reported = list(
    exact_matches = c(
      "Patient self-report", "Patient self-tested",
      "Patient filled survey", "Survey",
      "Patient Self-Reported Medication"
    ),
    case_sensitive = TRUE
  ),
  unlabeled = list(
    exact_matches = c("No matching concept", "0", ""),
    case_sensitive = FALSE
  )
)

.TYPE_CONCEPT_ORDER <- c(
  "EHR",
  "Claims",
  "Disease registry",
  "Patient reported",
  "Other",
  "Unlabeled"
)

.ALERT_THRESHOLDS <- list(
  default_dates_pct = 1,          # Show alert if >1% rows have default dates
  invalid_concepts_count = 0,     # Show alert if any rows have invalid concepts
  row_count_mismatch = TRUE       # Always show row count mismatch alerts
)

# Tables that participate in vocabulary harmonization (clinical data only)
.HARMONIZED_TABLES <- c(
  "visit_occurrence",
  "condition_occurrence",
  "drug_exposure",
  "procedure_occurrence",
  "device_exposure",
  "measurement",
  "observation",
  "note",
  "specimen"
)

# Canonical display order for tables (used in Sankey diagrams, etc.)
# Tables not in this list will appear alphabetically after these
.TABLE_ORDER <- c(
  "person",
  "visit_occurrence",
  "visit_detail",
  "condition_occurrence",
  "drug_exposure",
  "procedure_occurrence",
  "device_exposure",
  "measurement",
  "observation",
  "specimen",
  "note",
  "death"
)
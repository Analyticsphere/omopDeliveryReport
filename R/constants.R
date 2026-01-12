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
    "condition_occurrence" = "#ef4444",  # Red
    "device_exposure" = "#ec4899",       # Pink
    "drug_exposure" = "#3b82f6",         # Blue
    "measurement" = "#ff8d02",           # Orange
    "note" = "#6366f1",                  # Indigo
    "observation" = "#8b5cf6",           # Purple
    "procedure_occurrence" = "#84cc16",  # Lime Green
    "specimen" = "#96929f",              # Gray
    "visit_occurrence" = "#eab308"       # Yellow
  ),
  dqd_scores = list(
    "good_threshold" = 95,    # >= 95% is "good"
    "fair_threshold" = 85,    # >= 85% is "fair", below is "poor"
    "good_color" = "#10b981",
    "fair_color" = "#f59e0b",
    "poor_color" = "#ef4444"
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

.PIPELINE_DERIVED_TABLES <- c(
  "condition_era", "drug_era", "dose_era", "observation_period", "cdm_source"
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
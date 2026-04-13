# ── Environment ──────────────────────────────────────────────────────────────
DATA_DIR    <- Sys.getenv("DATA_DIR",    unset = "/data")
SCRIPTS_DIR <- Sys.getenv("SCRIPTS_DIR", unset = "/app")
OUTPUT_DIR  <- Sys.getenv("OUTPUT_DIR",  unset = "/data/outputs")


dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Helper ────────────────────────────────────────────────────────────────────
run_step <- function(step_name, script_file) {
  cat(sprintf("\n[STEP] %s\n", step_name))
  cat(sprintf("       Running: %s\n", script_file))
  tryCatch({
    source(script_file, local = new.env())
    cat(sprintf("[DONE] %s completed successfully.\n", step_name))
  }, error = function(e) {
    cat(sprintf("[ERROR] %s FAILED:\n  %s\n", step_name, conditionMessage(e)))
    stop(sprintf("Pipeline aborted at step: %s", step_name))
  })
}

# ── Pipeline ──────────────────────────────────────────────────────────────────

run_step(
  "1. API Data Acquisition & Weather Merge",
  file.path(SCRIPTS_DIR, "api_weather_merge.R")
)

run_step(
  "2. Data Cleaning & Feature Engineering",
  file.path(SCRIPTS_DIR, "data_cleaning.R")       # single unified file
)

run_step(
  "3. Exploratory Data Analysis",
  file.path(SCRIPTS_DIR, "exploratory_data_analysis.R")
)

run_step(
  "4. Descriptive Modelling (K-Means Clustering)",
  file.path(SCRIPTS_DIR, "descriptive_modelling.R")
)

run_step(
  "5. Classification Modelling (Decision Tree + Random Forest)",
  file.path(SCRIPTS_DIR, "classification_model.R")
)
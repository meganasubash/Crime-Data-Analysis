# Shared data directory
DATA_DIR    <- Sys.getenv("DATA_DIR",    unset = "/data")
SCRIPTS_DIR <- Sys.getenv("SCRIPTS_DIR", unset = "/app")
OUTPUT_DIR  <- Sys.getenv("OUTPUT_DIR",  unset = "/data/outputs")

# Create output directory if it doesn't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Helper: run a script and stop on failure ----
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

# ---- Pipeline stages ----

run_step(
  "1. API Data Acquisition & Weather Merge",
  file.path(SCRIPTS_DIR, "api_weather_merge.R")
)

run_step(
  "2. Data Cleaning (Stage 1)",
  file.path(SCRIPTS_DIR, "data_cleaning.R")
)

run_step(
  "3. Data Cleaning (Stage 2)",
  file.path(SCRIPTS_DIR, "data_cleaning_2.R")
)

run_step(
  "4. Exploratory Data Analysis",
  file.path(SCRIPTS_DIR, "exploratory_data_analysis.R")
)

run_step(
  "5. Descriptive Modelling (K-Means Clustering)",
  file.path(SCRIPTS_DIR, "descriptive_modelling.R")
)

run_step(
  "6. Classification Modelling (Decision Tree + Random Forest)",
  file.path(SCRIPTS_DIR, "classification_model.R")
)
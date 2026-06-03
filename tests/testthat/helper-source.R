# Helper: Source all project modules for testing
#
# This file is automatically loaded by testthat before running tests.
# It sources all R utility and analysis modules so test files can
# call functions directly without manual source() calls.
#
# Note: This project is NOT an R package. We source files using here::here()
# for reliable path resolution regardless of working directory.

library(here)

# --- Utility modules ---
utility_files <- list.files(
  here("R", "utils"),
  pattern = "\\.R$",
  full.names = TRUE
)
for (f in utility_files) {
  tryCatch(
    source(f, local = FALSE),
    error = function(e) {
      message(sprintf("Warning: Could not source %s: %s", basename(f), e$message))
    }
  )
}

# --- Analysis modules (source if they exist) ---
analysis_dir <- here("R", "analysis")
if (dir.exists(analysis_dir)) {
  analysis_files <- list.files(analysis_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in analysis_files) {
    tryCatch(
      source(f, local = FALSE),
      error = function(e) {
        message(sprintf("Warning: Could not source %s: %s", basename(f), e$message))
      }
    )
  }
}

# --- Ingest modules (source parsing functions only, skip main execution) ---
# Note: Ingest scripts run top-level code, so we only source them if they
# define functions without side effects. For now we skip ingest scripts
# and rely on fixtures/cached data for testing parse logic.

# --- Test fixtures path helper ---
fixtures_path <- function(...) {
  here("tests", "fixtures", ...)
}

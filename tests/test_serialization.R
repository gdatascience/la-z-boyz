#!/usr/bin/env Rscript
#' Unit tests for R/utils/serialization.R
#'
#' Run with: Rscript tests/test_serialization.R

source("R/utils/serialization.R")

# --- Test helpers ---
test_count <- 0
pass_count <- 0
fail_count <- 0

assert <- function(desc, expr) {
  test_count <<- test_count + 1
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    pass_count <<- pass_count + 1
    message(sprintf("  PASS: %s", desc))
  } else {
    fail_count <<- fail_count + 1
    message(sprintf("  FAIL: %s", desc))
  }
}

# --- Setup: temp directory for test output ---
tmp_dir <- tempfile("ser_test_")
dir.create(tmp_dir, recursive = TRUE)

# ============================================================
# Test 1: save_rds_with_metadata attaches correct attributes
# ============================================================
message("\n--- Test: save_rds_with_metadata ---")

test_df <- data.frame(
  player_name = c("Mike Trout", "Shohei Ohtani"),
  hr = c(40, 44),
  stringsAsFactors = FALSE
)

rds_path <- file.path(tmp_dir, "test_data.rds")
result <- save_rds_with_metadata(test_df, rds_path, source = "test_source", league_id = "l-z-bs")

assert("RDS file is created", file.exists(rds_path))
assert("Returns data invisibly", identical(nrow(result), 2L))
assert("source_file attribute set", attr(result, "source_file") == "test_source")
assert("league_id attribute set", attr(result, "league_id") == "l-z-bs")
assert("parsed_at attribute is POSIXct", inherits(attr(result, "parsed_at"), "POSIXct"))

# Verify round-trip preserves attributes
loaded <- readRDS(rds_path)
assert("Loaded data matches original values",
       identical(loaded$player_name, test_df$player_name) && identical(loaded$hr, test_df$hr))
assert("Loaded data has source_file attr", attr(loaded, "source_file") == "test_source")
assert("Loaded data has league_id attr", attr(loaded, "league_id") == "l-z-bs")
assert("Loaded data has parsed_at attr", inherits(attr(loaded, "parsed_at"), "POSIXct"))

# ============================================================
# Test 2: save_rds_with_metadata handles file system errors
# ============================================================
message("\n--- Test: save_rds_with_metadata file system error ---")

bad_path <- "/nonexistent_root_dir_xyz/impossible/test.rds"
# Should warn but not error
warn_result <- tryCatch(
  save_rds_with_metadata(test_df, bad_path, source = "test"),
  warning = function(w) w
)
assert("File system error produces warning", inherits(warn_result, "warning"))
assert("Warning mentions file path",
       grepl("nonexistent_root_dir_xyz", conditionMessage(warn_result)))

# ============================================================
# Test 3: load_rds_safe — happy path
# ============================================================
message("\n--- Test: load_rds_safe happy path ---")

safe_loaded <- load_rds_safe(rds_path)
assert("load_rds_safe returns data", !is.null(safe_loaded))
assert("load_rds_safe preserves content", identical(safe_loaded$player_name, test_df$player_name))
assert("load_rds_safe preserves attributes", attr(safe_loaded, "source_file") == "test_source")

# ============================================================
# Test 4: load_rds_safe — missing file
# ============================================================
message("\n--- Test: load_rds_safe missing file ---")

missing_result <- tryCatch(
  load_rds_safe(file.path(tmp_dir, "does_not_exist.rds")),
  warning = function(w) list(result = NULL, warning = w)
)
assert("Missing file returns NULL or warning",
       is.null(missing_result) || (is.list(missing_result) && is.null(missing_result$result)))

# ============================================================
# Test 5: load_rds_safe — corrupted file
# ============================================================
message("\n--- Test: load_rds_safe corrupted file ---")

corrupt_path <- file.path(tmp_dir, "corrupt.rds")
writeLines("this is not valid RDS content", corrupt_path)

corrupt_result <- tryCatch(
  load_rds_safe(corrupt_path),
  warning = function(w) list(result = NULL, warning = w)
)
assert("Corrupted file returns NULL or warning",
       is.null(corrupt_result) || (is.list(corrupt_result) && is.null(corrupt_result$result)))

# ============================================================
# Test 6: export_csv — basic export
# ============================================================
message("\n--- Test: export_csv basic ---")

csv_path <- file.path(tmp_dir, "test_export.csv")
csv_result <- export_csv(test_df, csv_path)

assert("CSV file is created", file.exists(csv_path))
assert("export_csv returns data invisibly", identical(nrow(csv_result), 2L))

# Read back and verify content
read_back <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
assert("CSV round-trip preserves player names",
       identical(read_back$player_name, test_df$player_name))
assert("CSV round-trip preserves numeric data",
       isTRUE(all.equal(read_back$hr, test_df$hr)))

# ============================================================
# Test 7: export_csv — UTF-8 characters
# ============================================================
message("\n--- Test: export_csv UTF-8 encoding ---")

utf8_df <- data.frame(
  player_name = c("Jos\u00e9 Ram\u00edrez", "Julio Rodr\u00edguez", "Rafael Devers"),
  team = c("CLE", "SEA", "BOS"),
  stringsAsFactors = FALSE
)

utf8_csv_path <- file.path(tmp_dir, "utf8_test.csv")
export_csv(utf8_df, utf8_csv_path, encoding = "UTF-8")

utf8_read_back <- read.csv(utf8_csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
assert("UTF-8 names preserved (Jos\u00e9)",
       utf8_read_back$player_name[1] == "Jos\u00e9 Ram\u00edrez")
assert("UTF-8 names preserved (Julio)",
       utf8_read_back$player_name[2] == "Julio Rodr\u00edguez")

# ============================================================
# Test 8: export_csv — non-data.frame input
# ============================================================
message("\n--- Test: export_csv non-data.frame error ---")

non_df_error <- tryCatch(
  export_csv(list(a = 1, b = 2), file.path(tmp_dir, "bad.csv")),
  error = function(e) e
)
assert("Non-data.frame input produces error", inherits(non_df_error, "error"))
assert("Error message mentions data frame",
       grepl("data frame", conditionMessage(non_df_error)))

# ============================================================
# Test 9: export_csv — file system error
# ============================================================
message("\n--- Test: export_csv file system error ---")

bad_csv_path <- "/nonexistent_root_dir_xyz/impossible/test.csv"
csv_warn <- tryCatch(
  export_csv(test_df, bad_csv_path),
  warning = function(w) w
)
assert("CSV file system error produces warning", inherits(csv_warn, "warning"))

# ============================================================
# Test 10: save_rds_with_metadata creates nested directories
# ============================================================
message("\n--- Test: save_rds_with_metadata directory creation ---")

nested_path <- file.path(tmp_dir, "deep", "nested", "dir", "data.rds")
save_rds_with_metadata(test_df, nested_path, source = "nested_test")
assert("Nested directories created and file saved", file.exists(nested_path))

# --- Cleanup ---
unlink(tmp_dir, recursive = TRUE)

# --- Summary ---
message(sprintf("\n=== Test Summary: %d/%d passed, %d failed ===",
                pass_count, test_count, fail_count))
if (fail_count > 0) {
  quit(status = 1)
}

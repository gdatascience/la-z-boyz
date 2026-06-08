#!/usr/bin/env Rscript
#' Parse Standings from CBS CSV Export
#'
#' Ingests the CSV standings file exported from CBS
#' (Standings > Overall > CSV export button).
#'
#' Usage: Rscript R/ingest/parse_standings.R [path/to/file.csv]
#'
#' If no path is provided, looks for `overall.csv` in data/imports/.
#'
#' Output: data/cache/standings.rds
#'
#' CSV Format (from CBS):
#'   - Divisions are separated by header rows like "North Division"
#'   - Column headers appear after each division header:
#'     Team,W,L,T,PCT,GB,Streak,Div,Wks,PF,Back,PA,
#'   - Team rows: TeamName,W,L,T,PCT,GB,Streak,DivRecord,Wks,PF,Back,PA,

library(here)

# --- Config ---
output_file <- here("data/cache/standings.rds")
EXPECTED_TEAMS <- 16
EXPECTED_DIVISIONS <- 4
LEAGUE_ID <- "l-z-bs"

# --- Logging utilities ---
log_error <- function(msg) message(sprintf("[%s] ERROR: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
log_warn  <- function(msg) message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
log_info  <- function(msg) message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))

# --- Find CSV file ---
find_csv_file <- function(path_arg = NULL) {
  if (!is.null(path_arg) && file.exists(path_arg)) {
    return(path_arg)
  }

  import_dir <- here("data/imports")

  # Look for overall.csv or standings*.csv
  candidates <- c(
    file.path(import_dir, "overall.csv"),
    list.files(import_dir, pattern = "^standings.*\\.csv$", full.names = TRUE)
  )
  candidates <- candidates[file.exists(candidates)]

  if (length(candidates) == 0) {
    stop("No standings CSV found in data/imports/. Export from CBS: Standings > Overall > CSV export button.")
  }

  candidates[1]
}

# --- Cache fallback ---
load_cached_standings <- function() {
  if (file.exists(output_file)) {
    cached <- tryCatch(readRDS(output_file), error = function(e) NULL)
    if (!is.null(cached)) {
      cache_time <- attr(cached, "parsed_at")
      if (!is.null(cache_time)) {
        log_warn(sprintf("Using cached standings data from %s", format(cache_time, "%Y-%m-%d %H:%M:%S")))
      } else {
        log_warn("Using cached standings data (unknown age)")
      }
    }
    return(cached)
  }
  NULL
}

# --- Main parser ---
parse_standings_csv <- function(csv_path) {
  log_info(paste("Parsing standings from CSV:", csv_path))

  lines <- readLines(csv_path, warn = FALSE)

  all_standings <- list()
  current_division <- NA_character_

  for (line in lines) {
    trimmed <- trimws(line)

    # Skip empty lines
    if (nchar(trimmed) == 0) next

    # Detect division header: a line like "North Division" (no commas in the data portion)
    # Division headers contain "Division" and have no commas before it
    if (grepl("Division$", trimmed) && !grepl(",", trimmed)) {
      current_division <- trimmed
      next
    }

    # Skip column header rows
    if (grepl("^Team,", trimmed)) next

    # Skip if no division context yet
    if (is.na(current_division)) next

    # Parse team row
    parsed <- tryCatch({
      con <- textConnection(line)
      result <- read.csv(con, header = FALSE, stringsAsFactors = FALSE, quote = '"')
      close(con)
      result
    }, error = function(e) NULL)

    if (is.null(parsed) || ncol(parsed) < 11) next

    team_name <- trimws(as.character(parsed$V1[1]))
    if (nchar(team_name) == 0) next

    wins   <- as.integer(parsed$V2[1])
    losses <- as.integer(parsed$V3[1])
    ties   <- as.integer(parsed$V4[1])
    pct    <- as.numeric(parsed$V5[1])

    gb_raw <- trimws(as.character(parsed$V6[1]))
    games_back <- if (gb_raw == "-" || gb_raw == "0") 0 else suppressWarnings(as.numeric(gb_raw))

    streak     <- trimws(as.character(parsed$V7[1]))
    div_record <- trimws(as.character(parsed$V8[1]))

    # Wks (magic number / weeks) column
    wks_raw <- trimws(as.character(parsed$V9[1]))
    magic_number <- wks_raw

    total_points       <- suppressWarnings(as.numeric(parsed$V10[1]))
    points_behind_raw  <- trimws(as.character(parsed$V11[1]))
    points_behind_leader <- suppressWarnings(as.numeric(points_behind_raw))

    points_against <- suppressWarnings(as.numeric(parsed$V12[1]))

    all_standings[[length(all_standings) + 1]] <- data.frame(
      division = current_division,
      team_name = team_name,
      wins = wins,
      losses = losses,
      ties = ties,
      pct = pct,
      games_back = games_back,
      streak = streak,
      div_record = div_record,
      magic_number = magic_number,
      total_points = total_points,
      points_behind_leader = points_behind_leader,
      points_against = points_against,
      stringsAsFactors = FALSE
    )
  }

  if (length(all_standings) == 0) {
    stop("No team rows parsed from CSV. Check file format.")
  }

  standings_df <- do.call(rbind, all_standings)

  # Derived fields
  standings_df$games_played <- standings_df$wins + standings_df$losses + standings_df$ties
  standings_df$ppg <- round(standings_df$total_points / standings_df$games_played, 1)

  # Sort by pct descending, then total points

  standings_df <- standings_df[order(-standings_df$pct, -standings_df$total_points), ]
  rownames(standings_df) <- NULL

  standings_df
}

# --- Main execution ---
args <- commandArgs(trailingOnly = TRUE)
csv_path <- tryCatch(
  find_csv_file(if (length(args) > 0) args[1] else NULL),
  error = function(e) {
    log_error(e$message)
    cached <- load_cached_standings()
    if (!is.null(cached)) return(NULL)
    stop(e$message)
  }
)

if (is.null(csv_path)) {
  # Using cached data (find_csv_file failed but cache was loaded)
  q(save = "no", status = 0)
}

standings_df <- parse_standings_csv(csv_path)

# --- Validation ---
n_teams <- nrow(standings_df)
n_divisions <- length(unique(standings_df$division))

if (n_teams != EXPECTED_TEAMS) {
  log_warn(sprintf("Expected %d teams but found %d: %s",
    EXPECTED_TEAMS, n_teams, paste(standings_df$team_name, collapse = ", ")))
}
if (n_divisions != EXPECTED_DIVISIONS) {
  log_warn(sprintf("Expected %d divisions but found %d: %s",
    EXPECTED_DIVISIONS, n_divisions, paste(unique(standings_df$division), collapse = ", ")))
}

# --- Add metadata ---
attr(standings_df, "source_file") <- normalizePath(csv_path, mustWork = FALSE)
attr(standings_df, "parsed_at") <- Sys.time()
attr(standings_df, "league_id") <- LEAGUE_ID
attr(standings_df, "team_count") <- n_teams
attr(standings_df, "division_count") <- n_divisions

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(standings_df, output_file)

# --- Report ---
log_info("Standings parsed successfully from CSV!")
log_info(sprintf("  Source: %s", basename(csv_path)))
log_info(sprintf("  Teams: %d", n_teams))
log_info(sprintf("  Divisions: %d", n_divisions))
log_info("  Overall rankings:")
for (i in seq_len(nrow(standings_df))) {
  r <- standings_df[i, ]
  log_info(sprintf("    %2d. %-30s %d-%d  (%.1f pts, %.1f ppg) [%s]",
    i, r$team_name, r$wins, r$losses, r$total_points, r$ppg, r$division))
}
log_info(sprintf("  Saved to: %s", output_file))

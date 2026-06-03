#!/usr/bin/env Rscript
#' Parse Standings from Saved CBS HTML
#'
#' Usage: Rscript R/ingest/parse_standings.R
#'
#' Prerequisites:
#'   1. Save https://l-z-bs.baseball.cbssports.com/standings as HTML
#'   2. Place the file at data/imports/standings.html
#'
#' Output: data/cache/standings.rds
#'
#' Error Handling:
#'   - If HTML import file is missing, displays instructions with exact CBS URL
#'   - Validates 16 teams found across 4 divisions
#'   - Falls back to cached RDS if parse fails
#'   - Adds metadata attributes (source_file, parsed_at, league_id) to saved RDS
#'
#' Validates: Requirements 1.4, 1.5, 1.7

library(rvest)
library(dplyr)

# --- Config ---
import_file <- "data/imports/standings.html"
output_file <- "data/cache/standings.rds"
EXPECTED_TEAMS <- 16
EXPECTED_DIVISIONS <- 4
CBS_STANDINGS_URL <- "https://l-z-bs.baseball.cbssports.com/standings"
LEAGUE_ID <- "l-z-bs"

# --- Logging utilities ---
#' Log an error message with timestamp
#' @param msg Character message to log
log_error <- function(msg) {
  message(sprintf("[%s] ERROR: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

#' Log a warning message with timestamp
#' @param msg Character message to log
log_warn <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

#' Log an info message with timestamp
#' @param msg Character message to log
log_info <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# --- Cache fallback ---
#' Load cached standings RDS if available
#' @return Data frame of cached standings or NULL if no cache exists
load_cached_standings <- function() {
  if (file.exists(output_file)) {
    cached <- tryCatch(
      readRDS(output_file),
      error = function(e) {
        log_error(paste("Cached RDS is corrupted:", e$message))
        NULL
      }
    )
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

#' Validate standings data frame structure and counts
#' @param standings_df Data frame to validate
#' @return TRUE if valid, FALSE otherwise
validate_standings <- function(standings_df) {
  if (!is.data.frame(standings_df)) {
    log_error("Standings data is not a data frame")
    return(FALSE)
  }

  required_cols <- c("division", "team_name", "wins", "losses", "ties", "pct",
                     "games_back", "streak", "div_record", "magic_number",
                     "total_points", "points_behind_leader", "points_against",
                     "games_played", "ppg")
  missing_cols <- setdiff(required_cols, names(standings_df))
  if (length(missing_cols) > 0) {
    log_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }

  n_teams <- nrow(standings_df)
  if (n_teams != EXPECTED_TEAMS) {
    log_warn(sprintf(
      "Expected %d teams but found %d. Data may be incomplete.",
      EXPECTED_TEAMS, n_teams
    ))
  }

  n_divisions <- length(unique(na.omit(standings_df$division)))
  if (n_divisions != EXPECTED_DIVISIONS) {
    log_warn(sprintf(
      "Expected %d divisions but found %d. Division parsing may have issues.",
      EXPECTED_DIVISIONS, n_divisions
    ))
  }

  TRUE
}

# --- Validate input file exists (Requirement 1.7) ---
if (!file.exists(import_file)) {
  log_error(paste("Missing file:", import_file))
  message(
    "\n",
    "========================================================\n",
    " MISSING FILE: ", import_file, "\n",
    "========================================================\n",
    "\n",
    "The standings HTML file is required for parsing.\n",
    "Please follow these steps to obtain it:\n",
    "\n",
    "  1. Open your browser and log into CBS Sports\n",
    "  2. Navigate to: ", CBS_STANDINGS_URL, "\n",
    "  3. Wait for the page to fully load (all divisions visible)\n",
    "  4. Save the page (Cmd+S / Ctrl+S) as 'Webpage, Complete'\n",
    "  5. Save the file as 'standings.html' in the following path:\n",
    "     ", normalizePath("data/imports", mustWork = FALSE), "/standings.html\n",
    "\n",
    "Note: You must be logged in to CBS to see full standings data.\n",
    "========================================================\n"
  )

  # Attempt cache fallback
  cached <- load_cached_standings()
  if (!is.null(cached)) {
    log_info("Returning cached data since HTML file is missing")
    invisible(cached)
  } else {
    stop("No cached data available. Cannot proceed without standings data.")
  }
}

# --- Parse HTML ---
log_info(paste("Parsing standings from:", import_file))
page <- tryCatch(
  read_html(import_file),
  error = function(e) {
    log_error(sprintf("Failed to parse HTML file: %s", conditionMessage(e)))
    log_info("Attempting to use cached data...")
    cached <- load_cached_standings()
    if (is.null(cached)) {
      stop(
        "Failed to parse HTML file: ", import_file, "\n",
        "Error: ", conditionMessage(e), "\n",
        "No cached data available. The file may be corrupted.\n",
        "Try re-saving from: ", CBS_STANDINGS_URL, "\n"
      )
    }
    return(cached)
  }
)

# If cache fallback returned a data frame, skip parsing
if (is.data.frame(page)) {
  log_info("Using cached standings data (parse failed)")
  invisible(page)
  q(save = "no", status = 0)
}

tables <- page |> html_elements("table.data")

if (length(tables) == 0) {
  log_error("No standings tables found in HTML file")
  cached <- load_cached_standings()
  if (!is.null(cached)) {
    log_info("Returning cached data since no tables found in HTML")
    invisible(cached)
  } else {
    stop(
      "No standings tables found in HTML file: ", import_file, "\n",
      "The CBS page layout may have changed, or the file was not saved correctly.\n",
      "Please re-save from: ", CBS_STANDINGS_URL, "\n"
    )
  }
}

# --- Extract standings data (Requirement 1.4) ---
# Required fields: division, team_name, wins, losses, ties, pct,
#   games_back, streak, div_record, magic_number, total_points,
#   points_behind_leader, points_against, games_played (derived), ppg (derived)
all_standings <- list()
current_division <- NA_character_

for (tbl in tables) {
  # Get all rows including title rows (division headers)
  all_rows <- tbl |> html_elements("tr")

  for (row in all_rows) {
    # Check if this is a division title row
    title_td <- row |> html_element("td[colspan]")
    if (!is.na(title_td)) {
      div_text <- html_text(title_td, trim = TRUE)
      if (grepl("Division$", div_text)) {
        current_division <- div_text
      }
      next
    }

    # Check if this is a label row (headers)
    row_class <- html_attr(row, "class")
    if (!is.na(row_class) && grepl("label", row_class)) {
      next
    }

    # Parse data rows
    cells <- row |> html_elements("td")
    if (length(cells) < 8) next

    team_link <- cells[[1]] |> html_element("a")
    team_name <- if (!is.na(team_link)) html_text(team_link, trim = TRUE) else html_text(cells[[1]], trim = TRUE)

    if (nchar(team_name) == 0) next

    # Extract numeric values from cells
    cell_vals <- sapply(cells, function(c) html_text(c, trim = TRUE))

    all_standings[[length(all_standings) + 1]] <- data.frame(
      division = current_division,
      team_name = team_name,
      wins = as.integer(cell_vals[2]),
      losses = as.integer(cell_vals[3]),
      ties = as.integer(cell_vals[4]),
      pct = as.numeric(cell_vals[5]),
      games_back = as.numeric(cell_vals[6]),
      streak = cell_vals[7],
      div_record = cell_vals[8],
      magic_number = if (length(cell_vals) >= 9) cell_vals[9] else NA_character_,
      total_points = if (length(cell_vals) >= 10) as.numeric(cell_vals[10]) else NA_real_,
      points_behind_leader = if (length(cell_vals) >= 11) as.numeric(cell_vals[11]) else NA_real_,
      points_against = if (length(cell_vals) >= 12) as.numeric(cell_vals[12]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

standings_df <- bind_rows(all_standings)

# --- Derived fields ---
standings_df <- standings_df |>
  mutate(
    games_played = wins + losses + ties,
    ppg = round(total_points / games_played, 1)
  ) |>
  arrange(desc(pct), desc(total_points))

# --- Validation ---
if (!validate_standings(standings_df)) {
  log_error("Validation failed for parsed standings data. Attempting cache fallback...")
  cached <- load_cached_standings()
  if (!is.null(cached) && validate_standings(cached)) {
    standings_df <- cached
    log_info("Using previously cached valid standings data")
  } else {
    stop("No valid standings data available (parse failed validation and no valid cache)")
  }
}

n_teams <- nrow(standings_df)
n_divisions <- length(unique(na.omit(standings_df$division)))

# Additional team count warning with specific details
if (n_teams != EXPECTED_TEAMS) {
  log_warn(sprintf(
    "Teams found (%d): %s",
    n_teams, paste(standings_df$team_name, collapse = ", ")
  ))
  log_warn(sprintf("Re-save HTML from: %s", CBS_STANDINGS_URL))
}

# Additional division warning
if (n_divisions != EXPECTED_DIVISIONS) {
  log_warn(sprintf(
    "Divisions found (%d): %s",
    n_divisions, paste(unique(na.omit(standings_df$division)), collapse = ", ")
  ))
}

# --- Add metadata attributes (Requirement 1.5) ---
attr(standings_df, "source_file") <- normalizePath(import_file, mustWork = FALSE)
attr(standings_df, "parsed_at") <- Sys.time()
attr(standings_df, "league_id") <- LEAGUE_ID
attr(standings_df, "team_count") <- n_teams
attr(standings_df, "division_count") <- n_divisions

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(standings_df, output_file)

# --- Report ---
log_info("Standings parsed successfully!")
log_info(sprintf("  Teams: %d", n_teams))
log_info(sprintf("  Divisions: %d", n_divisions))
log_info("  Overall rankings:")
ranked <- standings_df |> arrange(desc(pct), desc(total_points))
for (i in seq_len(nrow(ranked))) {
  r <- ranked[i, ]
  log_info(sprintf("    %2d. %-30s %d-%d  (%.0f pts, %.1f ppg) [%s]",
    i, r$team_name, r$wins, r$losses, r$total_points, r$ppg, r$division))
}
log_info(sprintf("  Saved to: %s", output_file))
log_info(sprintf("  Metadata: source_file=%s, parsed_at=%s, league_id=%s",
  attr(standings_df, "source_file"),
  format(attr(standings_df, "parsed_at"), "%Y-%m-%d %H:%M:%S"),
  attr(standings_df, "league_id")))

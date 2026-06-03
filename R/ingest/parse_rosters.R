#!/usr/bin/env Rscript
#' Parse Rosters & Salaries from Saved CBS HTML
#'
#' Usage: Rscript R/ingest/parse_rosters.R
#'
#' Prerequisites:
#'   1. Save https://l-z-bs.baseball.cbssports.com/teams/all as HTML
#'   2. Place the file at data/imports/rosters.html
#'
#' Output: data/cache/rosters.rds
#'
#' Error Handling:
#'   - If HTML import file is missing, displays instructions with exact CBS URL
#'   - Validates 16 teams found and player count between 350-450
#'   - Falls back to cached RDS if parse fails
#'   - Adds metadata attributes (source_file, parsed_at, league_id) to saved RDS
#'
#' Validates: Requirements 1.3, 1.5, 1.7, 1.8

library(rvest)
library(dplyr)

# --- Config ---
import_file <- "data/imports/rosters.html"
output_file <- "data/cache/rosters.rds"
EXPECTED_TEAMS <- 16
MIN_PLAYERS <- 350
MAX_PLAYERS <- 450
CBS_ROSTER_URL <- "https://l-z-bs.baseball.cbssports.com/teams/all"
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
#' Load cached rosters RDS if available
#' @return Data frame of cached rosters or NULL if no cache exists
load_cached_rosters <- function() {
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
        log_warn(sprintf("Using cached roster data from %s", format(cache_time, "%Y-%m-%d %H:%M:%S")))
      } else {
        log_warn("Using cached roster data (unknown age)")
      }
    }
    return(cached)
  }
  NULL
}

#' Validate roster data frame structure and counts
#' @param roster_df Data frame to validate
#' @return TRUE if valid, FALSE otherwise
validate_rosters <- function(roster_df) {
  if (!is.data.frame(roster_df)) {
    log_error("Roster data is not a data frame")
    return(FALSE)
  }

  required_cols <- c("team_name", "player_type", "roster_position", "player_name",
                     "eligible_positions", "mlb_team", "cbs_id", "salary_raw",
                     "salary", "is_minor_contract", "total_fpts")
  missing_cols <- setdiff(required_cols, names(roster_df))
  if (length(missing_cols) > 0) {
    log_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }

  n_teams <- length(unique(roster_df$team_name))
  if (n_teams != EXPECTED_TEAMS) {
    log_warn(sprintf(
      "Expected %d teams but found %d. Data may be incomplete.",
      EXPECTED_TEAMS, n_teams
    ))
  }

  n_players <- nrow(roster_df)
  if (n_players < MIN_PLAYERS || n_players > MAX_PLAYERS) {
    log_warn(sprintf(
      "Player count (%d) outside expected range (%d-%d). Data may be incomplete.",
      n_players, MIN_PLAYERS, MAX_PLAYERS
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
    "The roster HTML file is required for parsing.\n",
    "Please follow these steps to obtain it:\n",
    "\n",
    "  1. Open your browser and log into CBS Sports\n",
    "  2. Navigate to: ", CBS_ROSTER_URL, "\n",
    "  3. Wait for the page to fully load (all 16 teams visible)\n",
    "  4. Save the page (Cmd+S / Ctrl+S) as 'Webpage, Complete'\n",
    "  5. Save the file as 'rosters.html' in the following path:\n",
    "     ", normalizePath("data/imports", mustWork = FALSE), "/rosters.html\n",
    "\n",
    "Note: You must be logged in to CBS to see salary information.\n",
    "========================================================\n"
  )

  # Attempt cache fallback
  cached <- load_cached_rosters()
  if (!is.null(cached)) {
    log_info("Returning cached data since HTML file is missing")
    invisible(cached)
  } else {
    stop("No cached data available. Cannot proceed without roster data.")
  }
}

# --- Parse HTML ---
log_info(paste("Parsing rosters from:", import_file))
page <- tryCatch(
  read_html(import_file),
  error = function(e) {
    log_error(sprintf("Failed to parse HTML file: %s", conditionMessage(e)))
    log_info("Attempting to use cached data...")
    cached <- load_cached_rosters()
    if (is.null(cached)) {
      stop(
        "Failed to parse HTML file: ", import_file, "\n",
        "Error: ", conditionMessage(e), "\n",
        "No cached data available. The file may be corrupted.\n",
        "Try re-saving from: ", CBS_ROSTER_URL, "\n"
      )
    }
    return(cached)
  }
)

# If cache fallback returned a data frame, skip parsing
if (is.data.frame(page)) {
  log_info("Using cached roster data (parse failed)")
  invisible(page)
  q(save = "no", status = 0)
}

tables <- page |> html_elements("table.data")

if (length(tables) == 0) {
  log_error("No roster tables found in HTML file")
  cached <- load_cached_rosters()
  if (!is.null(cached)) {
    log_info("Returning cached data since no tables found in HTML")
    # Re-save cached data (preserve it)
    invisible(cached)
  } else {
    stop(
      "No roster tables found in HTML file: ", import_file, "\n",
      "The CBS page layout may have changed, or the file was not saved correctly.\n",
      "Please re-save from: ", CBS_ROSTER_URL, "\n"
    )
  }
}

# --- Extract roster data (Requirement 1.3) ---
# Required fields: team_name, player_type, roster_position, player_name,
#   eligible_positions, mlb_team, cbs_id, salary_raw, salary,
#   is_minor_contract, total_fpts
all_rosters <- list()

for (tbl in tables) {
  title_row <- tbl |> html_element("tr.title td")
  if (is.na(title_row)) next

  team_raw <- title_row |> html_text(trim = TRUE)
  player_type <- ifelse(grepl("Pitchers$", team_raw), "Pitcher", "Batter")
  team_name <- gsub("\\s*(Batters|Pitchers)$", "", team_raw)

  player_rows <- tbl |> html_elements("tr.playerRow")
  for (row in player_rows) {
    cells <- row |> html_elements("td")
    if (length(cells) < 9) next

    # Roster position (column 1)
    pos <- cells[[1]] |> html_text(trim = TRUE)

    # Player name and link (column 2)
    player_link <- cells[[2]] |> html_element("a.playerLink")
    player_name <- if (!is.na(player_link)) html_text(player_link, trim = TRUE) else ""

    # Eligible positions and MLB team from "1B,2B . WAS" format
    pos_team_span <- cells[[2]] |> html_element("span.playerPositionAndTeam")
    pos_team_text <- if (!is.na(pos_team_span)) html_text(pos_team_span, trim = TRUE) else ""

    # Split on bullet character or other common separators
    parts <- strsplit(pos_team_text, "\\s*[\u2022|\\|]\\s*")[[1]]
    eligible_positions <- if (length(parts) >= 1) trimws(parts[1]) else NA_character_
    mlb_team <- if (length(parts) >= 2) trimws(parts[2]) else NA_character_

    # CBS player ID from href
    href <- if (!is.na(player_link)) html_attr(player_link, "href") else ""
    cbs_id <- gsub(".*playerpage/", "", href)
    if (nchar(cbs_id) == 0 || cbs_id == href) cbs_id <- NA_character_

    # Salary (column 9) — dollar amount and optional asterisk for minor contracts
    salary_text <- cells[[9]] |> html_text(trim = TRUE)

    # Total fantasy points (last column, typically column 12)
    total_pts <- if (length(cells) >= 12) cells[[12]] |> html_text(trim = TRUE) else NA

    all_rosters[[length(all_rosters) + 1]] <- data.frame(
      team_name = team_name,
      player_type = player_type,
      roster_position = pos,
      player_name = player_name,
      eligible_positions = eligible_positions,
      mlb_team = mlb_team,
      cbs_id = cbs_id,
      salary_raw = salary_text,
      total_fpts = as.numeric(total_pts),
      stringsAsFactors = FALSE
    )
  }
}

roster_df <- bind_rows(all_rosters)

# --- Clean salary: parse amount and detect minor league contract flag ---
roster_df <- roster_df |>
  mutate(
    is_minor_contract = grepl("\\*", salary_raw),
    salary = as.numeric(gsub("[^0-9.]", "", salary_raw))
  )

# --- Validation (Requirements 1.8) ---
if (!validate_rosters(roster_df)) {
  log_error("Validation failed for parsed roster data. Attempting cache fallback...")
  cached <- load_cached_rosters()
  if (!is.null(cached) && validate_rosters(cached)) {
    roster_df <- cached
    log_info("Using previously cached valid roster data")
  } else {
    stop("No valid roster data available (parse failed validation and no valid cache)")
  }
}

n_teams <- length(unique(roster_df$team_name))
n_players <- nrow(roster_df)

# Additional team count warning with specific details
if (n_teams != EXPECTED_TEAMS) {
  log_warn(sprintf(
    "Teams found (%d): %s",
    n_teams, paste(unique(roster_df$team_name), collapse = ", ")
  ))
  log_warn(sprintf("Re-save HTML from: %s", CBS_ROSTER_URL))
}

# Additional player count warning with breakdown
if (n_players < MIN_PLAYERS || n_players > MAX_PLAYERS) {
  log_warn(sprintf(
    "Player breakdown — Batters: %d, Pitchers: %d",
    sum(roster_df$player_type == "Batter"),
    sum(roster_df$player_type == "Pitcher")
  ))
}

# --- Add metadata attributes (Requirement 1.5) ---
attr(roster_df, "source_file") <- normalizePath(import_file, mustWork = FALSE)
attr(roster_df, "parsed_at") <- Sys.time()
attr(roster_df, "league_id") <- LEAGUE_ID
attr(roster_df, "team_count") <- n_teams
attr(roster_df, "player_count") <- n_players

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(roster_df, output_file)

# --- Report ---
log_info("Rosters parsed successfully!")
log_info(sprintf("  Teams: %d", n_teams))
log_info(sprintf("  Players: %d", n_players))
log_info(sprintf("  Batters: %d", sum(roster_df$player_type == "Batter")))
log_info(sprintf("  Pitchers: %d", sum(roster_df$player_type == "Pitcher")))
log_info(sprintf("  Minor contracts: %d", sum(roster_df$is_minor_contract, na.rm = TRUE)))
log_info(sprintf("  Salary range: $%s - $%s",
  min(roster_df$salary, na.rm = TRUE),
  max(roster_df$salary, na.rm = TRUE)))
log_info(sprintf("  Saved to: %s", output_file))
log_info(sprintf("  Metadata: source_file=%s, parsed_at=%s, league_id=%s",
  attr(roster_df, "source_file"),
  format(attr(roster_df, "parsed_at"), "%Y-%m-%d %H:%M:%S"),
  attr(roster_df, "league_id")))

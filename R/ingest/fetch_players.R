#!/usr/bin/env Rscript
#' Fetch CBS Player Database (no authentication required)
#'
#' Usage: Rscript R/ingest/fetch_players.R
#'
#' Output: data/cache/players.rds
#'
#' This pulls the full MLB player database (~8,400 players) from the CBS API.
#' Includes: name, position, team, pro status, age, handedness, injury info.
#'
#' Error Handling:
#'   - 60-second timeout on CBS API call
#'   - On error/timeout, logs with timestamp and falls back to cached RDS
#'   - Validates player count (~8,400) after fetch
#'   - Adds metadata attributes (source_file, parsed_at, league_id) to saved RDS

library(httr2)
library(jsonlite)

# --- Config ---
league_id <- "l-z-bs"
base_url <- paste0("https://", league_id, ".baseball.cbssports.com")
output_file <- "data/cache/players.rds"
expected_player_count_min <- 7000
expected_player_count_max <- 10000

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

#' Load cached players RDS if available
#' @return Data frame of cached players or NULL if no cache exists
load_cached_players <- function() {
  if (file.exists(output_file)) {
    cached <- tryCatch(
      readRDS(output_file),
      error = function(e) {
        log_error(paste("Cached RDS is corrupted:", e$message))
        NULL
      }
    )
    if (!is.null(cached)) {
      cache_time <- attr(cached, "parsed_at") %||% attr(cached, "fetched_at")
      if (!is.null(cache_time)) {
        log_warn(sprintf("Using cached data from %s", format(cache_time, "%Y-%m-%d %H:%M:%S")))
      } else {
        log_warn("Using cached data (unknown age)")
      }
    }
    return(cached)
  }
  NULL
}

#' Validate player data frame structure and count
#' @param players_df Data frame to validate
#' @return TRUE if valid, FALSE otherwise
validate_players <- function(players_df) {

  if (!is.data.frame(players_df)) {
    log_error("Players data is not a data frame")
    return(FALSE)
  }

  n <- nrow(players_df)
  if (n < expected_player_count_min || n > expected_player_count_max) {
    log_warn(sprintf(
      "Player count (%d) outside expected range (%d-%d). Data may be incomplete.",
      n, expected_player_count_min, expected_player_count_max
    ))
    # Still return TRUE — partial data is better than no data
  }

  required_cols <- c("cbs_id", "fullname", "position", "mlb_team", "pro_status")
  missing_cols <- setdiff(required_cols, names(players_df))
  if (length(missing_cols) > 0) {
    log_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }

  TRUE
}

# --- Fetch with error handling ---
log_info("Fetching player database from CBS API...")
log_info("This is ~3MB and may take a moment...")

players_df <- tryCatch(
  {
    resp <- request(paste0(base_url, "/api/players/list")) |>
      req_url_query(response_format = "JSON") |>
      req_timeout(60) |>
      req_perform()

    players_json <- resp_body_json(resp)
    players_list <- players_json$body$players

    log_info(sprintf("Received %d players from API", length(players_list)))

    # --- Convert to data frame ---
    do.call(rbind, lapply(players_list, function(p) {
      data.frame(
        cbs_id = p$id %||% NA_character_,
        fullname = p$fullname %||% NA_character_,
        firstname = p$firstname %||% NA_character_,
        lastname = p$lastname %||% NA_character_,
        position = p$position %||% NA_character_,
        eligible_positions = p$eligible_positions_display %||% NA_character_,
        mlb_team = p$pro_team %||% NA_character_,
        pro_status = p$pro_status %||% NA_character_,
        age = as.integer(p$age %||% NA),
        bats = p$bats %||% NA_character_,
        throws = p$throws %||% NA_character_,
        jersey = p$jersey %||% NA_character_,
        injury = if (!is.null(p$icons$injury)) p$icons$injury else NA_character_,
        elias_id = p$elias_id %||% NA_character_,
        photo_url = p$photo %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }))
  },
  error = function(e) {
    log_error(sprintf("CBS API request failed: %s", e$message))
    log_info("Attempting to use cached data...")
    cached <- load_cached_players()
    if (is.null(cached)) {
      stop("No cached data available. Cannot proceed without player data.")
    }
    cached
  }
)

# --- Validate ---
if (!validate_players(players_df)) {
  log_error("Validation failed for fetched player data. Attempting cache fallback...")
  cached <- load_cached_players()
  if (!is.null(cached) && validate_players(cached)) {
    players_df <- cached
    log_info("Using previously cached valid data")
  } else {
    stop("No valid player data available (fetch failed validation and no valid cache)")
  }
}

# --- Add metadata attributes ---
attr(players_df, "source_file") <- paste0(base_url, "/api/players/list?response_format=JSON")
attr(players_df, "parsed_at") <- Sys.time()
attr(players_df, "league_id") <- league_id

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(players_df, output_file)

# --- Report ---
log_info("Player database saved!")
log_info(sprintf("  Total players: %d", nrow(players_df)))
log_info(sprintf("  Active (26-man): %d", sum(players_df$pro_status == "A", na.rm = TRUE)))
log_info(sprintf("  Minors: %d", sum(players_df$pro_status == "M", na.rm = TRUE)))
log_info(sprintf("  Injured List: %d", sum(players_df$pro_status == "IL", na.rm = TRUE)))
log_info(sprintf("  Positions: %s",
  paste(names(sort(table(players_df$position), decreasing = TRUE)[1:5]), collapse = ", ")))
log_info(sprintf("  Saved to: %s", output_file))
log_info(sprintf("  Metadata: source_file=%s, parsed_at=%s, league_id=%s",
  attr(players_df, "source_file"),
  format(attr(players_df, "parsed_at"), "%Y-%m-%d %H:%M:%S"),
  attr(players_df, "league_id")))

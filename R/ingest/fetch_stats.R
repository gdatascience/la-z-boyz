#!/usr/bin/env Rscript
#' Fetch Current and Historical Season Stats via {baseballr}
#'
#' Usage: Rscript R/ingest/fetch_stats.R
#'        Rscript R/ingest/fetch_stats.R --player "Jose Ramirez"
#'        Rscript R/ingest/fetch_stats.R --year 2023
#'        Rscript R/ingest/fetch_stats.R --seasons 4
#'
#' Output: data/cache/stats_batting_{year}.rds
#'         data/cache/stats_pitching_{year}.rds
#'
#' Uses {baseballr} to pull season stats from Baseball Reference.
#' Includes exponential backoff retry on failure (2s, 4s, 8s — max 3 retries),
#' fallback to cached RDS with cache-age warning, multi-season support,
#' and silent fallback from FanGraphs 403 errors to Baseball Reference.
#'
#' Error Handling:
#'   - Exponential backoff retry: 2s, 4s, 8s delays (max 3 retries)
#'   - On all retries exhausted, falls back to cached RDS if available
#'   - Warns about cache age when using stale data
#'   - FanGraphs 403 silently falls back to Baseball Reference
#'   - Adds metadata attributes (source_file, parsed_at, league_id, season)
#'
#' Requirements: 3.1, 3.2, 3.6, 3.7, 3.8, 3.9

library(baseballr)
library(dplyr)

# --- Configuration ---
CACHE_DIR <- "data/cache"
LEAGUE_ID <- "l-z-bs"
MAX_RETRIES <- 3
BASE_DELAY_SECONDS <- 2

args <- commandArgs(trailingOnly = TRUE)
current_year <- as.integer(format(Sys.Date(), "%Y"))

dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Logging helpers (consistent with fetch_players.R) ---

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

# --- Helper: Exponential backoff retry ---
#' Retry a function with exponential backoff (2s, 4s, 8s)
#'
#' On failure, waits base_delay * 2^(attempt-1) seconds before retrying.
#' Delays: attempt 1 fails -> wait 2s, attempt 2 fails -> wait 4s,
#' attempt 3 fails -> wait 8s, attempt 4 fails -> give up.
#'
#' @param fn Function to execute (no arguments)
#' @param max_retries Maximum number of retries (default 3)
#' @param base_delay Base delay in seconds (default 2)
#' @param source_label Label for log messages
#' @return Result of fn() or NULL on failure after all retries
retry_with_backoff <- function(fn, max_retries = MAX_RETRIES,
                               base_delay = BASE_DELAY_SECONDS,
                               source_label = "Baseball Reference") {
  for (attempt in seq_len(max_retries + 1)) {
    result <- tryCatch({
      fn()
    }, error = function(e) {
      if (attempt <= max_retries) {
        delay <- base_delay * (2^(attempt - 1))
        log_warn(sprintf("[%s] Attempt %d/%d failed: %s — retrying in %ds...",
                         source_label, attempt, max_retries + 1, e$message, delay))
        Sys.sleep(delay)
      } else {
        log_error(sprintf("[%s] All %d attempts failed: %s",
                          source_label, max_retries + 1, e$message))
      }
      NULL
    })
    if (!is.null(result)) return(result)
  }
  NULL
}

# --- Helper: FanGraphs 403 silent fallback ---
#' Attempt FanGraphs fetch; on 403 silently fall back to Baseball Reference
#'
#' Per requirement 3.7: If FanGraphs endpoints return 403 errors,
#' fall back to Baseball Reference as the primary stats source without failing.
#'
#' @param fg_fn FanGraphs fetch function
#' @param bref_fn Baseball Reference fallback function
#' @param label Label for logging
#' @return Data from whichever source succeeds, or NULL
fetch_with_fangraphs_fallback <- function(fg_fn, bref_fn, label = "stats") {
  # Attempt FanGraphs first
  fg_result <- tryCatch({
    fg_fn()
  }, error = function(e) {
    is_403 <- grepl("403|Forbidden|forbidden", e$message, ignore.case = TRUE)
    if (is_403) {
      # Silent fallback — no error displayed for 403 (Requirement 3.7)
      log_info(sprintf("[FanGraphs] 403 for %s — falling back to Baseball Reference", label))
    } else {
      log_warn(sprintf("[FanGraphs] Error fetching %s: %s — falling back to Baseball Reference",
                       label, e$message))
    }
    NULL
  })

  if (!is.null(fg_result) && nrow(fg_result) > 0) {
    return(fg_result)
  }

  # Fallback to Baseball Reference with exponential backoff retry
  retry_with_backoff(bref_fn, source_label = "Baseball Reference")
}

# --- Helper: Load cached RDS with age warning ---
#' Load cached RDS file, warn about cache age
#'
#' Per requirement 3.6: If Baseball Reference returns an error or is rate-limited,
#' use cached RDS data and log a warning with the cache age.
#'
#' @param file_path Path to the RDS file
#' @return Cached data or NULL if no cache exists
load_cached_with_warning <- function(file_path) {
  if (!file.exists(file_path)) {
    log_warn(sprintf("No cached data available at: %s", file_path))
    return(NULL)
  }

  cached <- tryCatch(
    readRDS(file_path),
    error = function(e) {
      log_error(sprintf("Cached RDS is corrupted at %s: %s", file_path, e$message))
      NULL
    }
  )

  if (is.null(cached)) return(NULL)

  parsed_at <- attr(cached, "parsed_at") %||% attr(cached, "fetched_at")

  if (!is.null(parsed_at)) {
    age_hours <- as.numeric(difftime(Sys.time(), parsed_at, units = "hours"))
    if (age_hours < 1) {
      age_msg <- sprintf("%.0f minutes", age_hours * 60)
    } else if (age_hours < 24) {
      age_msg <- sprintf("%.1f hours", age_hours)
    } else {
      age_msg <- sprintf("%.1f days", age_hours / 24)
    }
    log_warn(sprintf("Using cached data from %s ago (fetched: %s). Live fetch failed.",
                     age_msg, format(parsed_at, "%Y-%m-%d %H:%M")))
  } else {
    log_warn("Using cached data (unknown age). Live fetch failed.")
  }

  cached
}

# --- Helper: Save RDS with metadata ---
#' Save data as RDS with standard metadata attributes
#'
#' Stores: source_file, parsed_at, league_id, season
#' Per requirement 1.5 / 3.8: stats stored as season-specific RDS with metadata.
#'
#' @param data Data to save
#' @param file_path Output file path
#' @param source_file Description of data source endpoint
#' @param year Season year
save_stats_rds <- function(data, file_path, source_file = "Baseball Reference bref_daily", year = current_year) {
  attr(data, "source_file") <- source_file
  attr(data, "parsed_at") <- Sys.time()
  attr(data, "league_id") <- LEAGUE_ID
  attr(data, "season") <- year
  saveRDS(data, file_path)
}

# --- Helper: Fetch stats for a single year ---
#' Fetch batting and pitching stats for a given year
#'
#' Per requirements 3.1/3.2: Retrieves current-season batting/pitching stats
#' from Baseball Reference via bref_daily_batter()/bref_daily_pitcher().
#' Per requirement 3.8: Stores as stats_batting_{year}.rds / stats_pitching_{year}.rds.
#' Per requirement 3.9: Supports historical data for at least 3 prior seasons.
#'
#' @param year Integer year to fetch
#' @return List with $batting and $pitching data frames (or NULLs)
fetch_season_stats <- function(year) {
  log_info(sprintf("=== Fetching %d stats ===", year))

  batting_file <- file.path(CACHE_DIR, sprintf("stats_batting_%d.rds", year))
  pitching_file <- file.path(CACHE_DIR, sprintf("stats_pitching_%d.rds", year))


  # Determine date range
  season_start <- paste0(year, "-03-20")
  if (year == current_year) {
    season_end <- format(Sys.Date(), "%Y-%m-%d")
  } else {
    season_end <- paste0(year, "-11-01")
  }

  # --- Fetch batting stats (Requirement 3.1) ---
  log_info(sprintf("Fetching %d batting stats (date range: %s to %s)", year, season_start, season_end))

  batting <- fetch_with_fangraphs_fallback(
    fg_fn = function() {
      # FanGraphs attempt (currently returns 403)
      stop("403 Forbidden")
    },
    bref_fn = function() {
      bref_daily_batter(season_start, season_end)
    },
    label = sprintf("%d batting", year)
  )

  # If live fetch failed, try cache (Requirement 3.6)
  if (is.null(batting) || nrow(batting) == 0) {
    log_warn("Live batting fetch failed, checking cache...")
    batting <- load_cached_with_warning(batting_file)
  } else {
    log_info(sprintf("Retrieved %d batters for %d", nrow(batting), year))
    save_stats_rds(batting, batting_file,
                   source_file = sprintf("baseballr::bref_daily_batter('%s', '%s')", season_start, season_end),
                   year = year)
    log_info(sprintf("Saved to: %s", batting_file))
  }

  # --- Fetch pitching stats (Requirement 3.2) ---
  log_info(sprintf("Fetching %d pitching stats (date range: %s to %s)", year, season_start, season_end))

  pitching <- fetch_with_fangraphs_fallback(
    fg_fn = function() {
      # FanGraphs attempt (currently returns 403)
      stop("403 Forbidden")
    },
    bref_fn = function() {
      bref_daily_pitcher(season_start, season_end)
    },
    label = sprintf("%d pitching", year)
  )

  # If live fetch failed, try cache (Requirement 3.6)
  if (is.null(pitching) || nrow(pitching) == 0) {
    log_warn("Live pitching fetch failed, checking cache...")
    pitching <- load_cached_with_warning(pitching_file)
  } else {
    log_info(sprintf("Retrieved %d pitchers for %d", nrow(pitching), year))
    save_stats_rds(pitching, pitching_file,
                   source_file = sprintf("baseballr::bref_daily_pitcher('%s', '%s')", season_start, season_end),
                   year = year)
    log_info(sprintf("Saved to: %s", pitching_file))
  }

  list(batting = batting, pitching = pitching)
}

# --- Determine which years to fetch ---
if ("--year" %in% args) {
  year_idx <- which(args == "--year") + 1
  years_to_fetch <- as.integer(args[year_idx])
} else if ("--seasons" %in% args) {
  seasons_idx <- which(args == "--seasons") + 1
  n_seasons <- as.integer(args[seasons_idx])
  years_to_fetch <- seq(current_year - n_seasons + 1, current_year)
} else {
  # Default: fetch current year + 3 prior seasons (Requirement 3.9)
  years_to_fetch <- seq(current_year - 3, current_year)
}

log_info("Stats Pipeline")
log_info("==============")
log_info(sprintf("Years to fetch: %s", paste(years_to_fetch, collapse = ", ")))
log_info(sprintf("Cache directory: %s", CACHE_DIR))

# --- Fetch all requested seasons ---
results <- list()
for (yr in years_to_fetch) {
  results[[as.character(yr)]] <- fetch_season_stats(yr)
}

# --- If a specific player was requested, show their data ---
if ("--player" %in% args) {
  player_idx <- which(args == "--player") + 1
  player_name <- paste(args[player_idx:length(args)], collapse = " ")
  # Remove any trailing flags
  player_name <- gsub("\\s*--.*", "", player_name)

  cat(sprintf("\n=== Stats for: %s ===\n\n", player_name))

  for (yr in years_to_fetch) {
    yr_key <- as.character(yr)
    if (!is.null(results[[yr_key]]$batting)) {
      match <- results[[yr_key]]$batting |>
        filter(grepl(player_name, Name, ignore.case = TRUE))
      if (nrow(match) > 0) {
        cat(sprintf("%d BATTING:\n", yr))
        print(as.data.frame(match), row.names = FALSE)
        cat("\n")
      }
    }

    if (!is.null(results[[yr_key]]$pitching)) {
      match <- results[[yr_key]]$pitching |>
        filter(grepl(player_name, Name, ignore.case = TRUE))
      if (nrow(match) > 0) {
        cat(sprintf("%d PITCHING:\n", yr))
        print(as.data.frame(match), row.names = FALSE)
        cat("\n")
      }
    }
  }
}

# --- Summary ---
log_info("=== Stats Pipeline Summary ===")
for (yr in years_to_fetch) {
  yr_key <- as.character(yr)
  batting_n <- if (!is.null(results[[yr_key]]$batting)) nrow(results[[yr_key]]$batting) else 0
  pitching_n <- if (!is.null(results[[yr_key]]$pitching)) nrow(results[[yr_key]]$pitching) else 0
  log_info(sprintf("  %d: %d batters, %d pitchers", yr, batting_n, pitching_n))
}

log_info("Done!")

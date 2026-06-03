#!/usr/bin/env Rscript
#' Fetch MLB Draft Results & Prospect Info via {baseballr}
#'
#' Usage: Rscript R/ingest/fetch_draft.R
#'        Rscript R/ingest/fetch_draft.R --year 2023
#'        Rscript R/ingest/fetch_draft.R --player "Aidan Miller"
#'        Rscript R/ingest/fetch_draft.R --seasons 5
#'
#' Output: data/cache/draft_{year}.rds (for each year fetched)
#'         data/cache/prospects_{year}.rds
#'
#' Fetches MLB amateur draft results and prospect scouting data from MLB.com.
#' Includes exponential backoff retry on failure (2s, 4s, 8s — max 3 retries),
#' fallback to cached RDS with cache-age warning, and multi-season support.
#' Useful for evaluating minor league players on your roster or trade targets.
#'
#' Error Handling:
#'   - Exponential backoff retry: 2s, 4s, 8s delays (max 3 retries)
#'   - On all retries exhausted, falls back to cached RDS if available
#'   - Warns about cache age when using stale data
#'   - FanGraphs 403 silently falls back to MLB.com sources
#'   - Adds metadata attributes (source_file, parsed_at, league_id, season)
#'
#' Requirements: 3.10, 3.11, 3.12, 3.6, 3.7, 3.9

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
                               source_label = "MLB.com") {
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

# --- Helper: Load cached RDS with age warning ---
#' Load cached RDS file, warn about cache age
#'
#' Per requirement 3.6: If a source returns an error or is rate-limited,
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
#' Consistent with fetch_players.R and fetch_stats.R metadata patterns.
#'
#' @param data Data to save
#' @param file_path Output file path
#' @param source_file Description of data source endpoint
#' @param year Data year
save_draft_rds <- function(data, file_path, source_file = "MLB.com", year = current_year) {
  attr(data, "source_file") <- source_file
  attr(data, "parsed_at") <- Sys.time()
  attr(data, "league_id") <- LEAGUE_ID
  attr(data, "season") <- year
  saveRDS(data, file_path)
}

# --- Helper: FanGraphs 403 silent fallback for prospect data ---
#' Attempt FanGraphs fetch; on 403 silently fall back to MLB.com source
#'
#' Per requirement 3.7: FanGraphs 403 errors trigger silent fallback
#' to an alternative source without failing.
#'
#' @param fg_fn FanGraphs fetch function
#' @param mlb_fn MLB.com fallback function
#' @param label Label for logging
#' @return Data from whichever source succeeds, or NULL
fetch_with_fangraphs_fallback <- function(fg_fn, mlb_fn, label = "prospects") {
  # Attempt FanGraphs first
  fg_result <- tryCatch({
    fg_fn()
  }, error = function(e) {
    is_403 <- grepl("403|Forbidden|forbidden", e$message, ignore.case = TRUE)
    if (is_403) {
      # Silent fallback — no error displayed for 403 (Requirement 3.7)
      log_info(sprintf("[FanGraphs] 403 for %s — falling back to MLB.com", label))
    } else {
      log_warn(sprintf("[FanGraphs] Error fetching %s: %s — falling back to MLB.com",
                       label, e$message))
    }
    NULL
  })

  if (!is.null(fg_result) && nrow(fg_result) > 0) {
    return(fg_result)
  }

  # Fallback to MLB.com with exponential backoff retry
  retry_with_backoff(mlb_fn, source_label = "MLB.com")
}

# --- Determine which years to fetch ---
if ("--year" %in% args) {
  year_idx <- which(args == "--year") + 1
  years <- as.integer(args[year_idx])
} else if ("--seasons" %in% args) {
  seasons_idx <- which(args == "--seasons") + 1
  n_seasons <- as.integer(args[seasons_idx])
  years <- seq(current_year - n_seasons, current_year - 1)
} else {
  # Default: fetch last 3 years of drafts (Requirement 3.9/3.10: at least 3 prior seasons)
  years <- seq(current_year - 3, current_year - 1)
}

log_info("Draft & Prospect Pipeline")
log_info("=========================")
log_info(sprintf("Years to fetch: %s", paste(years, collapse = ", ")))
log_info(sprintf("Cache directory: %s", CACHE_DIR))

# --- Fetch draft results (Requirement 3.10) ---
for (yr in years) {
  log_info(sprintf("--- %d MLB Draft ---", yr))
  draft_file <- file.path(CACHE_DIR, sprintf("draft_%d.rds", yr))

  draft <- retry_with_backoff(
    fn = function() mlb_draft(yr),
    source_label = sprintf("MLB.com Draft %d", yr)
  )

  if (!is.null(draft) && nrow(draft) > 0) {
    log_info(sprintf("Retrieved %d picks for %d draft", nrow(draft), yr))
    save_draft_rds(draft, draft_file,
                   source_file = sprintf("baseballr::mlb_draft(%d)", yr),
                   year = yr)
    log_info(sprintf("Saved to: %s", draft_file))
  } else {
    log_warn(sprintf("Live fetch failed for %d draft, checking cache...", yr))
    cached_draft <- load_cached_with_warning(draft_file)
    if (!is.null(cached_draft)) {
      log_info(sprintf("Using cached %d draft data (%d picks)", yr, nrow(cached_draft)))
    } else {
      log_warn(sprintf("No draft data available for %d", yr))
    }
  }
}

# --- Fetch prospect rankings (Requirement 3.11) ---
log_info(sprintf("--- %d Prospect Rankings ---", current_year))
prospects_file <- file.path(CACHE_DIR, sprintf("prospects_%d.rds", current_year))

prospects <- fetch_with_fangraphs_fallback(
  fg_fn = function() {
    # FanGraphs MiLB prospect data (currently returns 403)
    # Per Requirement 3.14: when FanGraphs MiLB endpoints become accessible,
    # retrieve minor league game logs here
    stop("403 Forbidden")
  },
  mlb_fn = function() {
    mlb_draft_prospects(year = current_year)
  },
  label = sprintf("%d prospects", current_year)
)

if (!is.null(prospects) && nrow(prospects) > 0) {
  log_info(sprintf("Retrieved %d prospect entries for %d", nrow(prospects), current_year))
  save_draft_rds(prospects, prospects_file,
                 source_file = sprintf("baseballr::mlb_draft_prospects(%d)", current_year),
                 year = current_year)
  log_info(sprintf("Saved to: %s", prospects_file))
} else {
  # Try previous year as fallback
  log_info("Current year prospects unavailable, trying previous year...")
  prospects_prev_file <- file.path(CACHE_DIR, sprintf("prospects_%d.rds", current_year - 1))

  prospects <- retry_with_backoff(
    fn = function() mlb_draft_prospects(year = current_year - 1),
    source_label = sprintf("MLB.com Prospects %d", current_year - 1)
  )

  if (!is.null(prospects) && nrow(prospects) > 0) {
    log_info(sprintf("Retrieved %d prospect entries (from %d)", nrow(prospects), current_year - 1))
    save_draft_rds(prospects, prospects_prev_file,
                   source_file = sprintf("baseballr::mlb_draft_prospects(%d)", current_year - 1),
                   year = current_year - 1)
    log_info(sprintf("Saved to: %s", prospects_prev_file))
  } else {
    # Final fallback: load any cached prospects
    log_warn("Live prospect fetch failed, checking cache...")
    cached_prospects <- load_cached_with_warning(prospects_file)
    if (is.null(cached_prospects)) {
      cached_prospects <- load_cached_with_warning(prospects_prev_file)
    }
    if (!is.null(cached_prospects)) {
      prospects <- cached_prospects
      log_info(sprintf("Using cached prospect data (%d entries)", nrow(prospects)))
    } else {
      log_warn("No prospect data available")
    }
  }
}

# --- Player lookup if requested (Requirement 3.12/3.13) ---
if ("--player" %in% args) {
  player_idx <- which(args == "--player") + 1
  player_name <- paste(args[player_idx:length(args)], collapse = " ")
  player_name <- gsub("\\s*--.*", "", player_name) |> trimws()

  cat(sprintf("\n=== Draft History for: %s ===\n\n", player_name))

  # Search all fetched/cached drafts
  for (yr in years) {
    draft_file <- file.path(CACHE_DIR, sprintf("draft_%d.rds", yr))
    if (file.exists(draft_file)) {
      draft <- readRDS(draft_file)
      matches <- draft |> filter(grepl(player_name, person_full_name, ignore.case = TRUE))
      if (nrow(matches) > 0) {
        cat(sprintf("%d Draft:\n", yr))
        key_cols <- intersect(c("person_full_name", "pick_number", "pick_round",
          "team_name", "school_name", "position_short", "signing_bonus"), names(matches))
        print(as.data.frame(matches[, key_cols]), row.names = FALSE)
        cat("\n")
      }
    }
  }

  # Also look up player info from MLB.com (Requirement 3.12)
  cat("Player registry lookup:\n")
  parts <- strsplit(player_name, " ")[[1]]
  if (length(parts) >= 2) {
    lookup <- tryCatch({
      playerid_lookup(parts[length(parts)], parts[1])
    }, error = function(e) {
      log_warn(sprintf("Player ID lookup failed for '%s': %s", player_name, e$message))
      NULL
    })
    if (!is.null(lookup) && nrow(lookup) > 0) {
      relevant <- lookup |> filter(!is.na(mlbam_id))
      if (nrow(relevant) > 0) {
        print(as.data.frame(relevant |> select(first_name, last_name, mlbam_id, birth_year)))

        # Get detailed bio info from MLB.com (Requirement 3.12)
        for (mid in relevant$mlbam_id) {
          info <- tryCatch(mlb_people(person_ids = mid), error = function(e) {
            log_warn(sprintf("MLB people lookup failed for ID %s: %s", mid, e$message))
            NULL
          })
          if (!is.null(info) && nrow(info) > 0) {
            cat(sprintf("\n  %s - %s, %s\n  Born: %s (Age %d) | Bats: %s Throws: %s\n",
              info$full_name[1],
              info$primary_position_abbreviation[1],
              info$current_team_name[1] %||% "Unknown",
              info$birth_date[1], info$current_age[1],
              info$bat_side_code[1], info$pitch_hand_code[1]))
          }
        }
      }
    }
  }
}

# --- Summary ---
log_info("=== Draft Pipeline Summary ===")
for (yr in years) {
  draft_file <- file.path(CACHE_DIR, sprintf("draft_%d.rds", yr))
  if (file.exists(draft_file)) {
    d <- readRDS(draft_file)
    log_info(sprintf("  %d Draft: %d picks", yr, nrow(d)))
  } else {
    log_info(sprintf("  %d Draft: no data", yr))
  }
}
if (!is.null(prospects)) {
  log_info(sprintf("  Prospects: %d entries", nrow(prospects)))
}

log_info("Done!")

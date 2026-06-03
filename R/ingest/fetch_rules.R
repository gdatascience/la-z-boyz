#!/usr/bin/env Rscript
#' Fetch CBS League Rules (no authentication required)
#'
#' Usage: Rscript R/ingest/fetch_rules.R
#'
#' Output: data/cache/league_rules.rds
#'
#' Pulls the full league configuration from the CBS API as a parsed R list.
#'
#' Error Handling:
#'   - 60-second timeout on CBS API call
#'   - On error/timeout, logs with timestamp and falls back to cached RDS
#'   - Validates rules structure after fetch (scoring, roster, transactions)
#'   - Adds metadata attributes (source_file, parsed_at, league_id) to saved RDS

library(httr2)
library(xml2)

# --- Config ---
league_id <- "l-z-bs"
base_url <- paste0("https://", league_id, ".baseball.cbssports.com")
output_file <- "data/cache/league_rules.rds"

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

#' Load cached rules RDS if available
#' @return Rules list or NULL if no cache exists
load_cached_rules <- function() {
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

#' Validate rules structure contains required sections
#' @param rules List to validate
#' @return TRUE if valid, FALSE otherwise
validate_rules <- function(rules) {
  if (!is.list(rules)) {
    log_error("Rules data is not a list")
    return(FALSE)
  }

  # Check required top-level sections

required_sections <- c("scoring", "roster", "transactions")
  missing_sections <- setdiff(required_sections, names(rules))
  if (length(missing_sections) > 0) {
    log_error(paste("Missing required sections:", paste(missing_sections, collapse = ", ")))
    return(FALSE)
  }

  # Check scoring section has a type
  if (is.null(rules$scoring$type) || nchar(rules$scoring$type) == 0) {
    log_warn("Scoring type is empty — rules may be incomplete")
  }

  # Check roster has positions
  if (is.null(rules$roster$positions) || length(rules$roster$positions) == 0) {
    log_error("Roster positions are missing")
    return(FALSE)
  }

  # Check transactions section has FAAB info
  if (is.null(rules$transactions$faab_budget)) {
    log_warn("FAAB budget not found in transactions — rules may be incomplete")
  }

  TRUE
}

# --- Fetch with error handling ---
log_info("Fetching league rules from CBS API...")

rules <- tryCatch(
  {
    resp <- request(paste0(base_url, "/api/league/rules")) |>
      req_timeout(60) |>
      req_perform()

    body <- resp_body_string(resp)
    doc <- read_xml(body)

    # --- Parse into structured R list ---
    parsed_rules <- list()

    # Scoring system
    scoring_node <- xml_find_first(doc, ".//scoring_system")
    parsed_rules$scoring <- list(
      type = xml_text(xml_find_first(scoring_node, ".//type")),
      description = xml_text(xml_find_first(scoring_node, ".//scoring_system")),
      win_determination = xml_text(xml_find_first(scoring_node, ".//win_determination")),
      scoring_period = xml_text(xml_find_first(scoring_node, ".//scoring_period")),
      matchup_tiebreaker = xml_text(xml_find_first(scoring_node, ".//matchup_tiebreaker")),
      standings_tiebreaker = xml_text(xml_find_first(scoring_node, ".//standings_tiebreaker"))
    )

    # Roster positions
    positions <- xml_find_all(doc, ".//roster/positions/position")
    parsed_rules$roster$positions <- lapply(positions, function(pos) {
      list(
        abbr = xml_text(xml_find_first(pos, ".//abbr")),
        max_active = as.integer(xml_text(xml_find_first(pos, ".//max_active"))),
        min_active = as.integer(xml_text(xml_find_first(pos, ".//min_active")))
      )
    })

    # Roster limits
    statuses <- xml_find_all(doc, ".//roster/statuses/status")
    parsed_rules$roster$limits <- lapply(statuses, function(s) {
      list(
        description = xml_text(xml_find_first(s, ".//description")),
        max = as.integer(xml_text(xml_find_first(s, ".//max"))),
        min = as.integer(xml_text(xml_find_first(s, ".//min")))
      )
    })

    parsed_rules$roster$minor_league_exempt <- xml_text(
      xml_find_first(doc, ".//minor_league_players_dont_count_against_roster_limits")
    ) == "1"

    parsed_rules$roster$illegal_rosters_score_zero <- xml_text(
      xml_find_first(doc, ".//illegal_rosters_score_zero")
    ) == "1"

    # Transactions
    parsed_rules$transactions <- list(
      add_drop_policy = xml_text(xml_find_first(doc, ".//add_drop_policy/value")),
      faab_budget = xml_text(xml_find_first(doc, ".//add_drop_faab_starting_budget/value")),
      faab_min_bid = xml_text(xml_find_first(doc, ".//add_drop_faab_minimum_offer/value")),
      faab_zero_bids_allowed = xml_text(
        xml_find_first(doc, ".//add_drop_faab_zero_bids/value")) == "1",
      waiver_period_days = xml_text(xml_find_first(doc, ".//add_drop_waiver_period/value")),
      trade_policy = xml_text(xml_find_first(doc, ".//trade_policy/value")),
      trade_deadline = xml_text(xml_find_first(doc, ".//trade_deadline/value")),
      offseason_trades = xml_text(xml_find_first(doc, ".//trade_post_season/value")) == "1",
      lineup_deadline = xml_text(xml_find_first(doc, ".//lineup_deadline/value")),
      lineup_effective = xml_text(xml_find_first(doc, ".//lineup_effective_date/value"))
    )

    # Eligibility
    parsed_rules$eligibility <- list(
      player = xml_text(xml_find_first(doc, ".//eligibility/player_elig")),
      sp = xml_text(xml_find_first(doc, ".//eligibility/sp_elig")),
      rp = xml_text(xml_find_first(doc, ".//eligibility/rp_elig"))
    )

    # Player pool
    parsed_rules$player_pool <- xml_text(xml_find_first(doc, ".//player_pool/value"))

    # Fees
    parsed_rules$entry_fee <- as.numeric(xml_text(xml_find_first(doc, ".//entry_fee/value")))

    # Store raw XML for reference
    attr(parsed_rules, "raw_xml") <- body

    parsed_rules
  },
  error = function(e) {
    log_error(sprintf("CBS API request failed: %s", e$message))
    log_info("Attempting to use cached data...")
    cached <- load_cached_rules()
    if (is.null(cached)) {
      stop("No cached data available. Cannot proceed without league rules.")
    }
    cached
  }
)

# --- Validate ---
if (!validate_rules(rules)) {
  log_error("Validation failed for fetched rules data. Attempting cache fallback...")
  cached <- load_cached_rules()
  if (!is.null(cached) && validate_rules(cached)) {
    rules <- cached
    log_info("Using previously cached valid rules data")
  } else {
    stop("No valid rules data available (fetch failed validation and no valid cache)")
  }
}

# --- Add metadata attributes ---
attr(rules, "source_file") <- paste0(base_url, "/api/league/rules")
attr(rules, "parsed_at") <- Sys.time()
attr(rules, "league_id") <- league_id

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(rules, output_file)

# --- Report ---
log_info("League rules parsed!")
log_info(sprintf("  Scoring: %s", rules$scoring$description %||% "(not available)"))
log_info(sprintf("  Roster positions: %s",
  paste(sapply(rules$roster$positions, `[[`, "abbr"), collapse = ", ")))
log_info(sprintf("  FAAB budget: %s", rules$transactions$faab_budget %||% "(not available)"))
log_info(sprintf("  Trade deadline: %s", rules$transactions$trade_deadline %||% "(not available)"))
log_info(sprintf("  Entry fee: $%s", rules$entry_fee %||% "?"))
log_info(sprintf("  Saved to: %s", output_file))
log_info(sprintf("  Metadata: source_file=%s, parsed_at=%s, league_id=%s",
  attr(rules, "source_file"),
  format(attr(rules, "parsed_at"), "%Y-%m-%d %H:%M:%S"),
  attr(rules, "league_id")))

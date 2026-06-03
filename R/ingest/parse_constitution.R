#!/usr/bin/env Rscript
#' Parse Constitution from Saved CBS HTML (one-time operation)
#'
#' Usage: Rscript R/ingest/parse_constitution.R
#'
#' Prerequisites:
#'   1. Save the league details/constitution page as HTML
#'   2. Place the file at data/imports/constitution.html
#'
#' Output: data/league_constitution.rds (committed to git — does not change)

library(rvest)

# --- Validation ---

#' Validate that a constitution list contains all required fields
#'
#' Checks that the constitution has the complete set of fields needed by all
#' downstream analysis modules (scoring, salary, keepers, roster, playoffs, etc.)
#'
#' @param constitution A list representing the parsed league constitution
#' @return TRUE if all required fields are present. If invalid, returns FALSE
#'   with an attribute "missing_fields" containing a character vector of the
#'   missing field paths (e.g., "scoring$batting", "salary$auction_cap").
validate_constitution <- function(constitution) {
  # Define all required field paths that downstream modules depend on

  required_fields <- list(
    # Scoring weights
    list(path = c("scoring", "batting"), label = "scoring$batting"),
    list(path = c("scoring", "pitching"), label = "scoring$pitching"),
    # Salary caps
    list(path = c("salary", "auction_cap"), label = "salary$auction_cap"),
    list(path = c("salary", "in_season_cap"), label = "salary$in_season_cap"),
    list(path = c("salary", "keeper_cap"), label = "salary$keeper_cap"),
    # Keeper rules
    list(path = c("keepers", "annual_increase"), label = "keepers$annual_increase"),
    # Minor league rules
    list(path = c("minor_league", "promotion_threshold_ab"), label = "minor_league$promotion_threshold_ab"),
    list(path = c("minor_league", "promotion_threshold_ip"), label = "minor_league$promotion_threshold_ip"),
    # Roster construction
    list(path = c("roster", "positions"), label = "roster$positions"),
    # Playoff structure
    list(path = c("playoffs"), label = "playoffs"),
    # FAAB rules
    list(path = c("transactions", "faab_process"), label = "transactions$faab_process"),
    # Draft structure
    list(path = c("drafts", "auction"), label = "drafts$auction"),
    list(path = c("drafts", "minor_league"), label = "drafts$minor_league")
  )

  missing <- character(0)

  for (field in required_fields) {
    # Navigate the nested list using the path components
    value <- constitution
    found <- TRUE
    for (key in field$path) {
      if (is.list(value) && key %in% names(value)) {
        value <- value[[key]]
      } else {
        found <- FALSE
        break
      }
    }
    if (!found || is.null(value)) {
      missing <- c(missing, field$label)
    }
  }

  if (length(missing) == 0) {
    return(TRUE)
  } else {
    result <- FALSE
    attr(result, "missing_fields") <- missing
    return(result)
  }
}

#' Load the league constitution from RDS with error handling
#'
#' Attempts to load the constitution RDS file, validating its integrity.
#' Displays an informative error if the file is missing or corrupted.
#'
#' @param path Path to the league_constitution.rds file
#'   (default: "data/league_constitution.rds")
#' @return The constitution list if valid, or stops with an error message
load_constitution <- function(path = "data/league_constitution.rds") {
  # Check if file exists

  if (!file.exists(path)) {
    stop(
      "Constitution RDS file not found at: ", path, "\n",
      "  To generate it, run: Rscript R/ingest/parse_constitution.R\n",
      "  Prerequisites:\n",
      "    1. Save the CBS league constitution page as HTML\n",
      "    2. Place it at data/imports/constitution.html\n",
      "    3. Run the parse script to generate the RDS"
    )
  }

  # Attempt to read the RDS file (handle corrupted files)
  constitution <- tryCatch(
    readRDS(path),
    error = function(e) {
      stop(
        "Constitution RDS file is corrupted or unreadable: ", path, "\n",
        "  Error: ", conditionMessage(e), "\n",
        "  To fix, delete the file and re-run: Rscript R/ingest/parse_constitution.R"
      )
    }
  )

  # Validate structure

  valid <- validate_constitution(constitution)
  if (!isTRUE(valid)) {
    missing <- attr(valid, "missing_fields")
    stop(
      "Constitution RDS is missing required fields:\n",
      paste("  -", missing, collapse = "\n"), "\n",
      "  Re-parse the constitution: Rscript R/ingest/parse_constitution.R"
    )
  }

  constitution
}

# --- Config ---
import_file <- "data/imports/constitution.html"
output_file <- "data/league_constitution.rds"

# --- Validate input ---
if (!file.exists(import_file)) {
  stop("No constitution HTML file found at: ", import_file)
}

# --- Parse ---
message("Parsing constitution from: ", import_file)
page <- read_html(import_file)
body <- page |> html_element("body")
xml2::xml_remove(body |> html_elements("script"))
xml2::xml_remove(body |> html_elements("style"))
full_text <- html_text2(body)

# --- Extract constitution section ---
const_start <- regexpr("CONSTITUTION", full_text)
if (const_start > 0) {
  # Get from CONSTITUTION to end of rules (before footer)
  const_text <- substring(full_text, const_start)
  # Trim at footer
  footer_markers <- c("Refer to the Terms of Use", "More\nPick'em Games", "© 2004")
  for (marker in footer_markers) {
    pos <- regexpr(marker, const_text, fixed = TRUE)
    if (pos > 0) {
      const_text <- substring(const_text, 1, pos - 1)
      break
    }
  }
} else {
  const_text <- full_text
}

# --- Build structured constitution ---
constitution <- list()

# League identity
constitution$league_name <- "La-Z-Boyz of Summer"
constitution$league_url <- "https://l-z-bs.baseball.cbssports.com"
constitution$teams <- 16
constitution$divisions <- 4
constitution$entry_fee <- 100

# Salary structure
constitution$salary <- list(
  auction_cap = 260,
  in_season_cap = 300,
  keeper_cap = 80,
  keeper_increase_per_year = 4,
  minor_league_track = c(0, 1, 2, 3),  # $0 in minors, then $1*, $2*, $3*
  faab_budget = 250,
  min_faab_bid = 1,
  zero_bids_allowed = FALSE
)

# Keeper rules
constitution$keepers <- list(
  limit = "unlimited",
  salary_cap = 80,
  annual_increase = 4,
  minor_league_salary_track = list(
    year_0 = "$0 (in minors)",
    year_1 = "$1* (first full MLB season)",
    year_2 = "$2* (second full season)",
    year_3 = "$3* (third full season)",
    year_4_plus = "enters +$4/year keeper track"
  ),
  keeper_deadline = "03/17 (annually)",
  free_agent_keeper_rule = "Players picked up after trade deadline (Jul 31) NOT available as keeper",
  free_agent_keeper_salary = "Current salary + $4"
)

# Minor league rules
constitution$minor_league <- list(
  roster_spots = 5,
  max_minor_leaguers = 5,
  promotion_threshold_ab = 130,
  promotion_threshold_ip = 50,
  promotion_note = "Once threshold reached, must be promoted or dropped within 5 days or start of next scoring period",
  salary_in_minors = 0,
  salary_on_promotion = 1,
  can_return_to_minors = FALSE,
  dropped_minors_lose_salary_track = TRUE,
  no_free_agency_for_minors = TRUE,
  trades_allowed = TRUE
)

# Draft structure
constitution$drafts <- list(
  auction = list(
    type = "Live Salary Cap Draft",
    budget = 260,
    roster_size = 22,
    min_per_player = 1
  ),
  minor_league = list(
    rounds = 5,
    order = "Worst record first, playoff teams last",
    eligible = "MLB rookie status players",
    timing = "After auction draft"
  ),
  june = list(
    rounds = 3,
    eligible = "Current season MLB amateur draft picks only",
    order = "Same as minor league draft",
    timing = "1-2 weeks after MLB amateur draft"
  )
)

# Transactions
constitution$transactions <- list(
  trade_deadline = "July 31 (same as MLB)",
  trade_approval = "Commissioner",
  offseason_trades = TRUE,
  no_salary_cap_trading = TRUE,
  no_draft_trading = TRUE,
  traded_salary_transfers = TRUE,
  faab_process = "Tuesday, Friday, Sunday at 11 PM ET",
  dropped_player_waiver = "1 day",
  playoffs_non_contenders = "Cannot acquire via FAAB"
)

# Scoring
constitution$scoring <- list(
  type = "Head-to-Head Points",
  period = "Weekly (Monday to Sunday)",
  batting = list(
    singles = 1, doubles = 2, triples = 3, hr = 4,
    grand_slam_bonus = 2, cycle = 5,
    runs = 1, rbi = 1, bb = 1, hbp = 1,
    sb = 2, cs = -1, k_batter = -0.5
  ),
  pitching = list(
    innings = 3, k_pitcher = 0.5,
    wins = 5, losses = -5, saves = 7, holds = 5,
    quality_starts = 3, complete_games = 2,
    no_hitter = 5, perfect_game = 5,
    hits_allowed = -1, earned_runs = -1,
    walks_issued = -1, intentional_walks = 1
  )
)

# Roster
constitution$roster <- list(
  active = 16,
  reserve = 6,
  minors = 5,
  total = 27,
  positions = list(
    C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
    OF = 3, U = 1, SP = 5, RP = 2
  )
)

# Schedule / Playoffs
constitution$playoffs <- list(
  regular_season_weeks = 23,
  playoff_start_week = 24,
  playoff_duration_weeks = 3,
  qualifiers = "4 division winners + 2 wildcards (highest overall points)",
  top_2_seeds_bye = TRUE,
  seeding = "Win-Loss record, TB: Total Points, then Points Against"
)

# Prize pool
constitution$prizes <- list(
  total_collected = 1600,
  league_fees = 160,
  total_pool = 1440,
  first_place = 600,
  second_place = 300,
  third_place = 150,
  fourth_place = 100,
  weekly_high_score = 10,
  weekly_high_score_weeks = 23,
  season_highest_weekly = 30,
  season_highest_total = 30
)

# Store the raw text too
constitution$raw_text <- const_text

# --- Validate before saving ---
valid <- validate_constitution(constitution)
if (!isTRUE(valid)) {
  missing <- attr(valid, "missing_fields")
  stop(
    "Parsed constitution is missing required fields:\n",
    paste("  -", missing, collapse = "\n"), "\n",
    "  Check the HTML source for completeness."
  )
}
message("Validation passed: all required fields present.")

# --- Add metadata ---
attr(constitution, "parsed_at") <- Sys.time()
attr(constitution, "source_file") <- import_file

# --- Save ---
saveRDS(constitution, output_file)

# --- Report ---
message("\nConstitution parsed and structured!")
message("  League: ", constitution$league_name)
message("  Salary caps: Auction $", constitution$salary$auction_cap,
        " / In-Season $", constitution$salary$in_season_cap,
        " / Keeper $", constitution$salary$keeper_cap)
message("  Keeper increase: +$", constitution$keepers$annual_increase, "/year")
message("  Minor league track: $0 → $1* → $2* → $3* then +$4/yr")
message("  Playoffs: ", constitution$playoffs$qualifiers)
message("\nSaved to: ", output_file)
message("\nNOTE: This file is committed to git (constitution doesn't change)")

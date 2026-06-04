#' Fetch/Maintain Prospect Rankings
#'
#' Provides a manually-maintained lookup table of industry prospect rankings
#' from sources like FanGraphs, MLB Pipeline, and Just Baseball. Since these
#' rankings are not available via API, this module stores them as a structured
#' data frame that can be updated periodically.
#'
#' The prospect rankings fill a critical gap: players without MLB stats get $0
#' valuations from the projection model, but may be elite assets based on
#' industry consensus (e.g., Thomas White, Zyhir Hope, Konnor Griffin).
#'
#' Usage:
#'   rankings <- load_prospect_rankings()
#'   rankings <- update_prospect_ranking("Thomas White", rank = 14, ...)
#'   save_prospect_rankings(rankings)

# --- Constants ---

PROSPECT_RANKINGS_FILE <- "data/cache/prospect_rankings.rds"

#' Valid prospect tier values
VALID_TIERS <- c("elite", "high", "mid", "low")

#' Valid ranking sources
VALID_SOURCES <- c("MLB Pipeline", "FanGraphs", "Just Baseball",
                   "Baseball America", "Manual")

# --- Logging ---

log_info_prospect <- function(msg) {

  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# --- Core Functions ---

#' Create an empty prospect rankings data frame
#'
#' @return Data frame with proper column structure and zero rows
#' @keywords internal
create_empty_rankings <- function() {
  data.frame(
    player_name     = character(0),
    rank            = integer(0),
    source          = character(0),
    tier            = character(0),
    fv_grade        = integer(0),
    age             = integer(0),
    position        = character(0),
    mlb_team        = character(0),
    level           = character(0),
    eta             = integer(0),
    notes           = character(0),
    updated_at      = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
}


#' Load prospect rankings from cache
#'
#' Returns the cached prospect rankings data frame. If no file exists,
#' returns an empty data frame with the correct structure.
#'
#' @param file Path to .rds file (default: data/cache/prospect_rankings.rds)
#' @return Data frame of prospect rankings
#' @export
load_prospect_rankings <- function(file = NULL) {
  if (is.null(file)) {
    file <- if (requireNamespace("here", quietly = TRUE)) {
      here::here(PROSPECT_RANKINGS_FILE)
    } else {
      PROSPECT_RANKINGS_FILE
    }
  }

  if (file.exists(file)) {
    rankings <- readRDS(file)
    log_info_prospect(sprintf("Loaded %d prospect rankings from %s", nrow(rankings), file))
    return(rankings)
  }

  log_info_prospect("No prospect rankings file found; returning empty frame")
  create_empty_rankings()
}


#' Save prospect rankings to cache
#'
#' @param rankings Data frame of prospect rankings
#' @param file Path to .rds file (default: data/cache/prospect_rankings.rds)
#' @export
save_prospect_rankings <- function(rankings, file = NULL) {
  if (is.null(file)) {
    file <- if (requireNamespace("here", quietly = TRUE)) {
      here::here(PROSPECT_RANKINGS_FILE)
    } else {
      PROSPECT_RANKINGS_FILE
    }
  }

  saveRDS(rankings, file)
  log_info_prospect(sprintf("Saved %d prospect rankings to %s", nrow(rankings), file))
}


#' Add or update a prospect ranking
#'
#' If the player already exists in the rankings (by name + source), the
#' entry is updated. Otherwise, a new row is appended.
#'
#' @param rankings Existing rankings data frame
#' @param player_name Character: player name (use CBS roster spelling)
#' @param rank Integer: overall rank in the source's list (1-100+)
#' @param source Character: ranking source (e.g., "MLB Pipeline", "FanGraphs")
#' @param tier Character: one of "elite", "high", "mid", "low"
#' @param fv_grade Integer: FanGraphs Future Value grade (20-80 scale), or NA
#' @param age Integer: player's current age
#' @param position Character: primary position
#' @param mlb_team Character: MLB team abbreviation
#' @param level Character: current MiLB level (e.g., "AAA", "AA", "A+", "MLB")
#' @param eta Integer: expected MLB debut year
#' @param notes Character: free-text notes (injury flags, etc.)
#' @return Updated rankings data frame
#' @export
update_prospect_ranking <- function(rankings, player_name, rank, source,
                                     tier = NULL, fv_grade = NA_integer_,
                                     age = NA_integer_, position = NA_character_,
                                     mlb_team = NA_character_, level = NA_character_,
                                     eta = NA_integer_, notes = NA_character_) {
  # Input validation

  if (is.null(player_name) || nchar(trimws(player_name)) == 0) {
    stop("player_name must be non-empty", call. = FALSE)
  }
  if (!is.numeric(rank) || rank < 1) {
    stop("rank must be a positive integer", call. = FALSE)
  }
  if (!source %in% VALID_SOURCES) {
    stop(sprintf("source must be one of: %s", paste(VALID_SOURCES, collapse = ", ")),
         call. = FALSE)
  }

  # Auto-assign tier from rank if not provided

  if (is.null(tier)) {
    tier <- rank_to_tier(rank)
  }
  if (!tier %in% VALID_TIERS) {
    stop(sprintf("tier must be one of: %s", paste(VALID_TIERS, collapse = ", ")),
         call. = FALSE)
  }

  new_row <- data.frame(
    player_name = player_name,
    rank        = as.integer(rank),
    source      = source,
    tier        = tier,
    fv_grade    = as.integer(fv_grade),
    age         = as.integer(age),
    position    = as.character(position),
    mlb_team    = as.character(mlb_team),
    level       = as.character(level),
    eta         = as.integer(eta),
    notes       = as.character(notes),
    updated_at  = Sys.Date(),
    stringsAsFactors = FALSE
  )

  # Check if entry already exists (same player + source)
  existing_idx <- which(
    tolower(rankings$player_name) == tolower(player_name) &
    rankings$source == source
  )

  if (length(existing_idx) > 0) {
    # Update existing
    rankings[existing_idx[1], ] <- new_row
    log_info_prospect(sprintf("Updated: %s (%s #%d)", player_name, source, rank))
  } else {
    # Append new
    rankings <- rbind(rankings, new_row)
    log_info_prospect(sprintf("Added: %s (%s #%d)", player_name, source, rank))
  }

  rankings
}


#' Convert a rank number to a prospect tier
#'
#' @param rank Integer: overall prospect rank
#' @return Character: tier ("elite", "high", "mid", "low")
#' @export
rank_to_tier <- function(rank) {
  if (is.na(rank)) return("low")
  if (rank <= 10) return("elite")
  if (rank <= 30) return("high")
  if (rank <= 75) return("mid")
  return("low")
}


#' Look up a player's prospect tier from rankings
#'
#' Searches by normalized name. If multiple sources have rankings for the
#' same player, returns the best (lowest) rank and its corresponding tier.
#'
#' @param player_name Character: player name to look up
#' @param rankings Data frame of prospect rankings (from load_prospect_rankings())
#' @return List with: tier, best_rank, source, fv_grade, notes (or NULL if not found)
#' @export
lookup_prospect <- function(player_name, rankings) {
  if (is.null(rankings) || nrow(rankings) == 0) return(NULL)
  if (is.null(player_name) || is.na(player_name)) return(NULL)

  matches <- rankings[tolower(rankings$player_name) == tolower(player_name), , drop = FALSE]

  if (nrow(matches) == 0) return(NULL)

  # Use best (lowest) rank among sources
  best_idx <- which.min(matches$rank)
  best <- matches[best_idx, , drop = FALSE]

  list(
    player_name = best$player_name,
    tier        = best$tier,
    best_rank   = best$rank,
    source      = best$source,
    fv_grade    = best$fv_grade,
    age         = best$age,
    position    = best$position,
    level       = best$level,
    eta         = best$eta,
    notes       = best$notes
  )
}


#' Bulk-load prospect rankings from a data frame
#'
#' Convenience function for loading multiple rankings at once (e.g., from
#' a manually-maintained CSV or after web research).
#'
#' @param rankings_df Data frame with at minimum: player_name, rank, source.
#'   Optional: tier, fv_grade, age, position, mlb_team, level, eta, notes.
#' @return Prospect rankings data frame ready for save_prospect_rankings()
#' @export
bulk_load_rankings <- function(rankings_df) {
  if (is.null(rankings_df) || nrow(rankings_df) == 0) {
    return(create_empty_rankings())
  }

  required <- c("player_name", "rank", "source")
  missing <- setdiff(required, names(rankings_df))
  if (length(missing) > 0) {
    stop(sprintf("rankings_df missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  # Add missing optional columns with NA defaults
  optional_cols <- list(
    tier       = NA_character_,
    fv_grade   = NA_integer_,
    age        = NA_integer_,
    position   = NA_character_,
    mlb_team   = NA_character_,
    level      = NA_character_,
    eta        = NA_integer_,
    notes      = NA_character_,
    updated_at = Sys.Date()
  )

  for (col in names(optional_cols)) {
    if (!col %in% names(rankings_df)) {
      rankings_df[[col]] <- optional_cols[[col]]
    }
  }

  # Auto-assign tier from rank where missing
  missing_tier <- is.na(rankings_df$tier)
  rankings_df$tier[missing_tier] <- vapply(
    rankings_df$rank[missing_tier], rank_to_tier, character(1)
  )

  # Ensure updated_at is populated
  if (all(is.na(rankings_df$updated_at))) {
    rankings_df$updated_at <- Sys.Date()
  }

  rankings_df[, c("player_name", "rank", "source", "tier", "fv_grade",
                  "age", "position", "mlb_team", "level", "eta",
                  "notes", "updated_at")]
}

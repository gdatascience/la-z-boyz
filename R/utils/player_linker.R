#' Player Linker — Crosswalk between CBS, Baseball Reference, and MLBAM IDs
#'
#' Provides fuzzy name matching between CBS roster data and Baseball Reference
#' stats, plus MLBAM ID lookup via {baseballr}.
#'
#' Handles common name discrepancies:
#'   - Suffixes: Jr., Sr., II, III, IV
#'   - Accented characters: José → Jose, Ramírez → Ramirez
#'   - Abbreviated first names: "J.D." vs "JD", "A.J." vs "AJ"
#'   - Team name mapping: CBS abbreviations vs BRef full city names
#'
#' Requirements: 3.4, 3.5


# =============================================================================
# MLB Team Abbreviation ↔ Full Name Mapping
# =============================================================================

#' Mapping from CBS team abbreviations to Baseball Reference team names
#' @return Named character vector (abbreviation → full city/team name patterns)
get_team_mapping <- function() {

  c(
    "ARI" = "Arizona",
    "ATL" = "Atlanta",
    "BAL" = "Baltimore",
    "BOS" = "Boston",
    "CHC" = "Chicago",
    "CIN" = "Cincinnati",
    "CLE" = "Cleveland",
    "COL" = "Colorado",
    "CHW" = "Chicago",
    "CWS" = "Chicago",
    "DET" = "Detroit",
    "HOU" = "Houston",
    "KC"  = "Kansas City",
    "LAA" = "Los Angeles",
    "LAD" = "Los Angeles",
    "MIA" = "Miami",
    "MIL" = "Milwaukee",
    "MIN" = "Minnesota",
    "NYM" = "New York",
    "NYY" = "New York",
    "OAK" = "Oakland",
    "PHI" = "Philadelphia",
    "PIT" = "Pittsburgh",
    "SD"  = "San Diego",
    "SEA" = "Seattle",
    "SF"  = "San Francisco",
    "STL" = "St. Louis",
    "TB"  = "Tampa Bay",
    "TEX" = "Texas",
    "TOR" = "Toronto",
    "WAS" = "Washington"
  )
}


# =============================================================================
# Name Normalization Helpers
# =============================================================================

#' Normalize a player name for matching
#'
#' Strips suffixes (Jr., Sr., II, III, IV), removes accents, lowercases,
#' standardizes punctuation in abbreviated names, and collapses whitespace.
#'
#' @param name Character: player name
#' @return Character: normalized name
normalize_name <- function(name) {
  if (is.na(name) || nchar(trimws(name)) == 0) return(NA_character_)

  n <- trimws(name)

  # Remove common suffixes

  n <- gsub("\\s+(Jr\\.?|Sr\\.?|II|III|IV|V)\\s*$", "", n, ignore.case = TRUE)

  # Remove accents (transliterate to ASCII)
  n <- iconv(n, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

  # Lowercase
  n <- tolower(n)

  # Standardize abbreviated names: remove periods from initials (J.D. -> JD)
  n <- gsub("\\.", "", n)

  # Collapse multiple spaces
  n <- gsub("\\s+", " ", n)

  trimws(n)
}


#' Check if two team identifiers could refer to the same MLB team
#'
#' Handles CBS abbreviation vs BRef full city name comparison.
#' Also handles cases where both are abbreviations or both are full names.
#'
#' @param team_a Character: team from source A (could be abbreviation or full)
#' @param team_b Character: team from source B (could be abbreviation or full)
#' @return Logical: TRUE if teams match, FALSE otherwise, NA if either is NA
teams_match <- function(team_a, team_b) {
  if (is.na(team_a) || is.na(team_b)) return(NA)
  if (nchar(trimws(team_a)) == 0 || nchar(trimws(team_b)) == 0) return(NA)

  a <- trimws(team_a)
  b <- trimws(team_b)

  # Direct match (case-insensitive)
  if (tolower(a) == tolower(b)) return(TRUE)

  mapping <- get_team_mapping()

  # Try expanding abbreviation a to match b
  if (toupper(a) %in% names(mapping)) {
    city_a <- mapping[toupper(a)]
    if (grepl(city_a, b, ignore.case = TRUE)) return(TRUE)
  }

  # Try expanding abbreviation b to match a
  if (toupper(b) %in% names(mapping)) {
    city_b <- mapping[toupper(b)]
    if (grepl(city_b, a, ignore.case = TRUE)) return(TRUE)
  }

  # Both could be full names — check if they share the same city token
  a_lower <- tolower(a)
  b_lower <- tolower(b)
  for (city in unique(tolower(mapping))) {
    if (grepl(city, a_lower) && grepl(city, b_lower)) return(TRUE)
  }

  FALSE
}


# =============================================================================
# Fuzzy Match Players
# =============================================================================

#' Fuzzy match player names between two sources
#'
#' Uses normalized name comparison with Levenshtein distance (via base R `adist`)
#' and optional team matching to produce confidence scores.
#'
#' Confidence scoring:
#'   - Name similarity (0-1): 1 - (edit_distance / max_name_length)
#'   - Team bonus: +0.1 if teams match
#'   - Team penalty: -0.15 if teams provided but don't match
#'
#' @param name_a Character vector of names from source A
#' @param name_b Character vector of names from source B
#' @param team_a Character vector of MLB team identifiers for source A (optional)
#' @param team_b Character vector of MLB team identifiers for source B (optional)
#' @return Data frame with columns: idx_a, idx_b, name_a, name_b, confidence
fuzzy_match_players <- function(name_a, name_b, team_a = NULL, team_b = NULL) {
  if (length(name_a) == 0 || length(name_b) == 0) {
    return(data.frame(
      idx_a = integer(0),
      idx_b = integer(0),
      name_a = character(0),
      name_b = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Normalize all names
  norm_a <- vapply(name_a, normalize_name, character(1), USE.NAMES = FALSE)
  norm_b <- vapply(name_b, normalize_name, character(1), USE.NAMES = FALSE)

  results <- vector("list", length(name_a))

  for (i in seq_along(norm_a)) {
    if (is.na(norm_a[i])) {
      results[[i]] <- data.frame(
        idx_a = i, idx_b = NA_integer_,
        name_a = name_a[i], name_b = NA_character_,
        confidence = 0,
        stringsAsFactors = FALSE
      )
      next
    }

    # Compute edit distances from name_a[i] to all name_b
    distances <- adist(norm_a[i], norm_b, ignore.case = TRUE)[1, ]

    # Compute similarity scores (0-1)
    max_lens <- pmax(nchar(norm_a[i]), nchar(norm_b), na.rm = TRUE)
    max_lens[max_lens == 0] <- 1  # avoid division by zero
    similarities <- 1 - (distances / max_lens)

    # Apply team matching bonus/penalty
    if (!is.null(team_a) && !is.null(team_b)) {
      for (j in seq_along(team_b)) {
        tm <- teams_match(team_a[i], team_b[j])
        if (isTRUE(tm)) {
          similarities[j] <- similarities[j] + 0.1
        } else if (isFALSE(tm)) {
          similarities[j] <- similarities[j] - 0.15
        }
        # NA (unknown team) — no adjustment
      }
    }

    # Cap confidence at 1.0
    similarities <- pmin(similarities, 1.0)

    # Find best match
    best_j <- which.max(similarities)
    best_conf <- similarities[best_j]

    results[[i]] <- data.frame(
      idx_a = i,
      idx_b = best_j,
      name_a = name_a[i],
      name_b = name_b[best_j],
      confidence = round(best_conf, 4),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}


# =============================================================================
# Link Roster to Stats
# =============================================================================

#' Link CBS roster players to Baseball Reference stats
#'
#' Matches players from the CBS roster data frame to the Baseball Reference
#' stats data frame using fuzzy name matching with team-based disambiguation.
#'
#' Roster df expected columns: player_name, mlb_team, player_type
#' Stats df expected columns: Name, Team (full city name from BRef)
#'
#' @param roster_df Data frame from rosters.rds
#' @param stats_df Data frame from stats_batting_{year}.rds or stats_pitching_{year}.rds
#' @return roster_df augmented with matched stat columns from stats_df.
#'   Adds columns: matched_name (the BRef name matched), match_confidence (0-1).
#'   All stats columns from stats_df are joined for matched players.
#'   Unmatched players get NA for all stats columns.
link_roster_to_stats <- function(roster_df, stats_df) {
  if (is.null(roster_df) || nrow(roster_df) == 0) {
    warning("roster_df is NULL or empty")
    return(roster_df)
  }
  if (is.null(stats_df) || nrow(stats_df) == 0) {
    warning("stats_df is NULL or empty")
    # Return roster with NA stats columns
    roster_df$matched_name <- NA_character_
    roster_df$match_confidence <- NA_real_
    return(roster_df)
  }

  # Extract name/team vectors

  roster_names <- roster_df$player_name
  stats_names <- if ("Name" %in% names(stats_df)) stats_df$Name else stats_df$name
  roster_teams <- roster_df$mlb_team
  stats_teams <- if ("Team" %in% names(stats_df)) stats_df$Team else stats_df$team

  # Perform fuzzy matching

  matches <- fuzzy_match_players(
    name_a = roster_names,
    name_b = stats_names,
    team_a = roster_teams,
    team_b = stats_teams
  )

  # Apply confidence threshold (only link if confidence >= 0.7)
  confidence_threshold <- 0.7
  matches$idx_b[matches$confidence < confidence_threshold] <- NA_integer_

  # Add match metadata to roster
  roster_df$matched_name <- NA_character_
  roster_df$match_confidence <- NA_real_

  # Identify stats columns to join (exclude the name/team columns already in roster)
  stats_join_cols <- setdiff(names(stats_df), c("Name", "name", "Team", "team"))

  # Initialize all stats columns as NA

  for (col in stats_join_cols) {
    roster_df[[col]] <- NA
  }

  # Fill in matched data
  for (i in seq_len(nrow(matches))) {
    j <- matches$idx_b[i]
    roster_df$match_confidence[i] <- matches$confidence[i]

    if (!is.na(j)) {
      roster_df$matched_name[i] <- stats_names[j]
      for (col in stats_join_cols) {
        roster_df[[col]][i] <- stats_df[[col]][j]
      }
    }
  }

  roster_df
}


# =============================================================================
# MLBAM ID Lookup
# =============================================================================

#' Look up MLBAM ID for a player by name
#'
#' Uses baseballr::playerid_lookup() to find the MLBAM (MLB Advanced Media) ID
#' for a given player name. This enables linkage between CBS data and
#' Statcast/Baseball Reference data.
#'
#' Handles name parsing: splits "First Last" into first/last for the lookup.
#' Returns the best match by most recent season played.
#'
#' @param player_name Character: full player name (e.g., "Jose Ramirez")
#' @return Integer MLBAM ID, or NA_integer_ if not found or lookup fails
get_mlbam_id <- function(player_name) {
  if (is.na(player_name) || nchar(trimws(player_name)) == 0) {
    return(NA_integer_)
  }

  # Split name into parts
  parts <- strsplit(trimws(player_name), "\\s+")[[1]]

  if (length(parts) < 2) {
    warning(sprintf("Cannot parse player name into first/last: '%s'", player_name))
    return(NA_integer_)
  }

  # Handle suffixes — remove Jr., Sr., II, III, IV from last name
  suffix_pattern <- "^(Jr\\.?|Sr\\.?|II|III|IV|V)$"
  non_suffix <- parts[!grepl(suffix_pattern, parts, ignore.case = TRUE)]

  if (length(non_suffix) < 2) {
    # All parts were suffixes except first name — unlikely but handle gracefully
    non_suffix <- parts
  }

  first_name <- non_suffix[1]
  last_name <- paste(non_suffix[-1], collapse = " ")

  # Remove accents for lookup
  first_name <- iconv(first_name, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  last_name <- iconv(last_name, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

  # Attempt lookup via baseballr
  result <- tryCatch({
    # playerid_lookup searches the Chadwick Bureau register
    lookup <- baseballr::playerid_lookup(last_name = last_name, first_name = first_name)

    if (is.null(lookup) || nrow(lookup) == 0) {
      # Try with just last name (handles abbreviated first names)
      lookup <- baseballr::playerid_lookup(last_name = last_name)
    }

    if (is.null(lookup) || nrow(lookup) == 0) {
      return(NA_integer_)
    }

    # Find the MLBAM ID column (may be named differently across versions)
    mlbam_col <- grep("mlbam|mlb_id|key_mlbam", names(lookup),
                      value = TRUE, ignore.case = TRUE)

    if (length(mlbam_col) == 0) {
      warning("Could not find MLBAM ID column in playerid_lookup result")
      return(NA_integer_)
    }

    # If multiple matches, prefer the most recent player
    if (nrow(lookup) > 1) {
      # Check for first name match to disambiguate
      first_match <- grep(first_name, lookup$name_first,
                          ignore.case = TRUE, value = FALSE)
      if (length(first_match) > 0) {
        lookup <- lookup[first_match, , drop = FALSE]
      }

      # If still multiple, take the one with the most recent mlb_played_last
      if (nrow(lookup) > 1 && "mlb_played_last" %in% names(lookup)) {
        lookup <- lookup[order(lookup$mlb_played_last, decreasing = TRUE), ]
      }
    }

    id <- lookup[[mlbam_col[1]]][1]
    if (is.na(id) || id == 0) return(NA_integer_)
    as.integer(id)

  }, error = function(e) {
    warning(sprintf("MLBAM lookup failed for '%s': %s", player_name, e$message))
    NA_integer_
  })

  result
}

#' Roster Optimizer — Weekly lineup optimization
#'
#' Recommends optimal starting lineups for H2H matchups by assigning
#' eligible players to the 16 active roster slots to maximize projected
#' fantasy points. Handles injury exclusions, mid-week replacements,
#' 2-start pitcher preference, and matchup context.
#'
#' Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6

# Source dependencies (when run standalone)
# source("R/utils/scoring.R")
# source("R/analysis/player_valuation.R")

# --- Constants ---

#' Active roster slot configuration
#' C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2
ACTIVE_SLOTS <- list(

  C  = 1,
  `1B` = 1,

  `2B` = 1,
  `3B` = 1,
  SS = 1,
  OF = 3,
  U  = 1,
  SP = 5,
  RP = 2
)

#' Total active slots to fill
TOTAL_ACTIVE_SLOTS <- 16

#' Two-start quality threshold — prefer 2-start SP if quality within 80%
TWO_START_QUALITY_THRESHOLD <- 0.80

# --- Logging helpers ---

log_info_roster <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_warn_roster <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# --- Position Eligibility Helpers ---

#' Parse eligible positions from a comma-separated string
#'
#' @param eligible_str Character: comma-separated position string (e.g., "1B, OF, DH")
#' @return Character vector of eligible positions (uppercased, trimmed)
#' @keywords internal
parse_eligible_positions <- function(eligible_str) {
  if (is.null(eligible_str) || is.na(eligible_str) || nchar(eligible_str) == 0) {
    return(character(0))
  }
  positions <- strsplit(eligible_str, ",")[[1]]
  positions <- trimws(toupper(positions))
  positions[nchar(positions) > 0]
}

#' Check if a player is eligible for a specific roster slot
#'
#' @param player_positions Character vector of eligible positions for the player
#' @param slot Character: the roster slot to check (e.g., "C", "OF", "U", "SP")
#' @return Logical: TRUE if player can fill the slot
#' @keywords internal
is_eligible_for_slot <- function(player_positions, slot) {
  if (length(player_positions) == 0) return(FALSE)

  slot <- toupper(slot)

  # U (utility) slot can be filled by any batter
  if (slot == "U") {
    batter_positions <- c("C", "1B", "2B", "3B", "SS", "OF", "LF", "CF", "RF", "DH", "U")
    return(any(player_positions %in% batter_positions))
  }

  # OF slot can be filled by OF, LF, CF, RF

  if (slot == "OF") {
    of_positions <- c("OF", "LF", "CF", "RF")
    return(any(player_positions %in% of_positions))
  }

  # Direct position match
  return(slot %in% player_positions)
}

#' Determine if a player is a batter or pitcher based on positions
#'
#' @param positions Character vector of eligible positions
#' @param player_type Character: explicit player_type if available
#' @return Character: "Batter" or "Pitcher"
#' @keywords internal
infer_player_type <- function(positions, player_type = NULL) {
  if (!is.null(player_type) && !is.na(player_type)) {
    return(player_type)
  }
  pitcher_pos <- c("SP", "RP", "P", "CL")
  if (any(positions %in% pitcher_pos)) {
    return("Pitcher")
  }
  return("Batter")
}

# --- Injury Filtering ---

#' Check if a player is injured (IL or DTD status)
#'
#' @param injury_status Character: injury status string from CBS data
#' @param pro_status Character: pro_status field (e.g., "IL")
#' @return Logical: TRUE if player should be excluded from active lineup
#' @keywords internal
is_injured <- function(injury_status = NULL, pro_status = NULL) {
  # Check pro_status for IL designation
  if (!is.null(pro_status) && !is.na(pro_status)) {
    if (toupper(pro_status) == "IL") return(TRUE)
  }

  # Check injury_status text for IL or DTD

  if (!is.null(injury_status) && !is.na(injury_status)) {
    status_upper <- toupper(injury_status)
    if (grepl("\\bIL\\b|\\bDL\\b|\\bDTD\\b|\\bDAY-TO-DAY\\b|\\bINJURED\\b", status_upper)) {
      return(TRUE)
    }
  }

  return(FALSE)
}

# --- Core Optimization ---

#' Recommend optimal starting lineup for a scoring period
#'
#' Fills all 16 active slots (C, 1B, 2B, 3B, SS, 3×OF, U, 5×SP, 2×RP)
#' with eligible players projected to maximize total H2H Points for the week.
#' Excludes injured (IL/DTD) players, assigns each player to at most one slot,
#' and considers matchup context for risk preferences.
#'
#' @param my_team Character: owner's team name
#' @param rosters Data frame from rosters.rds with columns: team_name, player_name,
#'   eligible_positions, player_type, roster_position. Optionally: salary, mlb_team.
#' @param projections Data frame with columns: player_name (or cbs_id), proj_pts_per_week.
#'   Optionally: position, player_type, confidence_lo, confidence_hi.
#' @param injuries Data frame or named list with player injury info. If data frame,
#'   expects columns: player_name (or cbs_id), injury (status text), pro_status.
#'   If NULL, no injury filtering is applied.
#' @param matchup_context List with opponent strength info. Optional fields:
#'   - opponent_strength: "strong", "weak", or "average"
#'   - favor_upside: Logical, if TRUE prefer high-ceiling players (strong opponent)
#'   - favor_floor: Logical, if TRUE prefer high-floor players (weak opponent)
#' @return Data frame with columns:
#'   - slot: roster slot assigned (C, 1B, 2B, 3B, SS, OF, U, SP, RP)
#'   - player_name: assigned player name
#'   - eligible_positions: player's full eligibility
#'   - proj_pts_per_week: projected points
#'   - status: "active" for starters, "reserve" for bench
#'   - notes: any relevant info (e.g., "2-start SP")
#' @export
optimize_lineup <- function(my_team, rosters, projections, injuries = NULL,
                            matchup_context = NULL) {
  # --- Input validation ---
  if (is.null(my_team) || nchar(my_team) == 0) {
    stop("'my_team' must be a non-empty team name", call. = FALSE)
  }
  if (is.null(rosters) || nrow(rosters) == 0) {
    stop("'rosters' must be a non-empty data frame", call. = FALSE)
  }
  if (is.null(projections) || nrow(projections) == 0) {
    stop("'projections' must be a non-empty data frame", call. = FALSE)
  }

  # --- Filter to my team's roster ---
  my_roster <- rosters[tolower(rosters$team_name) == tolower(my_team), , drop = FALSE]
  if (nrow(my_roster) == 0) {
    stop(sprintf("Team '%s' not found in roster data", my_team), call. = FALSE)
  }

  # --- Merge projections onto roster ---
  # Match by player_name (case-insensitive)
  my_roster$player_name_lower <- tolower(my_roster$player_name)
  projections$player_name_lower <- tolower(projections$player_name)

  my_players <- merge(
    my_roster,
    projections[, c("player_name_lower", "proj_pts_per_week",
                    if ("confidence_lo" %in% names(projections)) "confidence_lo",
                    if ("confidence_hi" %in% names(projections)) "confidence_hi"),
                drop = FALSE],
    by = "player_name_lower",
    all.x = TRUE
  )

  # Players without projections get 0 projected points
  my_players$proj_pts_per_week[is.na(my_players$proj_pts_per_week)] <- 0

  # --- Apply injury exclusions (Requirement 8.2) ---
  my_players$is_injured <- FALSE

  if (!is.null(injuries)) {
    if (is.data.frame(injuries)) {
      injuries$player_name_lower <- tolower(injuries$player_name)
      for (i in seq_len(nrow(my_players))) {
        p_lower <- my_players$player_name_lower[i]
        inj_row <- injuries[injuries$player_name_lower == p_lower, , drop = FALSE]
        if (nrow(inj_row) > 0) {
          inj_status <- if ("injury" %in% names(inj_row)) inj_row$injury[1] else NULL
          pro_stat <- if ("pro_status" %in% names(inj_row)) inj_row$pro_status[1] else NULL
          my_players$is_injured[i] <- is_injured(inj_status, pro_stat)
        }
      }
    }
  }

  # Also check roster data itself for injury indicators
  if ("pro_status" %in% names(my_players)) {
    for (i in seq_len(nrow(my_players))) {
      if (!my_players$is_injured[i]) {
        my_players$is_injured[i] <- is_injured(
          injury_status = if ("injury" %in% names(my_players)) my_players$injury[i] else NULL,
          pro_status = my_players$pro_status[i]
        )
      }
    }
  }

  # --- Matchup context adjustment (Requirement 8.3) ---
  # If facing strong opponent, prefer upside (use confidence_hi as tiebreaker)
  # If heavily favored, prefer floor (use confidence_lo as tiebreaker)
  scoring_col <- "proj_pts_per_week"
  if (!is.null(matchup_context)) {
    if (isTRUE(matchup_context$favor_upside) && "confidence_hi" %in% names(my_players)) {
      # Blend projected with upside: 70% projection + 30% high end
      my_players$optimization_score <- 0.7 * my_players$proj_pts_per_week +
                                        0.3 * my_players$confidence_hi
    } else if (isTRUE(matchup_context$favor_floor) && "confidence_lo" %in% names(my_players)) {
      # Blend projected with floor: 70% projection + 30% low end
      my_players$optimization_score <- 0.7 * my_players$proj_pts_per_week +
                                        0.3 * my_players$confidence_lo
    } else {
      my_players$optimization_score <- my_players$proj_pts_per_week
    }
  } else {
    my_players$optimization_score <- my_players$proj_pts_per_week
  }

  # --- Parse eligible positions ---
  my_players$parsed_positions <- lapply(
    my_players$eligible_positions,
    parse_eligible_positions
  )

  # Infer player type
  my_players$inferred_type <- mapply(
    function(pos, ptype) infer_player_type(pos, ptype),
    my_players$parsed_positions,
    if ("player_type" %in% names(my_players)) my_players$player_type else rep(NA, nrow(my_players)),
    SIMPLIFY = TRUE
  )

  # --- Separate available (not injured) players ---
  available <- my_players[!my_players$is_injured, , drop = FALSE]
  injured_players <- my_players[my_players$is_injured, , drop = FALSE]

  if (nrow(injured_players) > 0) {
    log_info_roster(sprintf(
      "Excluding %d injured player(s): %s",
      nrow(injured_players),
      paste(injured_players$player_name, collapse = ", ")
    ))
  }

  # --- Greedy slot assignment ---
  # Strategy: assign scarce positions first, then fill remaining slots.
  # Order positions by scarcity (fewer slots = more constrained = assign first)
  # U is last since it accepts any batter.

  slot_order <- c("C", "SS", "2B", "3B", "1B", "OF", "SP", "RP", "U")

  assigned_players <- character(0)  # track player_name already assigned
  lineup <- data.frame(
    slot = character(0),
    player_name = character(0),
    eligible_positions = character(0),
    proj_pts_per_week = numeric(0),
    status = character(0),
    notes = character(0),
    stringsAsFactors = FALSE
  )

  for (slot in slot_order) {
    n_needed <- ACTIVE_SLOTS[[slot]]
    if (is.null(n_needed)) next

    # Find eligible, unassigned, available players for this slot
    candidates <- available[
      !available$player_name %in% assigned_players, , drop = FALSE
    ]

    eligible_mask <- vapply(candidates$parsed_positions, function(pos) {
      is_eligible_for_slot(pos, slot)
    }, logical(1))

    eligible_candidates <- candidates[eligible_mask, , drop = FALSE]

    # Sort by optimization score descending
    eligible_candidates <- eligible_candidates[
      order(-eligible_candidates$optimization_score), , drop = FALSE
    ]

    # Take top n_needed
    n_assign <- min(n_needed, nrow(eligible_candidates))

    if (n_assign > 0) {
      for (j in seq_len(n_assign)) {
        player <- eligible_candidates[j, , drop = FALSE]
        assigned_players <- c(assigned_players, player$player_name)

        lineup <- rbind(lineup, data.frame(
          slot = slot,
          player_name = player$player_name,
          eligible_positions = if (!is.null(player$eligible_positions)) player$eligible_positions else "",
          proj_pts_per_week = player$proj_pts_per_week,
          status = "active",
          notes = "",
          stringsAsFactors = FALSE
        ))
      }
    }

    # Warn if unable to fill a slot (Requirement 8.6)
    if (n_assign < n_needed) {
      unfilled <- n_needed - n_assign
      log_warn_roster(sprintf(
        "Unable to fill %d of %d %s slot(s) — no eligible players available",
        unfilled, n_needed, slot
      ))
      # Add placeholder row for unfilled slot
      for (k in seq_len(unfilled)) {
        lineup <- rbind(lineup, data.frame(
          slot = slot,
          player_name = NA_character_,
          eligible_positions = NA_character_,
          proj_pts_per_week = 0,
          status = "unfilled",
          notes = "No eligible player available",
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # --- Mark remaining players as reserve ---
  reserve_players <- available[
    !available$player_name %in% assigned_players, , drop = FALSE
  ]

  if (nrow(reserve_players) > 0) {
    reserve_df <- data.frame(
      slot = "Reserve",
      player_name = reserve_players$player_name,
      eligible_positions = reserve_players$eligible_positions,
      proj_pts_per_week = reserve_players$proj_pts_per_week,
      status = "reserve",
      notes = "",
      stringsAsFactors = FALSE
    )
    lineup <- rbind(lineup, reserve_df)
  }

  # --- Mark injured players ---
  if (nrow(injured_players) > 0) {
    injured_df <- data.frame(
      slot = "IL",
      player_name = injured_players$player_name,
      eligible_positions = injured_players$eligible_positions,
      proj_pts_per_week = injured_players$proj_pts_per_week,
      status = "injured",
      notes = if ("injury" %in% names(injured_players)) injured_players$injury else "",
      stringsAsFactors = FALSE
    )
    lineup <- rbind(lineup, injured_df)
  }

  # Clean up row names
  rownames(lineup) <- NULL

  # Summary logging
  active_count <- sum(lineup$status == "active")
  log_info_roster(sprintf(
    "Lineup optimized for %s: %d/%d active slots filled, %d reserve, %d injured",
    my_team, active_count, TOTAL_ACTIVE_SLOTS,
    sum(lineup$status == "reserve"),
    sum(lineup$status == "injured")
  ))

  lineup
}


#' Select optimal pitching staff for the week
#'
#' Recommends 5 SP + 2 RP from the available pitchers, preferring 2-start
#' pitchers over 1-start pitchers when quality is within 80% (Requirement 8.5).
#'
#' @param available_pitchers Data frame of team's pitcher roster with columns:
#'   player_name, eligible_positions (or position), player_type.
#'   Optionally: starts_this_week (integer), proj_pts_per_week.
#' @param projections Data frame with columns: player_name, proj_pts_per_week.
#'   Optionally: proj_pts_per_start (for per-start quality comparison).
#' @param schedule Data frame or list with weekly start info. If data frame,
#'   expects columns: player_name, starts_this_week (integer: 0, 1, or 2).
#'   If NULL, all SPs treated as 1-start.
#' @return Data frame with columns:
#'   - slot: "SP" or "RP"
#'   - player_name: assigned pitcher
#'   - starts_this_week: number of starts (SP only)
#'   - proj_pts_per_week: projected points for the week
#'   - rationale: reason for selection
#' @export
optimize_pitchers <- function(available_pitchers, projections, schedule = NULL) {
  # --- Input validation ---
  if (is.null(available_pitchers) || nrow(available_pitchers) == 0) {
    stop("'available_pitchers' must be a non-empty data frame", call. = FALSE)
  }
  if (is.null(projections) || nrow(projections) == 0) {
    stop("'projections' must be a non-empty data frame", call. = FALSE)
  }

  # --- Merge projections ---
  pitchers <- available_pitchers
  pitchers$player_name_lower <- tolower(pitchers$player_name)
  projections$player_name_lower <- tolower(projections$player_name)

  pitchers <- merge(
    pitchers,
    projections[, c("player_name_lower", "proj_pts_per_week",
                    if ("proj_pts_per_start" %in% names(projections)) "proj_pts_per_start"),
                drop = FALSE],
    by = "player_name_lower",
    all.x = TRUE,
    suffixes = c("", ".proj")
  )

  # Handle column name conflicts from merge
  if ("proj_pts_per_week.proj" %in% names(pitchers)) {
    pitchers$proj_pts_per_week <- ifelse(
      is.na(pitchers$proj_pts_per_week),
      pitchers$proj_pts_per_week.proj,
      pitchers$proj_pts_per_week
    )
    pitchers$proj_pts_per_week.proj <- NULL
  }

  pitchers$proj_pts_per_week[is.na(pitchers$proj_pts_per_week)] <- 0

  # --- Merge schedule info ---
  if (!is.null(schedule)) {
    if (is.data.frame(schedule)) {
      schedule$player_name_lower <- tolower(schedule$player_name)
      pitchers <- merge(
        pitchers,
        schedule[, c("player_name_lower", "starts_this_week"), drop = FALSE],
        by = "player_name_lower",
        all.x = TRUE
      )
    }
  }

  # Default starts_this_week to 1 for SPs if not provided
  if (!"starts_this_week" %in% names(pitchers)) {
    pitchers$starts_this_week <- NA_integer_
  }

  # --- Classify as SP or RP ---
  pitchers$parsed_positions <- lapply(
    if ("eligible_positions" %in% names(pitchers)) pitchers$eligible_positions
    else if ("position" %in% names(pitchers)) pitchers$position
    else rep(NA, nrow(pitchers)),
    parse_eligible_positions
  )

  pitchers$is_sp <- vapply(pitchers$parsed_positions, function(pos) {
    any(pos %in% c("SP", "P"))
  }, logical(1))

  pitchers$is_rp <- vapply(pitchers$parsed_positions, function(pos) {
    any(pos %in% c("RP", "CL", "P"))
  }, logical(1))

  # Default starts for SPs without schedule info
  pitchers$starts_this_week[pitchers$is_sp & is.na(pitchers$starts_this_week)] <- 1

  # RPs get 0 starts
  pitchers$starts_this_week[pitchers$is_rp & !pitchers$is_sp] <- 0

  # --- Compute per-start quality ---
  # If not provided, estimate from per-week projection and starts
  if (!"proj_pts_per_start" %in% names(pitchers)) {
    pitchers$proj_pts_per_start <- ifelse(
      pitchers$starts_this_week > 0,
      pitchers$proj_pts_per_week / pitchers$starts_this_week,
      pitchers$proj_pts_per_week
    )
  }

  # --- Select 5 SP (Requirement 8.5: prefer 2-start pitchers) ---
  sp_candidates <- pitchers[pitchers$is_sp, , drop = FALSE]

  # Compute effective weekly value considering starts
  # 2-start SPs get their full per-week projection (or 2 × per-start)
  # 1-start SPs get half their per-week projection (or 1 × per-start)
  sp_candidates$effective_weekly_pts <- ifelse(
    sp_candidates$starts_this_week >= 2,
    sp_candidates$proj_pts_per_start * 2,
    sp_candidates$proj_pts_per_start * sp_candidates$starts_this_week
  )

  # Apply 2-start preference: boost 2-start pitchers
  # A 2-start SP is preferred over a 1-start SP if the 2-start SP's
  # per-start quality is within 80% of the 1-start SP's per-start quality
  sp_candidates$sort_score <- sp_candidates$effective_weekly_pts

  # For 2-start SPs, add a bonus: if their per-start is within 80% of the best

  # 1-start SP, they should rank higher based on total weekly value
  # This is naturally handled by effective_weekly_pts since 2×(0.8×Q) > 1×Q
  # when per-start quality is at or above 80% of the best

  sp_candidates <- sp_candidates[order(-sp_candidates$sort_score), , drop = FALSE]

  # Select top 5 SPs
  n_sp <- min(5, nrow(sp_candidates))
  selected_sp <- sp_candidates[seq_len(n_sp), , drop = FALSE]

  # --- Select 2 RP ---
  # RPs are those eligible for RP/CL who are NOT already selected as SP
  rp_candidates <- pitchers[
    pitchers$is_rp & !(pitchers$player_name %in% selected_sp$player_name),
    , drop = FALSE
  ]
  rp_candidates <- rp_candidates[order(-rp_candidates$proj_pts_per_week), , drop = FALSE]

  n_rp <- min(2, nrow(rp_candidates))
  selected_rp <- if (n_rp > 0) rp_candidates[seq_len(n_rp), , drop = FALSE] else rp_candidates[0, , drop = FALSE]

  # --- Build result ---
  result <- data.frame(
    slot = character(0),
    player_name = character(0),
    starts_this_week = integer(0),
    proj_pts_per_week = numeric(0),
    rationale = character(0),
    stringsAsFactors = FALSE
  )

  if (n_sp > 0) {
    sp_rationale <- ifelse(
      selected_sp$starts_this_week >= 2,
      "2-start pitcher — extra IP and counting stats",
      "1-start pitcher — selected on quality"
    )
    sp_df <- data.frame(
      slot = rep("SP", n_sp),
      player_name = selected_sp$player_name,
      starts_this_week = selected_sp$starts_this_week,
      proj_pts_per_week = selected_sp$effective_weekly_pts,
      rationale = sp_rationale,
      stringsAsFactors = FALSE
    )
    result <- rbind(result, sp_df)
  }

  if (n_rp > 0) {
    rp_df <- data.frame(
      slot = rep("RP", n_rp),
      player_name = selected_rp$player_name,
      starts_this_week = rep(0L, n_rp),
      proj_pts_per_week = selected_rp$proj_pts_per_week,
      rationale = rep("Relief pitcher — selected on projected points", n_rp),
      stringsAsFactors = FALSE
    )
    result <- rbind(result, rp_df)
  }

  # --- Warn about unfilled slots (Requirement 8.6) ---
  if (n_sp < 5) {
    log_warn_roster(sprintf(
      "Only %d SP available (need 5) — %d SP slot(s) unfilled",
      n_sp, 5 - n_sp
    ))
  }
  if (n_rp < 2) {
    log_warn_roster(sprintf(
      "Only %d RP available (need 2) — %d RP slot(s) unfilled",
      n_rp, 2 - n_rp
    ))
  }

  rownames(result) <- NULL

  log_info_roster(sprintf(
    "Pitching staff optimized: %d SP + %d RP selected, %d 2-start SPs",
    n_sp, n_rp, sum(selected_sp$starts_this_week >= 2)
  ))

  result
}


#' Suggest replacement from reserve for a mid-week injury
#'
#' When a player's status changes mid-week to injured/DTD, this function
#' identifies the best eligible reserve player for the affected slot.
#'
#' @param injured_player Character: name of the injured player
#' @param injured_slot Character: the roster slot that needs filling (e.g., "SS", "SP")
#' @param lineup Data frame: current lineup from optimize_lineup()
#' @param projections Data frame with player projections
#' @return List with:
#'   - replacement_found: Logical
#'   - replacement_player: Character name of suggested replacement (or NA)
#'   - slot: The affected slot
#'   - proj_pts_per_week: Projected points of replacement
#'   - message: Human-readable message
#' @export
suggest_injury_replacement <- function(injured_player, injured_slot, lineup, projections) {
  if (is.null(injured_player) || is.na(injured_player)) {
    stop("'injured_player' must be provided", call. = FALSE)
  }
  if (is.null(injured_slot) || is.na(injured_slot)) {
    stop("'injured_slot' must be provided", call. = FALSE)
  }

  # Find reserve players from the lineup
  reserves <- lineup[lineup$status == "reserve", , drop = FALSE]

  if (nrow(reserves) == 0) {
    # Requirement 8.6: notify if no eligible replacement
    msg <- sprintf(
      "No replacement available for %s (%s slot) — no players on reserve",
      injured_player, injured_slot
    )
    log_warn_roster(msg)
    return(list(
      replacement_found = FALSE,
      replacement_player = NA_character_,
      slot = injured_slot,
      proj_pts_per_week = 0,
      message = msg
    ))
  }

  # Parse reserve players' positions and find eligible ones
  reserves$parsed_positions <- lapply(
    reserves$eligible_positions,
    parse_eligible_positions
  )

  eligible_mask <- vapply(reserves$parsed_positions, function(pos) {
    is_eligible_for_slot(pos, injured_slot)
  }, logical(1))

  eligible_reserves <- reserves[eligible_mask, , drop = FALSE]

  if (nrow(eligible_reserves) == 0) {
    # Requirement 8.6: notify if no eligible replacement
    msg <- sprintf(
      "No eligible replacement available for %s (%s slot) — no reserve player qualifies at %s",
      injured_player, injured_slot, injured_slot
    )
    log_warn_roster(msg)
    return(list(
      replacement_found = FALSE,
      replacement_player = NA_character_,
      slot = injured_slot,
      proj_pts_per_week = 0,
      message = msg
    ))
  }

  # Select the highest projected reserve player for remaining days
  # Merge projections if not already present
  if (!"proj_pts_per_week" %in% names(eligible_reserves) ||
      all(is.na(eligible_reserves$proj_pts_per_week))) {
    eligible_reserves$player_name_lower <- tolower(eligible_reserves$player_name)
    projections$player_name_lower <- tolower(projections$player_name)
    eligible_reserves <- merge(
      eligible_reserves,
      projections[, c("player_name_lower", "proj_pts_per_week"), drop = FALSE],
      by = "player_name_lower",
      all.x = TRUE,
      suffixes = c(".old", "")
    )
    if ("proj_pts_per_week.old" %in% names(eligible_reserves)) {
      eligible_reserves$proj_pts_per_week <- ifelse(
        is.na(eligible_reserves$proj_pts_per_week),
        eligible_reserves$proj_pts_per_week.old,
        eligible_reserves$proj_pts_per_week
      )
      eligible_reserves$proj_pts_per_week.old <- NULL
    }
  }

  eligible_reserves$proj_pts_per_week[is.na(eligible_reserves$proj_pts_per_week)] <- 0

  # Pick the best
  best_idx <- which.max(eligible_reserves$proj_pts_per_week)
  best_player <- eligible_reserves[best_idx, , drop = FALSE]

  msg <- sprintf(
    "Suggested replacement for %s (%s): %s (projected %.1f pts/wk)",
    injured_player, injured_slot,
    best_player$player_name, best_player$proj_pts_per_week
  )
  log_info_roster(msg)

  list(
    replacement_found = TRUE,
    replacement_player = best_player$player_name,
    slot = injured_slot,
    proj_pts_per_week = best_player$proj_pts_per_week,
    message = msg
  )
}

#' Player Valuation Engine
#'
#' Dollar value assignment based on league economics. Computes replacement
#' level by position, assigns dollar values proportional to Points Above
#' Replacement (PAR), and applies positional scarcity premiums.
#'
#' Requirements: 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8

# Source dependencies (when run standalone)
# source("R/utils/scoring.R")
# source("R/utils/keeper_value.R")

# --- Configuration ---

#' Default roster construction: slots per position × 16 teams
#' C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2
DEFAULT_ROSTER_SLOTS <- c(

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

#' Number of teams in the league
N_TEAMS <- 16

#' Default total salary pool: $260 × 16 teams = $4,160
DEFAULT_SALARY_POOL <- 4160

#' Default discount rate for keeper value NPV calculations
DEFAULT_DISCOUNT_RATE <- 0.9

# --- Logging helpers ---

log_info_val <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_warn_val <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# --- Position Mapping ---

#' Map a player's position to the valuation position group
#'
#' Handles multi-eligibility by returning the primary valuation position.
#' The U (utility) slot is filled by the best remaining batter regardless
#' of position, so we don't map anyone to "U" for replacement-level purposes.
#'
#' @param position Character: player's primary position
#' @param eligible_positions Character: comma-separated eligible positions (optional)
#' @param player_type Character: "Batter" or "Pitcher"
#' @return Character: valuation position group
#' @keywords internal
map_to_valuation_position <- function(position, eligible_positions = NULL,
                                       player_type = "Batter") {
  if (is.na(position) || is.null(position)) {
    if (!is.null(player_type) && player_type == "Pitcher") return("SP")
    return("U")
  }

  pos <- toupper(trimws(position))

  # Pitcher positions
  if (pos %in% c("SP", "P")) return("SP")
  if (pos %in% c("RP", "CL")) return("RP")

  # Batter positions
  if (pos == "C") return("C")
  if (pos == "1B") return("1B")
  if (pos == "2B") return("2B")
  if (pos == "3B") return("3B")
  if (pos == "SS") return("SS")
  if (pos %in% c("OF", "LF", "CF", "RF")) return("OF")
  if (pos %in% c("DH", "U")) return("U")

  # Fallback: use player_type to decide

  if (!is.null(player_type) && player_type == "Pitcher") return("SP")
  return("U")
}


# --- Core Functions ---

#' Compute replacement level by position
#'
#' Replacement level at each position is defined as the projected pts/week of
#' the (N × 16 + 1)th ranked player at that position, where N is the number
#' of roster slots at that position.
#'
#' The U (utility) slot is treated as the best remaining batter after all
#' positional slots are filled — replacement level is the (total_batter_slots × 16 + 1)th
#' best batter overall.
#'
#' @param projections Data frame with at minimum: position, player_type, proj_pts_per_week.
#'   Players with NA or missing proj_pts_per_week are excluded.
#' @param roster_slots Named vector of slots per position (before multiplying by N_TEAMS).
#'   Default: C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2
#' @return Named numeric vector: replacement-level pts/week per position
#' @export
compute_replacement_level <- function(projections, roster_slots = DEFAULT_ROSTER_SLOTS) {
  if (is.null(projections) || nrow(projections) == 0) {
    stop("projections must be a non-empty data frame", call. = FALSE)
  }

  # Ensure required columns exist
  required_cols <- c("position", "proj_pts_per_week")
  missing_cols <- setdiff(required_cols, names(projections))
  if (length(missing_cols) > 0) {
    stop(sprintf("projections is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Exclude players with no projections (Requirement 5.8)
  proj <- projections[!is.na(projections$proj_pts_per_week), ]

  if (nrow(proj) == 0) {
    stop("No players with valid proj_pts_per_week", call. = FALSE)
  }

  # Infer player_type if not present
  if (!"player_type" %in% names(proj)) {
    proj$player_type <- ifelse(
      proj$position %in% c("SP", "RP", "P", "CL"),
      "Pitcher", "Batter"
    )
  }

  # Map each player to their valuation position
  proj$val_position <- mapply(
    map_to_valuation_position,
    proj$position,
    MoreArgs = list(eligible_positions = NULL, player_type = NULL),
    USE.NAMES = FALSE
  )
  # Override with player_type for those mapped via fallback
  pitcher_mask <- proj$player_type == "Pitcher"
  proj$val_position[pitcher_mask & proj$val_position == "U"] <- "SP"

  # Compute replacement level for each position (excluding U)
  positions <- names(roster_slots)
  positions <- positions[positions != "U"]
  replacement_levels <- setNames(numeric(length(positions)), positions)

  for (pos in positions) {
    n_slots <- roster_slots[[pos]] * N_TEAMS
    pos_players <- proj[proj$val_position == pos, ]
    pos_players <- pos_players[order(-pos_players$proj_pts_per_week), ]

    # Replacement level = (N×16 + 1)th player's pts/week
    repl_rank <- n_slots + 1
    if (nrow(pos_players) >= repl_rank) {
      replacement_levels[pos] <- pos_players$proj_pts_per_week[repl_rank]
    } else if (nrow(pos_players) > 0) {
      # Not enough players — use the last player's value
      replacement_levels[pos] <- pos_players$proj_pts_per_week[nrow(pos_players)]
      log_warn_val(sprintf(
        "Only %d players at %s (need %d for replacement level)",
        nrow(pos_players), pos, repl_rank
      ))
    } else {
      replacement_levels[pos] <- 0
      log_warn_val(sprintf("No players found at position %s", pos))
    }
  }

  # U (utility) replacement level: based on all batters combined
  # The utility slot is filled by the best remaining batter after positional slots
  # For simplicity, U replacement = lowest replacement among batter positions
  batter_positions <- c("C", "1B", "2B", "3B", "SS", "OF")
  batter_repls <- replacement_levels[batter_positions]
  batter_repls <- batter_repls[batter_repls > 0]
  if (length(batter_repls) > 0) {
    # U replacement is slightly below the least scarce batter position
    replacement_levels["U"] <- min(batter_repls)
  } else {
    replacement_levels["U"] <- 0
  }

  replacement_levels
}


#' Assign dollar values to all projected players
#'
#' Distributes the total salary pool across all above-replacement players
#' proportional to their Points Above Replacement (PAR). Players below
#' replacement level receive $0 (or $1 minimum for rostered players).
#'
#' @param projections Data frame with at minimum: player_name, position,
#'   proj_pts_per_week, player_type. Optionally: salary, is_minor_contract,
#'   minor_track_year, cbs_id.
#' @param replacement_levels Named numeric vector from compute_replacement_level()
#' @param total_salary_pool Total $ to distribute (default: 260 × 16 = 4160)
#' @return Data frame with columns added:
#'   - dollar_value: projected $ value
#'   - pts_above_replacement: PAR
#'   - replacement_level: the replacement level at the player's position
#'   - surplus_value: dollar_value - salary (NA if no salary)
#'   - keeper_value_3yr: NPV of keeper surplus over 3 years
#'   - keeper_value_5yr: NPV of keeper surplus over 5 years
#' @export
assign_dollar_values <- function(projections, replacement_levels,
                                  total_salary_pool = DEFAULT_SALARY_POOL) {
  if (is.null(projections) || nrow(projections) == 0) {
    stop("projections must be a non-empty data frame", call. = FALSE)
  }
  if (is.null(replacement_levels) || length(replacement_levels) == 0) {
    stop("replacement_levels must be a non-empty named numeric vector", call. = FALSE)
  }

  # Exclude players with no projections (Requirement 5.8)
  has_proj <- !is.na(projections$proj_pts_per_week)
  result <- projections[has_proj, , drop = FALSE]

  if (nrow(result) == 0) {
    stop("No players with valid projections to value", call. = FALSE)
  }

  # Infer player_type if not present
  if (!"player_type" %in% names(result)) {
    result$player_type <- ifelse(
      result$position %in% c("SP", "RP", "P", "CL"),
      "Pitcher", "Batter"
    )
  }

  # Map positions for valuation
  result$val_position <- mapply(
    map_to_valuation_position,
    result$position,
    MoreArgs = list(eligible_positions = NULL, player_type = NULL),
    USE.NAMES = FALSE
  )
  pitcher_mask <- result$player_type == "Pitcher"
  result$val_position[pitcher_mask & result$val_position == "U"] <- "SP"

  # Compute PAR for each player
  result$replacement_level <- vapply(result$val_position, function(pos) {
    if (pos %in% names(replacement_levels)) {
      replacement_levels[[pos]]
    } else {
      # Fallback: use minimum replacement level
      min(replacement_levels, na.rm = TRUE)
    }
  }, numeric(1))

  result$pts_above_replacement <- result$proj_pts_per_week - result$replacement_level

  # Only players with positive PAR get dollar values
  positive_par <- pmax(result$pts_above_replacement, 0)
  total_par <- sum(positive_par)

  if (total_par > 0) {
    # Distribute salary pool proportional to PAR
    result$dollar_value <- (positive_par / total_par) * total_salary_pool
  } else {
    result$dollar_value <- 0
    log_warn_val("Total PAR is zero — all players valued at $0")
  }

  # Floor dollar values at $0 (players below replacement get $0)
  result$dollar_value <- pmax(result$dollar_value, 0)

  # Compute surplus value (Requirement 5.5)
  if ("salary" %in% names(result)) {
    result$surplus_value <- result$dollar_value - result$salary
  } else {
    result$surplus_value <- NA_real_
  }

  # Compute keeper values (Requirements 5.6, 5.7)
  result$keeper_value_3yr <- NA_real_
  result$keeper_value_5yr <- NA_real_

  if ("salary" %in% names(result)) {
    for (i in seq_len(nrow(result))) {
      salary_i <- result$salary[i]
      if (is.na(salary_i)) next

      is_minor <- if ("is_minor_contract" %in% names(result)) {
        isTRUE(result$is_minor_contract[i])
      } else {
        FALSE
      }
      minor_yr <- if ("minor_track_year" %in% names(result)) {
        val <- result$minor_track_year[i]
        if (is.na(val)) 0 else val
      } else {
        0
      }

      dollar_val_i <- result$dollar_value[i]

      # Project keeper salary for 3 and 5 years
      salaries_3yr <- project_keeper_salary(
        salary_i, is_minor_contract = is_minor,
        minor_track_year = minor_yr, years_ahead = 3
      )
      salaries_5yr <- project_keeper_salary(
        salary_i, is_minor_contract = is_minor,
        minor_track_year = minor_yr, years_ahead = 5
      )

      # Assume constant dollar value for future years (simplified)
      # A more sophisticated model would project declining value with age
      values_3yr <- rep(dollar_val_i, 3)
      values_5yr <- rep(dollar_val_i, 5)

      # Compute NPV surplus
      keeper_3 <- compute_keeper_surplus(values_3yr, salaries_3yr,
                                          discount_rate = DEFAULT_DISCOUNT_RATE)
      keeper_5 <- compute_keeper_surplus(values_5yr, salaries_5yr,
                                          discount_rate = DEFAULT_DISCOUNT_RATE)

      result$keeper_value_3yr[i] <- keeper_3$npv_surplus
      result$keeper_value_5yr[i] <- keeper_5$npv_surplus
    }
  }

  # Clean up internal column
  result$val_position <- NULL

  log_info_val(sprintf(
    "Valued %d players: pool=$%s, total PAR=%.1f, mean value=$%.1f",
    nrow(result), format(total_salary_pool, big.mark = ","),
    total_par, mean(result$dollar_value)
  ))

  result
}


#' Positional scarcity adjustment
#'
#' Adjusts dollar values to reflect positional scarcity. Positions with fewer
#' total league slots (C=16, SS=16) are scarcer than positions with more slots
#' (OF=48, SP=80), so players at thin positions receive a premium.
#'
#' The adjustment uses a scarcity multiplier inversely proportional to the
#' number of total league slots at each position, normalized so the total
#' salary pool remains constant after adjustment.
#'
#' @param values Data frame with dollar_value, position, player_type columns
#'   (typically output from assign_dollar_values())
#' @param roster_slots Named vector of slots per position (before × N_TEAMS).
#'   Default: C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2
#' @return Data frame with adjusted dollar_value and added positional_scarcity column
#' @export
adjust_positional_scarcity <- function(values, roster_slots = DEFAULT_ROSTER_SLOTS) {
  if (is.null(values) || nrow(values) == 0) {
    stop("values must be a non-empty data frame", call. = FALSE)
  }

  # Compute total league slots per position
  total_slots <- roster_slots * N_TEAMS

  # Scarcity multiplier: inversely proportional to total slots

  # More slots = less scarce = lower multiplier
  # Formula: scarcity = median(total_slots) / total_slots
  # This gives >1 for thin positions and <1 for deep positions
  median_slots <- median(total_slots)
  scarcity_multipliers <- median_slots / total_slots

  # Map players to their valuation position
  if (!"player_type" %in% names(values)) {
    values$player_type <- ifelse(
      values$position %in% c("SP", "RP", "P", "CL"),
      "Pitcher", "Batter"
    )
  }

  val_positions <- mapply(
    map_to_valuation_position,
    values$position,
    MoreArgs = list(eligible_positions = NULL, player_type = NULL),
    USE.NAMES = FALSE
  )
  pitcher_mask <- values$player_type == "Pitcher"
  val_positions[pitcher_mask & val_positions == "U"] <- "SP"

  # Assign scarcity multiplier to each player
  values$positional_scarcity <- vapply(val_positions, function(pos) {
    if (pos %in% names(scarcity_multipliers)) {
      scarcity_multipliers[[pos]]
    } else {
      1.0  # no adjustment for unmapped positions
    }
  }, numeric(1))

  # Store original total for normalization
  original_total <- sum(values$dollar_value[values$dollar_value > 0])

  # Apply scarcity multiplier to dollar values
  values$dollar_value <- values$dollar_value * values$positional_scarcity

  # Renormalize so total pool is preserved (Requirement: pool conservation)
  new_total <- sum(values$dollar_value[values$dollar_value > 0])
  if (new_total > 0 && original_total > 0) {
    normalization_factor <- original_total / new_total
    values$dollar_value <- values$dollar_value * normalization_factor
  }

  # Recompute surplus value after scarcity adjustment
  if ("salary" %in% names(values)) {
    values$surplus_value <- values$dollar_value - values$salary
  }

  # Recompute keeper values after scarcity adjustment
  if ("salary" %in% names(values)) {
    for (i in seq_len(nrow(values))) {
      salary_i <- values$salary[i]
      if (is.na(salary_i)) next

      is_minor <- if ("is_minor_contract" %in% names(values)) {
        isTRUE(values$is_minor_contract[i])
      } else {
        FALSE
      }
      minor_yr <- if ("minor_track_year" %in% names(values)) {
        val <- values$minor_track_year[i]
        if (is.na(val)) 0 else val
      } else {
        0
      }

      dollar_val_i <- values$dollar_value[i]

      salaries_3yr <- project_keeper_salary(
        salary_i, is_minor_contract = is_minor,
        minor_track_year = minor_yr, years_ahead = 3
      )
      salaries_5yr <- project_keeper_salary(
        salary_i, is_minor_contract = is_minor,
        minor_track_year = minor_yr, years_ahead = 5
      )

      values_3yr <- rep(dollar_val_i, 3)
      values_5yr <- rep(dollar_val_i, 5)

      keeper_3 <- compute_keeper_surplus(values_3yr, salaries_3yr,
                                          discount_rate = DEFAULT_DISCOUNT_RATE)
      keeper_5 <- compute_keeper_surplus(values_5yr, salaries_5yr,
                                          discount_rate = DEFAULT_DISCOUNT_RATE)

      values$keeper_value_3yr[i] <- keeper_3$npv_surplus
      values$keeper_value_5yr[i] <- keeper_5$npv_surplus
    }
  }

  log_info_val(sprintf(
    "Applied scarcity adjustment: C=%.2f, SS=%.2f, OF=%.2f, SP=%.2f",
    scarcity_multipliers[["C"]],
    scarcity_multipliers[["SS"]],
    scarcity_multipliers[["OF"]],
    scarcity_multipliers[["SP"]]
  ))

  values
}


# --- Convenience Pipeline Function ---

#' Run the full player valuation pipeline
#'
#' Convenience function that runs all valuation steps in sequence:
#' 1. Compute replacement levels
#' 2. Assign dollar values
#' 3. Apply positional scarcity
#'
#' Excludes players with no projections (proj_pts_per_week is NA).
#'
#' @param projections Data frame with proj_pts_per_week, position, player_type.
#'   Optionally includes salary, is_minor_contract, minor_track_year.
#' @param roster_slots Named vector of position slots (default: league standard)
#' @param total_salary_pool Total dollars to distribute (default: $4,160)
#' @return Data frame with all valuation columns (dollar_value, surplus_value,
#'   keeper_value_3yr, keeper_value_5yr, positional_scarcity, etc.)
#' @export
run_valuation_pipeline <- function(projections,
                                    roster_slots = DEFAULT_ROSTER_SLOTS,
                                    total_salary_pool = DEFAULT_SALARY_POOL) {
  # Step 1: Compute replacement levels
  replacement_levels <- compute_replacement_level(projections, roster_slots)

  log_info_val("Replacement levels computed:")
  for (pos in names(replacement_levels)) {
    log_info_val(sprintf("  %s: %.2f pts/week", pos, replacement_levels[[pos]]))
  }

  # Step 2: Assign dollar values (also computes surplus and keeper values)
  valued <- assign_dollar_values(projections, replacement_levels, total_salary_pool)

  # Step 3: Apply positional scarcity adjustment
  valued <- adjust_positional_scarcity(valued, roster_slots)

  # Sort by dollar value descending for rankings
  valued <- valued[order(-valued$dollar_value), ]
  rownames(valued) <- NULL

  log_info_val(sprintf("Valuation pipeline complete: %d players valued", nrow(valued)))

  valued
}

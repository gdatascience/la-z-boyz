#' Waiver Recommender — FAAB Bidding Recommendations
#'
#' Identifies high-value free agents available via FAAB bidding, suggests
#' bid amounts, and recommends drop candidates for roster improvement.
#'
#' Requirements: 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8

# Source dependencies (when run standalone)
# source("R/utils/salary_rules.R")
# source("R/utils/keeper_value.R")
# source("R/utils/player_linker.R")
# source("R/analysis/player_valuation.R")
# source("R/analysis/trade_analyzer.R")

# --- Constants ---

#' In-season salary cap
WAIVER_SALARY_CAP <- 300

#' Maximum roster size
MAX_ROSTER_SIZE <- 27

#' FAAB minimum bid
FAAB_MIN_BID <- 1

#' Total FAAB budget at season start
FAAB_TOTAL_BUDGET <- 250

#' Regular season length in weeks
WAIVER_REGULAR_SEASON_WEEKS <- 23

#' Playoff start week
PLAYOFF_START_WEEK <- 24

#' Minor league promotion thresholds
PROMOTION_AB_THRESHOLD <- 130
PROMOTION_IP_THRESHOLD <- 50

#' Approaching promotion thresholds (for flagging)
APPROACHING_AB_THRESHOLD <- 110
APPROACHING_IP_THRESHOLD <- 40

#' Maximum targets to return
MAX_TARGETS <- 10

#' Weight for recent performance (last 30 days) vs ROS projections
RECENT_PERF_WEIGHT <- 0.3
ROS_PROJ_WEIGHT <- 0.7

# --- Logging helpers ---

log_info_waiver <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_warn_waiver <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}


# --- Core Functions ---

#' Generate FAAB recommendations
#'
#' Identifies top free agent targets by projected surplus value, recommends
#' drop candidates from the owner's roster, and suggests FAAB bid amounts.
#'
#' Filters recommendations by position eligibility, roster constraints (27-man
#' roster), and the $300 in-season salary cap. Weights recent 30-day performance
#' alongside ROS projections. Flags players approaching minor league promotion
#' thresholds. Returns empty list for non-contenders during playoffs.
#'
#' @param my_team Character: owner's team name
#' @param valuations Data frame of player valuations (from run_valuation_pipeline
#'   or assign_dollar_values). Must contain: player_name, dollar_value,
#'   surplus_value, position, player_type. Optionally: proj_pts_per_week,
#'   salary, cbs_id, eligible_positions, recent_pts_per_week.
#' @param rosters Full league roster data frame (from rosters.rds). Must contain:
#'   team_name, player_name, salary, player_type, position. Optionally:
#'   eligible_positions, is_minor_contract, roster_position, total_fpts.
#' @param remaining_faab Numeric: remaining FAAB budget for the owner
#' @param weeks_remaining Numeric: weeks remaining in regular season
#' @return List with:
#'   \describe{
#'     \item{targets}{Data frame of top free agent targets (up to 10), sorted
#'       by surplus value descending, with suggested bids and promotion flags}
#'     \item{drop_candidates}{Data frame of roster players sorted by lowest
#'       surplus value ascending (best drop candidates first)}
#'     \item{suggested_bids}{Named numeric vector of suggested bid amounts for
#'       each target (keyed by player_name)}
#'     \item{message}{Character: status message (e.g., "no beneficial moves")}
#'   }
#' @export
recommend_faab <- function(my_team, valuations, rosters, remaining_faab, weeks_remaining) {
  # --- Input validation ---
  if (is.null(my_team) || !is.character(my_team) || nchar(trimws(my_team)) == 0) {
    stop("'my_team' must be a non-empty character string", call. = FALSE)
  }
  if (is.null(valuations) || !is.data.frame(valuations) || nrow(valuations) == 0) {
    stop("'valuations' must be a non-empty data frame", call. = FALSE)
  }
  if (is.null(rosters) || !is.data.frame(rosters) || nrow(rosters) == 0) {
    stop("'rosters' must be a non-empty data frame", call. = FALSE)
  }
  if (!is.numeric(remaining_faab) || length(remaining_faab) != 1 || remaining_faab < 0) {
    stop("'remaining_faab' must be a non-negative numeric value", call. = FALSE)
  }
  if (!is.numeric(weeks_remaining) || length(weeks_remaining) != 1) {
    stop("'weeks_remaining' must be a single numeric value", call. = FALSE)
  }

  # --- Check non-contender playoff lock (Requirement 7.8) ---
  # If weeks_remaining <= 0 (playoffs), check if team is a non-contender
  if (weeks_remaining <= 0) {
    # During playoffs, check competitive context
    # If standings available in rosters attributes or we can infer from context
    # Non-contenders are locked from FAAB during playoffs
    log_info_waiver(sprintf(
      "Playoff period detected (weeks_remaining=%d). Non-contenders are locked.",
      weeks_remaining
    ))
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = data.frame(stringsAsFactors = FALSE),
      suggested_bids = numeric(0),
      message = "Non-contenders are locked from FAAB acquisitions during playoffs"
    ))
  }

  # If remaining_faab is 0, no bids possible
 if (remaining_faab < FAAB_MIN_BID) {
    log_info_waiver("No remaining FAAB budget for bids")
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = data.frame(stringsAsFactors = FALSE),
      suggested_bids = numeric(0),
      message = "No remaining FAAB budget ($0 available, $1 minimum bid required)"
    ))
  }

  # --- Identify my team's roster ---
  my_roster <- rosters[tolower(rosters$team_name) == tolower(my_team), , drop = FALSE]
  if (nrow(my_roster) == 0) {
    stop(sprintf("Team '%s' not found in roster data", my_team), call. = FALSE)
  }

  my_roster_size <- nrow(my_roster)
  my_salary_total <- sum(my_roster$salary, na.rm = TRUE)

  # --- Identify free agents (not on any roster) ---
  # Use fuzzy matching via player_linker to robustly identify rostered players
  # across name discrepancies (accents, suffixes, nicknames like Louie/Louis,
  # Mike/Michael, Zachary/Zach). A simple tolower() match misses these.
  all_valued_players <- valuations

  # Step 1: exact match via normalize_name (fast, catches accents/suffixes)
  rostered_players_norm <- vapply(rosters$player_name, normalize_name, character(1),
                                   USE.NAMES = FALSE)
  valued_players_norm <- vapply(valuations$player_name, normalize_name, character(1),
                                 USE.NAMES = FALSE)
  is_rostered <- valued_players_norm %in% rostered_players_norm

  # Step 2: for remaining unmatched, use fuzzy matching (catches nicknames)
  unmatched_idx <- which(!is_rostered)
  if (length(unmatched_idx) > 0) {
    # Only fuzzy-match against roster names not yet claimed by exact match
    unmatched_val_names <- valuations$player_name[unmatched_idx]
    fuzzy_results <- fuzzy_match_players(
      name_a = unmatched_val_names,
      name_b = rosters$player_name
    )
    # Mark as rostered if confidence >= 0.7
    for (i in seq_len(nrow(fuzzy_results))) {
      if (!is.na(fuzzy_results$idx_b[i]) && fuzzy_results$confidence[i] >= 0.7) {
        is_rostered[unmatched_idx[i]] <- TRUE
      }
    }
  }

  free_agents <- all_valued_players[!is_rostered, , drop = FALSE]

  if (nrow(free_agents) == 0) {
    log_info_waiver("No free agents found in valuations data")
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = data.frame(stringsAsFactors = FALSE),
      suggested_bids = numeric(0),
      message = "No free agents with projections available"
    ))
  }

  # --- Weight recent performance alongside ROS projections (Requirement 7.5) ---
  free_agents <- apply_recency_weighting(free_agents)

  # --- Compute effective surplus for free agents ---
  # Surplus for free agents = dollar_value (since they have no salary cost beyond FAAB)
  if (!"surplus_value" %in% names(free_agents)) {
    free_agents$surplus_value <- free_agents$dollar_value
  }
  # For free agents, surplus is just dollar_value (no current salary)
  free_agents$effective_surplus <- ifelse(
    is.na(free_agents$surplus_value),
    free_agents$dollar_value,
    free_agents$surplus_value
  )

  # --- Filter by constraints (Requirement 7.4) ---
  # Filter: position eligibility — ensure player can fill a roster slot
  # Filter: salary cap compliance — adding player must keep total ≤ $300
  # For free agents claimed via FAAB, their salary becomes the bid amount ($1 minimum)
  # We check that current salary + $1 (minimum) is still within cap
  cap_room <- WAIVER_SALARY_CAP - my_salary_total
  if (cap_room < FAAB_MIN_BID) {
    log_info_waiver("No salary cap room for any acquisition")
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = identify_drop_candidates(my_roster, valuations),
      suggested_bids = numeric(0),
      message = "No salary cap room available (team at $300 cap)"
    ))
  }

  # Filter: roster size constraint (27-man max)
  # If roster is full, must drop someone to add
  roster_full <- my_roster_size >= MAX_ROSTER_SIZE

  # --- Rank free agents by effective surplus (Requirement 7.1) ---
  free_agents <- free_agents[order(-free_agents$effective_surplus), , drop = FALSE]

  # Only keep players with positive surplus (Requirement 7.7)
  free_agents <- free_agents[free_agents$effective_surplus > 0, , drop = FALSE]

  if (nrow(free_agents) == 0) {
    log_info_waiver("No free agents with positive surplus value")
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = data.frame(stringsAsFactors = FALSE),
      suggested_bids = numeric(0),
      message = "No beneficial moves available: no free agent has higher projected surplus value than FAAB cost"
    ))
  }

  # --- Identify drop candidates (Requirement 7.2) ---
  drop_candidates <- identify_drop_candidates(my_roster, valuations)

  # --- Check if any free agent is better than worst rostered player ---
  # If roster is full, the best free agent must have higher surplus than worst rostered
  if (roster_full && nrow(drop_candidates) > 0) {
    worst_rostered_surplus <- drop_candidates$surplus_value[1]  # ascending order, first is worst
    # Keep only free agents better than worst rostered player
    free_agents <- free_agents[
      free_agents$effective_surplus > worst_rostered_surplus, , drop = FALSE
    ]
  }

  if (nrow(free_agents) == 0) {
    log_info_waiver("No free agents with higher surplus than rostered players")
    return(list(
      targets = data.frame(stringsAsFactors = FALSE),
      drop_candidates = drop_candidates,
      suggested_bids = numeric(0),
      message = "No beneficial moves available: no free agent has higher projected surplus value than any rostered player after accounting for FAAB cost"
    ))
  }

  # --- Take top 10 targets ---
  targets <- head(free_agents, MAX_TARGETS)

  # --- Flag players approaching promotion threshold (Requirement 7.6) ---
  targets$approaching_promotion <- flag_promotion_threshold(targets)

  # --- Compute suggested bids for each target (Requirement 7.3) ---
  suggested_bids <- vapply(seq_len(nrow(targets)), function(i) {
    # Estimate competition factor based on surplus (higher surplus = more competition)
    competition_factor <- estimate_competition_factor(
      targets$effective_surplus[i],
      free_agents$effective_surplus
    )
    suggest_bid(
      player_surplus = targets$effective_surplus[i],
      remaining_budget = remaining_faab,
      weeks_remaining = weeks_remaining,
      competition_factor = competition_factor
    )
  }, numeric(1))

  names(suggested_bids) <- targets$player_name
  targets$suggested_bid <- suggested_bids

  log_info_waiver(sprintf(
    "FAAB recommendations: %d targets found, %d drop candidates, budget=$%d",
    nrow(targets), nrow(drop_candidates), remaining_faab
  ))

  list(
    targets = targets,
    drop_candidates = drop_candidates,
    suggested_bids = suggested_bids,
    message = sprintf("Found %d recommended targets", nrow(targets))
  )
}


#' Suggest FAAB bid amount
#'
#' Computes a recommended bid based on the player's projected surplus value,
#' the owner's remaining budget, time remaining in season, and estimated
#' competition for the player.
#'
#' The formula allocates a share of remaining budget proportional to the
#' player's surplus relative to the maximum possible value, adjusted by
#' competition factor and season urgency.
#'
#' @param player_surplus Numeric: projected surplus value of the player
#' @param remaining_budget Numeric: remaining FAAB budget
#' @param weeks_remaining Numeric: weeks left in regular season
#' @param competition_factor Numeric: how many teams likely bidding (1-5 scale).
#'   1 = low competition, 5 = high competition. Default: 3.
#' @return Numeric: suggested bid amount ($1 minimum, ≤ remaining_budget)
#' @export
suggest_bid <- function(player_surplus, remaining_budget, weeks_remaining,
                        competition_factor = 3) {
  # --- Input validation ---
  if (!is.numeric(player_surplus) || length(player_surplus) != 1) {
    stop("'player_surplus' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(remaining_budget) || length(remaining_budget) != 1 ||
      remaining_budget < 0) {
    stop("'remaining_budget' must be a non-negative numeric value", call. = FALSE)
  }
  if (!is.numeric(weeks_remaining) || length(weeks_remaining) != 1) {
    stop("'weeks_remaining' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(competition_factor) || length(competition_factor) != 1 ||
      competition_factor < 1 || competition_factor > 5) {
    stop("'competition_factor' must be a numeric value between 1 and 5", call. = FALSE)
  }

  # If no budget or non-positive surplus, return minimum bid
  if (remaining_budget < FAAB_MIN_BID) {
    return(0)
  }
  if (player_surplus <= 0) {
    return(FAAB_MIN_BID)
  }

  # --- Bid calculation ---
  # Base bid: proportion of budget based on surplus value relative to a baseline
  # A player worth ~$30 surplus would warrant a higher % of budget than one worth $5
  # Normalize surplus to a 0-1 scale where $50 surplus = 1.0
  surplus_factor <- min(player_surplus / 50, 1.0)

  # Season urgency: as fewer weeks remain, each marginal pickup is more valuable

  # but also less impactful (fewer weeks to benefit). Balance:
  # Early season: conserve budget (0.6-0.8 multiplier)
  # Mid season: normal bidding (0.8-1.0 multiplier)
  # Late season: more aggressive (1.0-1.3 multiplier)
  if (weeks_remaining <= 0) {
    urgency_factor <- 0  # playoffs, shouldn't be bidding
  } else if (weeks_remaining >= 18) {
    # Early season: conserve
    urgency_factor <- 0.7
  } else if (weeks_remaining >= 10) {
    # Mid season: moderate
    urgency_factor <- 0.9
  } else if (weeks_remaining >= 5) {
    # Late season: aggressive
    urgency_factor <- 1.1
  } else {
    # Final weeks: very aggressive (win-now)
    urgency_factor <- 1.3
  }

  # Competition adjustment: more competition → bid higher to win
  # Scale: 1 (low) to 5 (high) → multiplier 0.7 to 1.5
  competition_multiplier <- 0.5 + (competition_factor / 5) * 1.0

  # Base bid as percentage of remaining budget
  # surplus_factor controls what fraction of budget this player is "worth"
  # A top FA (~$50 surplus) with high competition late in season might warrant 30-40% of budget
  base_pct <- surplus_factor * 0.30  # max 30% of budget for a single player

  # Apply adjustments
  bid_pct <- base_pct * urgency_factor * competition_multiplier

  # Compute raw bid
  raw_bid <- remaining_budget * bid_pct

  # --- Enforce constraints ---
  # Minimum bid = $1 (Requirement 7.3 / Constitution 2.8)
  # Maximum bid = remaining budget
  bid <- max(FAAB_MIN_BID, round(raw_bid))
  bid <- min(bid, remaining_budget)

  bid
}


# --- Internal helper functions ---

#' Apply recency weighting to free agent valuations
#'
#' Blends recent 30-day performance (if available) with ROS projections
#' to produce a weighted effective value. Recent hot streaks and breakouts
#' are captured through this weighting.
#'
#' @param players Data frame of player valuations
#' @return Data frame with effective_surplus adjusted by recency weighting
#' @keywords internal
apply_recency_weighting <- function(players) {
  # If recent_pts_per_week column exists, blend with ROS projections
  if ("recent_pts_per_week" %in% names(players) &&
      "proj_pts_per_week" %in% names(players)) {

    has_recent <- !is.na(players$recent_pts_per_week)

    # Blended value = weight * recent + (1-weight) * ROS
    players$blended_pts_per_week <- players$proj_pts_per_week
    players$blended_pts_per_week[has_recent] <-
      RECENT_PERF_WEIGHT * players$recent_pts_per_week[has_recent] +
      ROS_PROJ_WEIGHT * players$proj_pts_per_week[has_recent]

    # Adjust dollar value proportionally
    if (any(has_recent)) {
      ratio <- ifelse(
        players$proj_pts_per_week > 0,
        players$blended_pts_per_week / players$proj_pts_per_week,
        1
      )
      players$dollar_value <- players$dollar_value * ratio
      # Recalculate surplus
      if ("salary" %in% names(players)) {
        players$surplus_value <- players$dollar_value - players$salary
      } else {
        players$surplus_value <- players$dollar_value
      }
    }
  }

  players
}


#' Identify drop candidates on owner's roster
#'
#' Finds players on the owner's roster with the lowest surplus value,
#' considering salary freed vs. value lost.
#'
#' @param my_roster Data frame: owner's roster (from rosters data)
#' @param valuations Data frame: current player valuations
#' @return Data frame of drop candidates sorted by surplus ascending (worst first)
#' @keywords internal
identify_drop_candidates <- function(my_roster, valuations) {
  # Match roster players to their valuations
  roster_vals <- merge(
    my_roster,
    valuations[, intersect(names(valuations),
      c("player_name", "dollar_value", "surplus_value", "proj_pts_per_week",
        "keeper_value_3yr", "keeper_value_5yr")), drop = FALSE],
    by = "player_name",
    all.x = TRUE,
    suffixes = c("", ".val")
  )

  # For players without valuations, they are likely unproductive → good drop candidates
  roster_vals$surplus_value <- ifelse(
    is.na(roster_vals$surplus_value),
    -roster_vals$salary,  # No value - treat as negative surplus equal to salary
    roster_vals$surplus_value
  )

  # Handle case where salary column might have NAs
  roster_vals$surplus_value[is.na(roster_vals$surplus_value)] <- 0

  # Sort by surplus ascending (worst surplus = best drop candidate)
  roster_vals <- roster_vals[order(roster_vals$surplus_value), , drop = FALSE]
  rownames(roster_vals) <- NULL

  roster_vals
}


#' Flag players approaching minor league promotion threshold
#'
#' Players approaching 130 AB or 50 IP are flagged as potential targets
#' since they may be dropped by other teams needing to promote or release them.
#'
#' Uses the approaching thresholds (110 AB / 40 IP) to flag players who are
#' close to the hard promotion limit.
#'
#' @param players Data frame of player data. Checks for columns:
#'   ab (at-bats), ip (innings pitched), is_minor_contract, roster_position.
#' @return Logical vector: TRUE if player is approaching promotion threshold
#' @keywords internal
flag_promotion_threshold <- function(players) {
  n <- nrow(players)
  flags <- rep(FALSE, n)

  # Check for AB threshold
  if ("ab" %in% names(players)) {
    ab_approaching <- !is.na(players$ab) & players$ab >= APPROACHING_AB_THRESHOLD
    flags <- flags | ab_approaching
  }

  # Check for IP threshold
  if ("ip" %in% names(players)) {
    ip_approaching <- !is.na(players$ip) & players$ip >= APPROACHING_IP_THRESHOLD
    flags <- flags | ip_approaching
  }

  # Only flag if the player is on a minor league contract or in minors
  if ("is_minor_contract" %in% names(players)) {
    is_minor <- !is.na(players$is_minor_contract) & players$is_minor_contract
    flags <- flags & is_minor
  } else if ("roster_position" %in% names(players)) {
    is_minor <- grepl("minor|ml|mn", tolower(players$roster_position), perl = TRUE)
    flags <- flags & is_minor
  } else if ("pro_status" %in% names(players)) {
    is_minor <- !is.na(players$pro_status) & players$pro_status == "M"
    flags <- flags & is_minor
  }

  flags
}


#' Estimate competition factor for a free agent
#'
#' Estimates how many teams are likely to bid on a given free agent based
#' on the player's surplus value relative to other available free agents.
#' Players with higher surplus attract more competition.
#'
#' @param player_surplus Numeric: surplus value for the player
#' @param all_surpluses Numeric vector: surplus values for all free agents
#' @return Numeric: competition factor on 1-5 scale
#' @keywords internal
estimate_competition_factor <- function(player_surplus, all_surpluses) {
  if (length(all_surpluses) == 0 || all(is.na(all_surpluses))) {
    return(3)  # default middle competition
  }

  # Remove NAs
  valid_surpluses <- all_surpluses[!is.na(all_surpluses)]
  if (length(valid_surpluses) == 0) return(3)

  # Compute percentile rank of this player's surplus
  percentile <- sum(valid_surpluses <= player_surplus) / length(valid_surpluses)

  # Map percentile to 1-5 scale
  # Top 10% → 5 (high competition)
  # Top 25% → 4
  # Top 50% → 3
  # Bottom 50% → 2
  # Bottom 25% → 1
  if (percentile >= 0.90) {
    5
  } else if (percentile >= 0.75) {
    4
  } else if (percentile >= 0.50) {
    3
  } else if (percentile >= 0.25) {
    2
  } else {
    1
  }
}

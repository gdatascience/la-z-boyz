#' Trade Analyzer — Multi-dimensional trade evaluation
#'
#' Evaluates proposed trades for fairness, strategic value, salary cap impact,
#' and positional fit. Uses surplus values from the Player_Valuation_Engine,
#' roster data, and standings context to produce actionable recommendations.
#'
#' Requirements: 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 6.10

# Source dependencies (when run standalone)
# source("R/utils/salary_rules.R")
# source("R/utils/scoring.R")
# source("R/utils/keeper_value.R")
# source("R/analysis/player_valuation.R")

# --- Constants ---

#' In-season salary cap
TRADE_SALARY_CAP <- 300

#' Number of regular-season weeks (for playoff probability estimation)
REGULAR_SEASON_WEEKS <- 23

#' Playoff spots available
PLAYOFF_SPOTS <- 6

#' Number of teams in the league
LEAGUE_TEAMS <- 16

# --- Logging helpers ---

log_info_trade <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_warn_trade <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_error_trade <- function(msg) {
  message(sprintf("[%s] ERROR: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}


# --- Core Functions ---

#' Analyze a proposed trade
#'
#' Evaluates a trade proposal across multiple dimensions: net surplus value,
#' projected points change, salary cap impact, keeper implications, positional
#' fit, prospect value, and lopsidedness detection.
#'
#' @param give Character vector of player names being traded away
#' @param receive Character vector of player names being received
#' @param my_team Character: owner's team name
#' @param valuations Data frame of player valuations (from run_valuation_pipeline
#'   or assign_dollar_values), must contain: player_name, dollar_value,
#'   surplus_value, position, player_type. Optionally: proj_pts_per_week, salary.
#' @param rosters Full league roster data frame (from rosters.rds), must contain:
#'   team_name, player_name, salary, player_type, position or eligible_positions.
#' @param standings Current standings data frame (from standings.rds), must contain:
#'   team_name, wins, losses, games_back, division, total_points.
#' @param roster_slots Named vector of positions and counts (default: league standard).
#' @param draft_data Optional data frame of MLB draft data for prospect analysis.
#' @return List with:
#'   \describe{
#'     \item{summary}{Character: human-readable trade summary}
#'     \item{value_diff}{Numeric: net surplus value difference (positive = trade favors you)}
#'     \item{salary_impact}{List from compute_salary_impact: net_change, new_total, cap_compliant}
#'     \item{keeper_analysis}{List: keeper implications for each player in the trade}
#'     \item{positional_impact}{List: positions improved, weakened, over-filled, empty}
#'     \item{prospect_analysis}{List: prospect profiles for minor league players (if any)}
#'     \item{is_lopsided}{Logical: TRUE if trade value difference exceeds 20% threshold}
#'     \item{recommendation}{Character: "accept", "reject", or "counter"}
#'     \item{justification}{Character: detailed reasoning for the recommendation}
#'   }
#' @export
analyze_trade <- function(give, receive, my_team, valuations, rosters, standings,
                          roster_slots = NULL, draft_data = NULL) {
  # --- Input validation ---
  if (is.null(give) || length(give) == 0) {
    stop("'give' must contain at least one player name", call. = FALSE)
  }
  if (is.null(receive) || length(receive) == 0) {
    stop("'receive' must contain at least one player name", call. = FALSE)
  }
  if (is.null(my_team) || nchar(my_team) == 0) {
    stop("'my_team' must be a non-empty team name", call. = FALSE)
  }

  # --- Resolve players from roster data (Requirement 6.10) ---
  # Check that all players in the trade exist in roster data
  all_trade_players <- c(give, receive)
  roster_names <- rosters$player_name

  unresolved <- all_trade_players[!tolower(all_trade_players) %in% tolower(roster_names)]
  if (length(unresolved) > 0) {
    stop(
      sprintf(
        "Trade rejected: player(s) not found in roster data: %s",
        paste(unresolved, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # --- Look up player data from rosters ---
  give_roster <- rosters[tolower(rosters$player_name) %in% tolower(give), , drop = FALSE]
  receive_roster <- rosters[tolower(rosters$player_name) %in% tolower(receive), , drop = FALSE]

  # --- Look up valuations ---
  give_vals <- valuations[tolower(valuations$player_name) %in% tolower(give), , drop = FALSE]
  receive_vals <- valuations[tolower(valuations$player_name) %in% tolower(receive), , drop = FALSE]

  # --- Compute net surplus value difference (Requirement 6.1) ---
  surplus_give <- if (nrow(give_vals) > 0 && "surplus_value" %in% names(give_vals)) {
    sum(give_vals$surplus_value, na.rm = TRUE)
  } else {
    0
  }

  surplus_receive <- if (nrow(receive_vals) > 0 && "surplus_value" %in% names(receive_vals)) {
    sum(receive_vals$surplus_value, na.rm = TRUE)
  } else {
    0
  }

  value_diff <- surplus_receive - surplus_give

  # --- Compute projected pts/week change (Requirement 6.2) ---
  pts_per_week_change <- compute_pts_week_change(give_vals, receive_vals)

  # --- Compute salary cap impact (Requirement 6.3) ---
  # Get current team salary total
  my_roster <- rosters[tolower(rosters$team_name) == tolower(my_team), , drop = FALSE]
  current_total <- if (nrow(my_roster) > 0 && "salary" %in% names(my_roster)) {
    sum(my_roster$salary, na.rm = TRUE)
  } else {
    0
  }

  # Build players_in and players_out for salary impact
  players_out <- if (nrow(give_roster) > 0 && "salary" %in% names(give_roster)) {
    give_roster[, c("player_name", "salary"), drop = FALSE]
  } else {
    data.frame(player_name = character(0), salary = numeric(0), stringsAsFactors = FALSE)
  }

  players_in <- if (nrow(receive_roster) > 0 && "salary" %in% names(receive_roster)) {
    receive_roster[, c("player_name", "salary"), drop = FALSE]
  } else {
    data.frame(player_name = character(0), salary = numeric(0), stringsAsFactors = FALSE)
  }

  salary_impact <- compute_salary_impact(players_in, players_out, current_total)

  # --- Assess competitive context (Requirement 6.6) ---
  context <- assess_competitive_context(my_team, standings)

  # --- Keeper implications (Requirement 6.4) ---
  keeper_analysis <- analyze_keeper_implications(give_vals, receive_vals, rosters)

  # --- Positional impact (Requirement 6.5) ---
  if (is.null(roster_slots)) {
    roster_slots <- c(C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
                      OF = 3, U = 1, SP = 5, RP = 2)
  }
  positional_impact <- analyze_positional_impact(
    give_vals, receive_vals, my_roster, roster_slots
  )

  # --- Prospect analysis (Requirement 6.7, 6.8) ---
  prospect_analysis <- list()
  all_trade_roster <- rbind(give_roster, receive_roster)
  minor_players <- all_trade_roster[
    isTRUE_vec(all_trade_roster$is_minor_contract), , drop = FALSE
  ]
  if (nrow(minor_players) > 0) {
    for (i in seq_len(nrow(minor_players))) {
      pname <- minor_players$player_name[i]
      prospect_analysis[[pname]] <- analyze_prospect_value(pname, rosters, draft_data)
    }
  }

  # --- Lopsided trade detection (Requirement 6.9) ---
  lopsided <- is_trade_lopsided(give_vals, receive_vals)

  # --- Generate recommendation (Requirement 6.9) ---
  recommendation <- generate_recommendation(
    value_diff = value_diff,
    pts_change = pts_per_week_change$total,
    salary_impact = salary_impact,
    context = context
  )

  # --- Generate justification ---
  justification <- build_trade_justification(
    value_diff = value_diff,
    pts_change = pts_per_week_change,
    salary_impact = salary_impact,
    keeper_analysis = keeper_analysis,
    positional_impact = positional_impact,
    context = context,
    lopsided = lopsided,
    recommendation = recommendation
  )

  # --- Build summary ---
  summary_text <- build_trade_summary(
    give = give,
    receive = receive,
    value_diff = value_diff,
    pts_change = pts_per_week_change,
    salary_impact = salary_impact,
    context = context,
    recommendation = recommendation
  )

  log_info_trade(sprintf(
    "Trade analyzed: giving [%s] for [%s] — recommendation: %s",
    paste(give, collapse = ", "),
    paste(receive, collapse = ", "),
    recommendation
  ))

  list(
    summary = summary_text,
    value_diff = value_diff,
    salary_impact = salary_impact,
    keeper_analysis = keeper_analysis,
    positional_impact = positional_impact,
    prospect_analysis = prospect_analysis,
    is_lopsided = lopsided,
    recommendation = recommendation,
    justification = justification
  )
}


#' Determine team competitive context
#'
#' Assesses whether a team should be in contend, rebuild, or middle mode
#' based on current standings, winning percentage, and games back.
#'
#' @param team_name Character: team name to look up
#' @param standings Data frame from standings.rds with: team_name, wins,
#'   losses, games_back, division, total_points, pct.
#' @return List with:
#'   \describe{
#'     \item{mode}{Character: "contend", "rebuild", or "middle"}
#'     \item{playoff_prob}{Numeric: estimated playoff probability (0-1)}
#'     \item{games_back}{Numeric: games behind division leader}
#'     \item{division_rank}{Integer: rank within division (1-4)}
#'   }
#' @export
assess_competitive_context <- function(team_name, standings) {
  if (is.null(standings) || nrow(standings) == 0) {
    log_warn_trade("Standings data is empty; returning default middle context")
    return(list(
      mode = "middle",
      playoff_prob = 0.375,
      games_back = NA_real_,
      division_rank = NA_integer_
    ))
  }

  # Find the team in standings (case-insensitive)
  team_row <- standings[tolower(standings$team_name) == tolower(team_name), , drop = FALSE]

  if (nrow(team_row) == 0) {
    log_warn_trade(sprintf("Team '%s' not found in standings; returning default context", team_name))
    return(list(
      mode = "middle",
      playoff_prob = 0.375,
      games_back = NA_real_,
      division_rank = NA_integer_
    ))
  }

  # Use first match if duplicates exist

  team_row <- team_row[1, , drop = FALSE]

  # Extract team's division

  team_division <- team_row$division
  games_back <- if ("games_back" %in% names(team_row)) {
    as.numeric(team_row$games_back)
  } else {
    NA_real_
  }
  if (is.na(games_back)) games_back <- 0

  # Compute division rank
  div_teams <- standings[standings$division == team_division, , drop = FALSE]
  div_teams <- div_teams[order(-div_teams$pct, -div_teams$total_points), ]
  division_rank <- which(tolower(div_teams$team_name) == tolower(team_name))
  division_rank <- if (length(division_rank) > 0) division_rank[1] else NA_integer_

  # Compute overall rank for wildcard consideration
  all_ranked <- standings[order(-standings$pct, -standings$total_points), ]
  overall_rank <- which(tolower(all_ranked$team_name) == tolower(team_name))
  overall_rank <- if (length(overall_rank) > 0) overall_rank[1] else LEAGUE_TEAMS

  # Estimate playoff probability based on position and games back
  # Simple heuristic:
  #   - Division leaders (rank 1): high probability
  #   - Within 3 games back: moderate probability
  #   - More than 6 games back: low probability
  #   - Overall rank within PLAYOFF_SPOTS: boost
  winning_pct <- as.numeric(team_row$pct)
  if (is.na(winning_pct)) winning_pct <- 0.5

  # Base probability from winning percentage relative to field
  base_prob <- winning_pct

  # Adjust for games back
  if (games_back <= 0) {
    playoff_prob <- min(0.95, base_prob + 0.3)
  } else if (games_back <= 3) {
    playoff_prob <- min(0.80, base_prob + 0.1)
  } else if (games_back <= 6) {
    playoff_prob <- max(0.15, base_prob - 0.1)
  } else {
    playoff_prob <- max(0.05, base_prob - 0.3)
  }

  # Boost if overall rank is within playoff spots

  if (overall_rank <= PLAYOFF_SPOTS) {
    playoff_prob <- min(1.0, playoff_prob + 0.1)
  }

  # Clamp to [0, 1]
  playoff_prob <- max(0, min(1, playoff_prob))

  # Determine mode
  mode <- if (playoff_prob >= 0.6) {
    "contend"
  } else if (playoff_prob <= 0.25) {
    "rebuild"
  } else {
    "middle"
  }

  list(
    mode = mode,
    playoff_prob = round(playoff_prob, 3),
    games_back = games_back,
    division_rank = division_rank
  )
}


# --- Internal helper functions ---

#' Compute projected points-per-week change from a trade
#'
#' Breaks down points change by batting and pitching contributions.
#'
#' @param give_vals Valuation data for players being given away
#' @param receive_vals Valuation data for players being received
#' @return List with: total, batting, pitching
#' @keywords internal
compute_pts_week_change <- function(give_vals, receive_vals) {
  # Helper to sum pts/week for a subset by player type
  sum_pts <- function(vals, player_type = NULL) {
    if (is.null(vals) || nrow(vals) == 0) return(0)
    if (!"proj_pts_per_week" %in% names(vals)) return(0)
    if (!is.null(player_type) && "player_type" %in% names(vals)) {
      vals <- vals[vals$player_type == player_type, , drop = FALSE]
    }
    sum(vals$proj_pts_per_week, na.rm = TRUE)
  }

  give_batting <- sum_pts(give_vals, "Batter")
  give_pitching <- sum_pts(give_vals, "Pitcher")
  receive_batting <- sum_pts(receive_vals, "Batter")
  receive_pitching <- sum_pts(receive_vals, "Pitcher")

  batting_change <- receive_batting - give_batting
  pitching_change <- receive_pitching - give_pitching
  total_change <- batting_change + pitching_change

  list(
    total = total_change,
    batting = batting_change,
    pitching = pitching_change
  )
}


#' Generate trade recommendation based on multi-dimensional analysis
#'
#' @param value_diff Net surplus value difference
#' @param pts_change Total projected pts/week change
#' @param salary_impact Salary impact list
#' @param context Competitive context list
#' @return Character: "accept", "reject", or "counter"
#' @keywords internal
generate_recommendation <- function(value_diff, pts_change, salary_impact, context) {
  # Reject if salary cap violated

  if (!salary_impact$cap_compliant) {
    return("reject")
  }

  # Scoring: weight value and points change based on competitive mode
  if (context$mode == "contend") {
    # Contenders value immediate points more
    score <- (0.4 * value_diff) + (0.6 * pts_change)
  } else if (context$mode == "rebuild") {
    # Rebuilders value surplus (keeper potential) more
    score <- (0.8 * value_diff) + (0.2 * pts_change)
  } else {
    # Middle teams balance both
    score <- (0.6 * value_diff) + (0.4 * pts_change)
  }

  # Decision thresholds
  if (score > 1) {
    "accept"
  } else if (score < -3) {
    "reject"
  } else {
    "counter"
  }
}


#' Build a human-readable trade summary
#'
#' @param give Character vector of players given
#' @param receive Character vector of players received
#' @param value_diff Net surplus difference
#' @param pts_change Points change list (total, batting, pitching)
#' @param salary_impact Salary impact list
#' @param context Competitive context list
#' @param recommendation Character recommendation
#' @return Character: formatted summary text
#' @keywords internal
build_trade_summary <- function(give, receive, value_diff, pts_change,
                                salary_impact, context, recommendation) {
  lines <- character(0)

  lines <- c(lines, "=== TRADE ANALYSIS ===")
  lines <- c(lines, sprintf("Give: %s", paste(give, collapse = ", ")))
  lines <- c(lines, sprintf("Receive: %s", paste(receive, collapse = ", ")))
  lines <- c(lines, "")

  # Value analysis
  direction <- if (value_diff > 0) "in your favor" else if (value_diff < 0) "against you" else "even"
  lines <- c(lines, sprintf("Net Surplus Value: %+.1f (%s)", value_diff, direction))
  lines <- c(lines, "")

  # Points impact
  lines <- c(lines, "Projected Pts/Week Change:")
  lines <- c(lines, sprintf("  Total:    %+.1f pts/wk", pts_change$total))
  lines <- c(lines, sprintf("  Batting:  %+.1f pts/wk", pts_change$batting))
  lines <- c(lines, sprintf("  Pitching: %+.1f pts/wk", pts_change$pitching))
  lines <- c(lines, "")

  # Salary impact
  lines <- c(lines, "Salary Cap Impact:")
  lines <- c(lines, sprintf("  Net salary change: %+.0f", salary_impact$net_change))
  lines <- c(lines, sprintf("  New team total: $%.0f / $%d", salary_impact$new_total, TRADE_SALARY_CAP))
  cap_status <- if (salary_impact$cap_compliant) "COMPLIANT" else "VIOLATION"
  lines <- c(lines, sprintf("  Cap status: %s", cap_status))
  lines <- c(lines, "")

  # Competitive context
  lines <- c(lines, sprintf("Team Mode: %s (playoff prob: %.0f%%)",
                            toupper(context$mode), context$playoff_prob * 100))
  if (!is.na(context$division_rank)) {
    lines <- c(lines, sprintf("  Division rank: %d, Games back: %.1f",
                              context$division_rank, context$games_back))
  }
  lines <- c(lines, "")

  # Recommendation
  rec_text <- switch(recommendation,
    "accept" = "ACCEPT — trade improves your team",
    "reject" = "REJECT — trade is unfavorable or violates cap",
    "counter" = "COUNTER — trade is close but could be better"
  )
  lines <- c(lines, sprintf("Recommendation: %s", rec_text))

  paste(lines, collapse = "\n")
}


# --- Task 8.2: Keeper, Positional, Prospect, and Lopsided Trade Analysis ---

#' Vectorized isTRUE helper for logical vectors
#' @param x Logical vector (may contain NA)
#' @return Logical vector with NA treated as FALSE
#' @keywords internal
isTRUE_vec <- function(x) {
  if (is.null(x)) return(logical(0))
  !is.na(x) & x
}


#' Analyze keeper implications for trade players
#'
#' For each player in the trade, computes salary escalation path, 3-year
#' keeper value, and keeper eligibility status.
#'
#' @param give_vals Valuation data for players being given away
#' @param receive_vals Valuation data for players being received
#' @param rosters Full league roster data (for salary/contract info)
#' @return List with give and receive sublists, each containing per-player
#'   keeper analysis: current_salary, escalation_path, keeper_value_3yr,
#'   keeper_eligible.
#' @export
analyze_keeper_implications <- function(give_vals, receive_vals, rosters) {
  analyze_one_side <- function(vals, rosters) {
    if (is.null(vals) || nrow(vals) == 0) return(list())

    results <- list()
    for (i in seq_len(nrow(vals))) {
      pname <- vals$player_name[i]

      # Look up roster info for salary/contract details
      roster_row <- rosters[tolower(rosters$player_name) == tolower(pname), , drop = FALSE]
      if (nrow(roster_row) == 0) next
      roster_row <- roster_row[1, , drop = FALSE]

      current_salary <- if ("salary" %in% names(roster_row)) {
        as.numeric(roster_row$salary)
      } else {
        0
      }

      is_minor <- if ("is_minor_contract" %in% names(roster_row)) {
        isTRUE(roster_row$is_minor_contract)
      } else {
        FALSE
      }

      minor_track_yr <- if (is_minor && "minor_track_year" %in% names(roster_row)) {
        as.integer(roster_row$minor_track_year)
      } else if (is_minor) {
        # Infer track year from salary if not explicitly stored
        if (current_salary == 0) 0L
        else if (current_salary == 1) 1L
        else if (current_salary == 2) 2L
        else if (current_salary == 3) 3L
        else 3L
      } else {
        0L
      }

      # Compute 3-year salary escalation path
      escalation_path <- project_keeper_salary(
        current_salary = current_salary,
        is_minor_contract = is_minor,
        minor_track_year = minor_track_yr,
        years_ahead = 3
      )

      # Compute 3-year keeper value using projected dollar value
      # Use current dollar_value as baseline (simple approximation: slight decay)
      dollar_val <- if ("dollar_value" %in% names(vals)) {
        as.numeric(vals$dollar_value[i])
      } else {
        0
      }
      # Project values with slight annual decay (0.95 per year)
      projected_values_3yr <- dollar_val * (0.95 ^ (1:3))

      keeper_surplus <- compute_keeper_surplus(
        projected_values = projected_values_3yr,
        projected_salaries = escalation_path,
        discount_rate = 0.9
      )

      # Keeper eligibility: players acquired via post-deadline FAAB are NOT eligible
      # Check for acquisition_type or faab_post_deadline flag in roster data
      keeper_eligible <- TRUE
      if ("acquisition_type" %in% names(roster_row)) {
        if (!is.na(roster_row$acquisition_type) &&
            tolower(roster_row$acquisition_type) == "faab_post_deadline") {
          keeper_eligible <- FALSE
        }
      }
      if ("keeper_eligible" %in% names(roster_row)) {
        keeper_eligible <- isTRUE(roster_row$keeper_eligible)
      }

      results[[pname]] <- list(
        current_salary = current_salary,
        is_minor_contract = is_minor,
        escalation_path = escalation_path,
        keeper_value_3yr = keeper_surplus$npv_surplus,
        annual_surplus = keeper_surplus$annual_surplus,
        keeper_eligible = keeper_eligible
      )
    }
    results
  }

  list(
    give = analyze_one_side(give_vals, rosters),
    receive = analyze_one_side(receive_vals, rosters)
  )
}


#' Analyze positional impact of a trade
#'
#' Identifies positions that are improved, weakened, over-filled, or left
#' empty relative to the league's roster slot requirements.
#'
#' @param give_vals Valuation data for players being given away
#' @param receive_vals Valuation data for players being received
#' @param my_roster Data frame of current team roster (before trade)
#' @param roster_slots Named vector of position slots (e.g., c(C=1, SS=1, OF=3))
#' @return List with:
#'   \describe{
#'     \item{improved}{Character vector of positions getting better}
#'     \item{weakened}{Character vector of positions getting worse}
#'     \item{overfilled}{Character vector of positions with more players than slots}
#'     \item{unfilled}{Character vector of positions with no player after trade}
#'     \item{details}{Named list with per-position before/after counts}
#'   }
#' @export
analyze_positional_impact <- function(give_vals, receive_vals, my_roster,
                                      roster_slots) {
  # Determine position column to use
  get_position <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(character(0))
    if ("position" %in% names(df)) {
      return(as.character(df$position))
    } else if ("eligible_positions" %in% names(df)) {
      # Take primary (first) position from comma-separated list
      return(vapply(strsplit(as.character(df$eligible_positions), ","),
                    function(x) trimws(x[1]), character(1)))
    }
    return(rep(NA_character_, nrow(df)))
  }

  # Get positions of players going out and coming in
  give_positions <- get_position(give_vals)
  receive_positions <- get_position(receive_vals)

  # Get current roster positions
  roster_positions <- get_position(my_roster)

  # Count current roster players per position
  pos_names <- names(roster_slots)
  current_counts <- vapply(pos_names, function(pos) {
    sum(roster_positions == pos, na.rm = TRUE)
  }, integer(1))

  # Count players leaving by position
  give_counts <- vapply(pos_names, function(pos) {
    sum(give_positions == pos, na.rm = TRUE)
  }, integer(1))

  # Count players arriving by position
  receive_counts <- vapply(pos_names, function(pos) {
    sum(receive_positions == pos, na.rm = TRUE)
  }, integer(1))

  # Post-trade counts
  post_trade_counts <- current_counts - give_counts + receive_counts

  # Classify each position
improved <- character(0)
  weakened <- character(0)
  overfilled <- character(0)
  unfilled <- character(0)

  details <- list()
  for (pos in pos_names) {
    before <- current_counts[[pos]]
    after <- post_trade_counts[[pos]]
    slots <- roster_slots[[pos]]

    details[[pos]] <- list(before = before, after = after, slots = slots)

    # Net change in position quality (simple: more = better if under-filled)
    net <- receive_counts[[pos]] - give_counts[[pos]]

    if (net > 0) {
      improved <- c(improved, pos)
    } else if (net < 0) {
      weakened <- c(weakened, pos)
    }

    if (after > slots) {
      overfilled <- c(overfilled, pos)
    }
    if (after == 0 && slots > 0) {
      unfilled <- c(unfilled, pos)
    }
  }

  list(
    improved = improved,
    weakened = weakened,
    overfilled = overfilled,
    unfilled = unfilled,
    details = details
  )
}


#' Analyze prospect value for a minor league player
#'
#' Assesses a minor league player's value based on draft pedigree, salary
#' track position, and available prospect information.
#'
#' @param player_name Character: player name to look up
#' @param rosters Full league roster data
#' @param draft_data Optional data frame of MLB draft data (from draft_{year}.rds).
#'   Expected columns: player_name (or fullName), round, pick_number (or pickNumber),
#'   signing_bonus (or signingBonus), school, year.
#' @return List with:
#'   \describe{
#'     \item{player_name}{Character: player name}
#'     \item{salary_track}{List: current salary, is_minor, track_year, 5yr projection}
#'     \item{draft_pedigree}{List: round, pick, bonus, school (or NULL if not found)}
#'     \item{prospect_tier}{Character: "elite", "high", "mid", or "low"}
#'     \item{keeper_value_5yr}{Numeric: NPV surplus over 5 years on minor track}
#'   }
#' @export
analyze_prospect_value <- function(player_name, rosters, draft_data = NULL) {
  # Look up roster info
  roster_row <- rosters[tolower(rosters$player_name) == tolower(player_name), , drop = FALSE]

  current_salary <- 0
  is_minor <- TRUE
  minor_track_yr <- 0L

  if (nrow(roster_row) > 0) {
    roster_row <- roster_row[1, , drop = FALSE]
    current_salary <- if ("salary" %in% names(roster_row)) {
      as.numeric(roster_row$salary)
    } else {
      0
    }
    is_minor <- if ("is_minor_contract" %in% names(roster_row)) {
      isTRUE(roster_row$is_minor_contract)
    } else {
      TRUE
    }
    minor_track_yr <- if ("minor_track_year" %in% names(roster_row)) {
      as.integer(roster_row$minor_track_year)
    } else {
      if (current_salary == 0) 0L
      else if (current_salary == 1) 1L
      else if (current_salary == 2) 2L
      else if (current_salary == 3) 3L
      else 3L
    }
  }

  # 5-year salary projection on minor league track
  salary_5yr <- project_keeper_salary(
    current_salary = current_salary,
    is_minor_contract = is_minor,
    minor_track_year = minor_track_yr,
    years_ahead = 5
  )

  salary_track <- list(
    current_salary = current_salary,
    is_minor_contract = is_minor,
    track_year = minor_track_yr,
    projected_salaries_5yr = salary_5yr
  )

  # Draft pedigree lookup
  draft_pedigree <- NULL
  if (!is.null(draft_data) && nrow(draft_data) > 0) {
    # Try matching player name (handle different column naming conventions)
    name_col <- if ("player_name" %in% names(draft_data)) {
      "player_name"
    } else if ("fullName" %in% names(draft_data)) {
      "fullName"
    } else if ("name" %in% names(draft_data)) {
      "name"
    } else {
      NULL
    }

    if (!is.null(name_col)) {
      match_idx <- which(tolower(draft_data[[name_col]]) == tolower(player_name))
      if (length(match_idx) > 0) {
        draft_row <- draft_data[match_idx[1], , drop = FALSE]
        round <- if ("round" %in% names(draft_row)) draft_row$round else NA
        pick <- if ("pick_number" %in% names(draft_row)) {
          draft_row$pick_number
        } else if ("pickNumber" %in% names(draft_row)) {
          draft_row$pickNumber
        } else {
          NA
        }
        bonus <- if ("signing_bonus" %in% names(draft_row)) {
          draft_row$signing_bonus
        } else if ("signingBonus" %in% names(draft_row)) {
          draft_row$signingBonus
        } else {
          NA
        }
        school <- if ("school" %in% names(draft_row)) draft_row$school else NA

        draft_pedigree <- list(
          round = as.integer(round),
          pick = as.integer(pick),
          signing_bonus = as.numeric(bonus),
          school = as.character(school)
        )
      }
    }
  }

  # Determine prospect tier based on draft pedigree
  prospect_tier <- classify_prospect_tier(draft_pedigree)

  # Compute 5-year keeper value on minor league salary track
  # Use a modest projected value for prospects based on tier
  base_value <- switch(prospect_tier,
    "elite" = 25,
    "high" = 18,
    "mid" = 12,
    "low" = 7
  )
  # Assume value ramps up over 5 years for prospects
  projected_values_5yr <- base_value * c(0.4, 0.6, 0.8, 1.0, 1.0)

  keeper_surplus_5yr <- compute_keeper_surplus(
    projected_values = projected_values_5yr,
    projected_salaries = salary_5yr,
    discount_rate = 0.9
  )

  list(
    player_name = player_name,
    salary_track = salary_track,
    draft_pedigree = draft_pedigree,
    prospect_tier = prospect_tier,
    keeper_value_5yr = keeper_surplus_5yr$npv_surplus
  )
}


#' Classify prospect tier based on draft pedigree
#'
#' @param draft_pedigree List with round, pick, signing_bonus (or NULL)
#' @return Character: "elite", "high", "mid", or "low"
#' @keywords internal
classify_prospect_tier <- function(draft_pedigree) {
  if (is.null(draft_pedigree)) return("low")

  round <- draft_pedigree$round
  pick <- draft_pedigree$pick
  bonus <- draft_pedigree$signing_bonus

  # Classify based on draft round and signing bonus
  if (!is.na(round) && round == 1) {
    # First round picks
    if (!is.na(pick) && pick <= 10) return("elite")
    return("high")
  }

  if (!is.na(round) && round == 2) {
    return("high")
  }

  if (!is.na(bonus) && bonus >= 1000000) {
    # High bonus regardless of round suggests elite talent
    return("high")
  }

  if (!is.na(round) && round <= 5) {
    return("mid")
  }

  "low"
}


#' Detect lopsided trades
#'
#' A trade is lopsided when the absolute net surplus difference exceeds
#' 20% of the total trade value (sum of absolute dollar values on both sides).
#'
#' @param give_vals Valuation data for players being given away
#' @param receive_vals Valuation data for players being received
#' @return Logical: TRUE if the trade exceeds the 20% lopsided threshold
#' @export
is_trade_lopsided <- function(give_vals, receive_vals) {
  # Get absolute dollar values for each side
  give_total <- if (!is.null(give_vals) && nrow(give_vals) > 0 &&
                    "dollar_value" %in% names(give_vals)) {
    sum(abs(give_vals$dollar_value), na.rm = TRUE)
  } else {
    0
  }

  receive_total <- if (!is.null(receive_vals) && nrow(receive_vals) > 0 &&
                       "dollar_value" %in% names(receive_vals)) {
    sum(abs(receive_vals$dollar_value), na.rm = TRUE)
  } else {
    0
  }

  total_trade_value <- give_total + receive_total

  # Avoid division by zero

  if (total_trade_value == 0) return(FALSE)

  # Net surplus difference
  surplus_give <- if (!is.null(give_vals) && nrow(give_vals) > 0 &&
                      "surplus_value" %in% names(give_vals)) {
    sum(give_vals$surplus_value, na.rm = TRUE)
  } else {
    0
  }

  surplus_receive <- if (!is.null(receive_vals) && nrow(receive_vals) > 0 &&
                         "surplus_value" %in% names(receive_vals)) {
    sum(receive_vals$surplus_value, na.rm = TRUE)
  } else {
    0
  }

  net_surplus_diff <- abs(surplus_receive - surplus_give)

  # Lopsided if net difference > 20% of total trade value
  net_surplus_diff > 0.20 * total_trade_value
}


#' Build structured justification for trade recommendation
#'
#' Generates a detailed text explanation covering all trade dimensions.
#'
#' @param value_diff Net surplus value difference
#' @param pts_change Points change list (total, batting, pitching)
#' @param salary_impact Salary impact list
#' @param keeper_analysis Keeper implications list
#' @param positional_impact Positional impact list
#' @param context Competitive context list
#' @param lopsided Logical: is the trade lopsided
#' @param recommendation Character: accept/reject/counter
#' @return Character: multi-line justification text
#' @keywords internal
build_trade_justification <- function(value_diff, pts_change, salary_impact,
                                      keeper_analysis, positional_impact,
                                      context, lopsided, recommendation) {
  reasons <- character(0)

  # Value assessment
  if (value_diff > 0) {
    reasons <- c(reasons, sprintf("Net surplus value favors you by +%.1f", value_diff))
  } else if (value_diff < 0) {
    reasons <- c(reasons, sprintf("Net surplus value is against you by %.1f", value_diff))
  } else {
    reasons <- c(reasons, "Net surplus value is even")
  }

  # Points impact
  if (pts_change$total > 0) {
    reasons <- c(reasons, sprintf("Projected +%.1f pts/wk improvement", pts_change$total))
  } else if (pts_change$total < 0) {
    reasons <- c(reasons, sprintf("Projected %.1f pts/wk decline", pts_change$total))
  }

  # Salary impact
  if (!salary_impact$cap_compliant) {
    reasons <- c(reasons, "CRITICAL: Trade violates the $300 salary cap")
  } else {
    reasons <- c(reasons, sprintf(
      "Salary cap compliant (new total: $%.0f/$300)", salary_impact$new_total
    ))
  }

  # Keeper value comparison
  give_keeper_val <- sum(vapply(keeper_analysis$give, function(x) {
    if (is.null(x$keeper_value_3yr)) 0 else x$keeper_value_3yr
  }, numeric(1)))
  receive_keeper_val <- sum(vapply(keeper_analysis$receive, function(x) {
    if (is.null(x$keeper_value_3yr)) 0 else x$keeper_value_3yr
  }, numeric(1)))
  keeper_diff <- receive_keeper_val - give_keeper_val
  if (abs(keeper_diff) > 1) {
    direction <- if (keeper_diff > 0) "gaining" else "losing"
    reasons <- c(reasons, sprintf(
      "3-year keeper value: %s %.1f in keeper NPV", direction, abs(keeper_diff)
    ))
  }

  # Positional impact
  if (length(positional_impact$unfilled) > 0) {
    reasons <- c(reasons, sprintf(
      "WARNING: Positions left empty: %s",
      paste(positional_impact$unfilled, collapse = ", ")
    ))
  }
  if (length(positional_impact$improved) > 0) {
    reasons <- c(reasons, sprintf(
      "Positions improved: %s", paste(positional_impact$improved, collapse = ", ")
    ))
  }

  # Lopsided warning
  if (lopsided) {
    reasons <- c(reasons, "ALERT: Trade appears lopsided (>20% value difference)")
  }

  # Strategic fit
  reasons <- c(reasons, sprintf(
    "Strategic fit: team is in %s mode (%.0f%% playoff probability)",
    context$mode, context$playoff_prob * 100
  ))

  paste(reasons, collapse = "\n")
}

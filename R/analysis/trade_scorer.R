#' Trade Scorer — Composite scoring for trade comparisons
#'
#' Provides a 0-100 scoring system for comparing multiple trade proposals
#' side by side. Uses three weighted components: Immediate Impact (pts/wk),
#' Value & Keeper (surplus + NPV), and Strategic Fit (cap, prospects, position).
#'
#' Designed to be used after analyze_trade() to produce a single comparable
#' number across different trade options.

# --- Positional Awareness ---

#' Roster slot configuration for the league
#' Each position maps to the number of starting slots available.
ROSTER_SLOTS <- list(

  C  = 1,
  "1B" = 1,
  "2B" = 1,
  "3B" = 1,
  SS = 1,
  OF = 3,
  U  = 1,
  SP = 5,
  RP = 2
)

#' Parse a player's eligible positions from the CSV format
#' @param elig_str Character like "2B,3B,SS,OF" or NA
#' @return Character vector of positions
parse_eligible_positions <- function(elig_str) {
  if (is.na(elig_str) || nchar(trimws(elig_str)) == 0) return(character(0))
  trimws(strsplit(elig_str, ",")[[1]])
}

#' Compute positional fit score for a trade
#'
#' Evaluates how well received players fill roster needs and how much
#' giving away players hurts positional depth. Uses the team's current
#' active roster to determine strength at each position.
#'
#' @param my_roster Data frame: full roster for my team (from rosters.rds)
#' @param give_names Character vector: player names being traded away
#' @param receive_names Character vector: player names being received
#' @param all_rosters Data frame: full league rosters (for looking up received players' positions)
#' @param valuations Data frame: player valuations (for pts/wk comparisons). Optional.
#' @return List with:
#'   \describe{
#'     \item{score}{Numeric: net positional fit score (-25 to +25)}
#'     \item{details}{Character: human-readable explanation}
#'     \item{receive_bonus}{Numeric: bonus from filling needs}
#'     \item{give_penalty}{Numeric: penalty from weakening positions}
#'   }
#' @export
compute_positional_fit <- function(my_roster, give_names, receive_names,
                                   all_rosters, valuations = NULL) {

  # Only consider active + reserve players for positional strength
  active_roster <- my_roster[my_roster$roster_section %in% c("Active", "Reserves"), ]

  # Build position strength map: for each position, what's the best player's FPTS?
  # This tells us which positions are strong vs weak
  pos_strength <- list()
  pos_players <- list()

  for (i in seq_len(nrow(active_roster))) {
    p <- active_roster[i, ]
    positions <- parse_eligible_positions(p$eligible_positions)
    # Also include the slot they're currently rostered at (this is where they ACTUALLY play)
    roster_pos <- p$roster_position
    if (!roster_pos %in% positions) {
      positions <- c(positions, roster_pos)
    }
    fpts <- if (is.na(p$total_fpts)) 0 else p$total_fpts
    for (pos in positions) {
      if (is.null(pos_strength[[pos]])) {
        pos_strength[[pos]] <- numeric(0)
        pos_players[[pos]] <- character(0)
      }
      pos_strength[[pos]] <- c(pos_strength[[pos]], fpts)
      pos_players[[pos]] <- c(pos_players[[pos]], p$player_name)
    }
  }

  # Sort each position's players by production (descending)
  for (pos in names(pos_strength)) {
    ord <- order(pos_strength[[pos]], decreasing = TRUE)
    pos_strength[[pos]] <- pos_strength[[pos]][ord]
    pos_players[[pos]] <- pos_players[[pos]][ord]
  }

  # --- Compute league-average production per position for context ---
  # Use a simple threshold: if starter FPTS is below league median for that

  # position, it's a "weak" spot. Above 75th percentile = "strong".
  # For simplicity, use total_fpts across all active starters in the league.
  all_active <- all_rosters[all_rosters$roster_section == "Active", ]
  league_pos_medians <- list()
  for (pos in names(ROSTER_SLOTS)) {
    pos_fpts <- all_active$total_fpts[all_active$roster_position == pos]
    pos_fpts <- pos_fpts[!is.na(pos_fpts)]
    league_pos_medians[[pos]] <- if (length(pos_fpts) > 0) median(pos_fpts) else 100
  }

  # --- Score received players ---
  receive_bonus <- 0
  receive_details <- character(0)

  for (rname in receive_names) {
    # Look up received player's eligible positions from any roster
    rrow <- all_rosters[tolower(all_rosters$player_name) == tolower(rname), , drop = FALSE]
    if (nrow(rrow) == 0) next
    rrow <- rrow[1, ]
    r_positions <- parse_eligible_positions(rrow$eligible_positions)
    # Include U eligibility for all hitters
    if (rrow$player_type == "Batter" && !"U" %in% r_positions) {
      r_positions <- c(r_positions, "U")
    }
    if (length(r_positions) == 0) next

    # Find the position where this player provides the most upgrade
    # Prefer actual positions over U (utility is a fallback slot)
    best_pos <- NULL
    best_value <- -Inf

    for (pos in r_positions) {
      n_slots <- if (!is.null(ROSTER_SLOTS[[pos]])) ROSTER_SLOTS[[pos]] else 1
      current_fpts <- if (!is.null(pos_strength[[pos]])) pos_strength[[pos]] else numeric(0)

      # What's the weakest starter at this position?
      # (the Nth best player where N = number of slots)
      weakest_starter_fpts <- if (length(current_fpts) >= n_slots) {
        current_fpts[n_slots]
      } else {
        0  # Empty slot
      }

      r_fpts <- if (is.na(rrow$total_fpts)) 0 else rrow$total_fpts
      upgrade <- r_fpts - weakest_starter_fpts
      median_at_pos <- if (!is.null(league_pos_medians[[pos]])) league_pos_medians[[pos]] else 100

      # Score: how much does this player upgrade the weakest spot?
      # Filling an empty slot or replacing a below-median starter = big bonus
      # Replacing an above-median starter = small or no bonus (redundant)
      if (length(current_fpts) < n_slots) {
        # Empty slot — big bonus
        value <- 15
      } else if (weakest_starter_fpts < median_at_pos * 0.7) {
        # Weak spot (below 70% of league median) — good bonus if upgrade
        value <- if (upgrade > 0) min(15, upgrade / 20) else -5
      } else if (weakest_starter_fpts < median_at_pos) {
        # Below average — moderate bonus if upgrade
        value <- if (upgrade > 0) min(10, upgrade / 30) else -5
      } else {
        # Strong position — redundant, penalty
        value <- -5
      }

      # Penalize U as a fallback — only count it if no better position fits
      if (pos == "U") {
        value <- value - 8
      }

      if (value > best_value) {
        best_value <- value
        best_pos <- pos
      }
    }

    if (!is.null(best_pos)) {
      receive_bonus <- receive_bonus + best_value
      tag <- if (best_value > 5) "fills need"
             else if (best_value > 0) "mild upgrade"
             else "redundant"
      receive_details <- c(receive_details,
        sprintf("%s → %s (%s, %+.0f)", rname, best_pos, tag, best_value))
    }
  }

  # --- Score given players (penalty for weakening positions) ---
  give_penalty <- 0
  give_details <- character(0)

  for (gname in give_names) {
    grow <- my_roster[tolower(my_roster$player_name) == tolower(gname), , drop = FALSE]
    if (nrow(grow) == 0) next
    grow <- grow[1, ]

    # Only penalize if this player is an active starter
    if (grow$roster_section != "Active") next

    pos <- grow$roster_position
    n_slots <- if (!is.null(ROSTER_SLOTS[[pos]])) ROSTER_SLOTS[[pos]] else 1
    current_fpts <- if (!is.null(pos_strength[[pos]])) pos_strength[[pos]] else numeric(0)

    g_fpts <- if (is.na(grow$total_fpts)) 0 else grow$total_fpts

    # How many players do we have at this position (including reserves who can fill in)?
    depth <- length(current_fpts)

    if (depth <= n_slots) {
      # We're already at minimum depth — losing this player leaves a hole
      give_penalty <- give_penalty - 10
      give_details <- c(give_details,
        sprintf("%s leaves %s thin (-10)", gname, pos))
    } else if (g_fpts > current_fpts[min(depth, n_slots + 1)]) {
      # We have depth but this player is clearly our best — mild penalty
      give_penalty <- give_penalty - 3
      give_details <- c(give_details,
        sprintf("%s is top %s but have depth (-3)", gname, pos))
    }
    # If we have plenty of depth and this player isn't the best, no penalty
  }

  # --- Net score ---
  net_score <- receive_bonus + give_penalty
  # Clamp to [-25, 25]
  net_score <- max(-25, min(25, net_score))

  details_text <- paste(c(
    if (length(receive_details) > 0) paste("  Receive:", receive_details) else NULL,
    if (length(give_details) > 0) paste("  Give:", give_details) else NULL
  ), collapse = "\n")

  list(
    score = round(net_score, 1),
    details = details_text,
    receive_bonus = round(receive_bonus, 1),
    give_penalty = round(give_penalty, 1)
  )
}

# --- Constants ---

#' Baseline pts/wk for scoring normalization (JRam-level production)
BASELINE_PTS_PER_WEEK <- 17.28

#' Keeper NPV benchmark for comparison (typical star player)
BASELINE_KEEPER_3YR <- 47.27

#' In-season salary cap
SCORER_SALARY_CAP <- 300

# --- Core Scoring Function ---

#' Score a trade on a 0-100 scale
#'
#' Computes a composite trade score using three weighted components. Designed
#' for comparing multiple trades involving the same core player(s).
#'
#' @param pts_wk_change Numeric: net change in projected pts/wk
#'   (receive side total minus give side total)
#' @param surplus_change Numeric: net surplus value change
#'   (receive surplus minus give surplus)
#' @param keeper_3yr_receive Numeric: total 3yr keeper NPV of received pieces
#'   (only include keepable pieces; exclude rental players)
#' @param cap_freed Numeric: salary freed (give salary minus receive salary).
#'   Negative means you're taking on more salary.
#' @param prospect_bonus Numeric (0-35): bonus points for prospect upside.
#'   Guidelines: elite top-10 prospect = 25-30, top-25 = 20, top-50 = 15,
#'   top-100 = 10, minor contract bonus = +5.
#' @param positional_fit Numeric (0-25): bonus for filling roster holes.
#'   Guidelines: fills a starter hole = 15-20, adds depth = 5-10.
#' @param strategic_penalty Numeric (0-30): penalty for bad league dynamics.
#'   Guidelines: helping league leader = 20-30, helping division rival = 10-15.
#' @param sp_ace_bonus Numeric (0-15): adjustment for top-5 system SP.
#'   Compensates for structural pitcher pts/wk undervaluation.
#' @param injury_penalty Numeric (0-15): penalty for receiving injured players.
#'   Guidelines: 10-day IL = 5, 60-day IL = 10-15, flexor/UCL concern = 5-10.
#' @param context_mode Character: "contend", "middle", or "rebuild".
#'   Adjusts component weights. Default "middle".
#' @return List with:
#'   \describe{
#'     \item{total}{Numeric: composite score 0-100}
#'     \item{grade}{Character: letter grade (A, B+, B, B-, C+, C, D, F)}
#'     \item{immediate}{Numeric: immediate impact component score 0-100}
#'     \item{value}{Numeric: value & keeper component score 0-100}
#'     \item{strategic}{Numeric: strategic fit component score 0-100}
#'     \item{components}{List: detailed breakdown of each sub-score}
#'   }
#' @export
score_trade <- function(pts_wk_change,
                        surplus_change,
                        keeper_3yr_receive,
                        cap_freed,
                        prospect_bonus = 0,
                        positional_fit = 0,
                        strategic_penalty = 0,
                        sp_ace_bonus = 0,
                        injury_penalty = 0,
                        context_mode = "middle") {

  # --- Input validation ---
  stopifnot(is.numeric(pts_wk_change), length(pts_wk_change) == 1)
  stopifnot(is.numeric(surplus_change), length(surplus_change) == 1)
  stopifnot(is.numeric(keeper_3yr_receive), length(keeper_3yr_receive) == 1)
  stopifnot(is.numeric(cap_freed), length(cap_freed) == 1)
  stopifnot(context_mode %in% c("contend", "middle", "rebuild"))

  # --- Component weights by competitive mode ---
  weights <- switch(context_mode,
    "contend" = c(immediate = 0.40, value = 0.40, strategic = 0.20),
    "middle"  = c(immediate = 0.30, value = 0.50, strategic = 0.20),
    "rebuild" = c(immediate = 0.15, value = 0.65, strategic = 0.20)
  )

  # --- Component 1: Immediate Impact (0-100) ---
  # 0 pts/wk change = 50, +baseline (full replacement) = 100, -baseline = 0
  immediate_raw <- 50 + (pts_wk_change / BASELINE_PTS_PER_WEEK) * 50
  # Apply SP ace bonus (compensates for pitcher pts/wk undervaluation)
  immediate_score <- max(0, min(100, immediate_raw + sp_ace_bonus))
  # Apply injury penalty
  immediate_score <- max(0, immediate_score - injury_penalty)

  # --- Component 2: Value & Keeper (0-100) ---
  # Surplus sub-score: +$40 = 100, 0 = 50, -$40 = 0
  surplus_score <- 50 + (surplus_change / 40) * 50
  surplus_score <- max(0, min(100, surplus_score))

  # Keeper sub-score: compare receive-side NPV to baseline
  keeper_diff <- keeper_3yr_receive - BASELINE_KEEPER_3YR
  keeper_score <- 50 + (keeper_diff / 100) * 50
  keeper_score <- max(0, min(100, keeper_score))

  # Combine: 40% surplus, 60% keeper
  value_score <- (surplus_score * 0.4) + (keeper_score * 0.6)

  # --- Component 3: Strategic Fit (0-100) ---
  # Cap space contribution (max 30 pts from freeing full salary)
  cap_score <- (max(0, cap_freed) / 37) * 30

  # Sum all strategic factors
  strategic_score <- cap_score + prospect_bonus + positional_fit - strategic_penalty
  strategic_score <- max(0, min(100, strategic_score))

  # --- Weighted Total ---
  total <- (immediate_score * weights["immediate"]) +
           (value_score * weights["value"]) +
           (strategic_score * weights["strategic"])

  # --- Assign Grade ---
  grade <- if (total >= 80) "A"
           else if (total >= 65) "B+"
           else if (total >= 56) "B"
           else if (total >= 53) "B-"
           else if (total >= 48) "C+"
           else if (total >= 40) "C"
           else if (total >= 10) "D"
           else "F"

  list(
    total      = round(total, 1),
    grade      = grade,
    immediate  = round(immediate_score, 1),
    value      = round(value_score, 1),
    strategic  = round(strategic_score, 1),
    components = list(
      pts_wk_change      = pts_wk_change,
      surplus_change     = surplus_change,
      keeper_3yr_receive = keeper_3yr_receive,
      cap_freed          = cap_freed,
      prospect_bonus     = prospect_bonus,
      positional_fit     = positional_fit,
      strategic_penalty  = strategic_penalty,
      sp_ace_bonus       = sp_ace_bonus,
      injury_penalty     = injury_penalty,
      context_mode       = context_mode,
      weights            = weights
    )
  )
}


#' Format a trade score for display
#'
#' @param score List: output from score_trade()
#' @param trade_name Character: descriptive name for the trade
#' @return Character: formatted multi-line string
#' @export
format_trade_score <- function(score, trade_name = "Trade") {
  sprintf(
    paste0(
      "=== %s ===\n",
      "  Immediate Impact (%d%%): %.1f/100  [pts/wk: %+.1f]\n",
      "  Value & Keeper   (%d%%): %.1f/100  [surplus: %+.1f | 3yr NPV: $%.1f]\n",
      "  Strategic Fit    (%d%%): %.1f/100  [cap: $%d freed]\n",
      "  TOTAL SCORE: %.1f / 100  [Grade: %s]"
    ),
    trade_name,
    round(score$components$weights["immediate"] * 100),
    score$immediate,
    score$components$pts_wk_change,
    round(score$components$weights["value"] * 100),
    score$value,
    score$components$surplus_change,
    score$components$keeper_3yr_receive,
    round(score$components$weights["strategic"] * 100),
    score$strategic,
    round(score$components$cap_freed),
    score$total,
    score$grade
  )
}

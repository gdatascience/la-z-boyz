#' Trade Scorer — Composite scoring for trade comparisons
#'
#' Provides a 0-100 scoring system for comparing multiple trade proposals
#' side by side. Uses three weighted components: Immediate Impact (pts/wk),
#' Value & Keeper (surplus + NPV), and Strategic Fit (cap, prospects, position).
#'
#' Designed to be used after analyze_trade() to produce a single comparable
#' number across different trade options.

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

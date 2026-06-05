#' Evaluate Trade Offer — Reusable trade evaluation script
#'
#' General-purpose script for evaluating a trade proposal. Loads all data,
#' looks up player valuations, applies manual overrides (prospect rankings,
#' injury penalties, real-world NPV adjustments), scores the trade, and
#' outputs a formatted summary.
#'
#' Usage:
#'   source("R/analysis/evaluate_trade_offer.R")
#'   result <- evaluate_offer(
#'     give = c("Jose Ramirez"),
#'     receive = c("Gunnar Henderson", "Braxton Ashcraft"),
#'     overrides = list(
#'       "Gunnar Henderson" = list(real_3yr_npv = 80),
#'       "Braxton Ashcraft" = list(real_pts_wk = 7.5)
#'     ),
#'     prospect_bonus = 10,
#'     positional_fit = 20
#'   )

# Source dependencies
if (!exists("score_trade")) {
  library(here)
  source(here("R/utils/keeper_value.R"))
  source(here("R/utils/salary_rules.R"))
  source(here("R/utils/player_linker.R"))
  source(here("R/analysis/trade_scorer.R"))
  source(here("R/ingest/fetch_prospect_rankings.R"))
}

# --- Data Loading (cached, loads once per session) ---

.trade_eval_env <- new.env(parent = emptyenv())

load_trade_data <- function(force = FALSE) {
  if (!force && exists("valuations", envir = .trade_eval_env)) {
    return(invisible(NULL))
  }
  .trade_eval_env$valuations <- readRDS(here::here("data/cache/valuations.rds"))
  .trade_eval_env$rosters <- readRDS(here::here("data/cache/rosters.rds"))
  .trade_eval_env$standings <- readRDS(here::here("data/cache/standings.rds"))
  .trade_eval_env$rankings <- load_prospect_rankings()
  message("Trade evaluation data loaded.")
}


# --- Player Lookup ---

#' Look up a player's value from cached data
#'
#' Returns salary, pts/wk, surplus, keeper NPV, and prospect tier for a player.
#' Handles players not in valuations (prospects with no MLB stats) by returning
#' zeroes for model values and checking prospect rankings.
#'
#' @param player_name Character: player name
#' @param overrides Optional list with manual overrides (real_pts_wk, real_3yr_npv, etc.)
#' @return List with all relevant player data
lookup_player <- function(player_name, overrides = NULL) {
  load_trade_data()

  valuations <- .trade_eval_env$valuations
  rosters <- .trade_eval_env$rosters
  rankings <- .trade_eval_env$rankings

  # Roster lookup
  roster_row <- rosters[tolower(rosters$player_name) == tolower(player_name), , drop = FALSE]
  salary <- if (nrow(roster_row) > 0) roster_row$salary[1] else 0
  team_name <- if (nrow(roster_row) > 0) roster_row$team_name[1] else NA
  is_minor <- if (nrow(roster_row) > 0) isTRUE(roster_row$is_minor_contract[1]) else FALSE

  # Valuation lookup
  val_row <- valuations[tolower(valuations$player_name) == tolower(player_name), , drop = FALSE]
  if (nrow(val_row) > 0) {
    pts_wk <- val_row$proj_pts_per_week[1]
    dollar_value <- val_row$dollar_value[1]
    surplus <- val_row$surplus_value[1]
    keeper_3yr <- val_row$keeper_value_3yr[1]
    keeper_5yr <- val_row$keeper_value_5yr[1]
  } else {
    pts_wk <- 0
    dollar_value <- 0
    surplus <- 0
    keeper_3yr <- 0
    keeper_5yr <- 0
  }

  # Prospect ranking lookup
  prospect <- lookup_prospect(player_name, rankings)
  prospect_tier <- if (!is.null(prospect)) prospect$tier else NA
  prospect_rank <- if (!is.null(prospect)) prospect$best_rank else NA

  # Apply overrides
  if (!is.null(overrides)) {
    if (!is.null(overrides$real_pts_wk)) pts_wk <- overrides$real_pts_wk
    if (!is.null(overrides$real_3yr_npv)) keeper_3yr <- overrides$real_3yr_npv
    if (!is.null(overrides$real_5yr_npv)) keeper_5yr <- overrides$real_5yr_npv
    if (!is.null(overrides$real_surplus)) surplus <- overrides$real_surplus
  }

  list(
    player_name   = player_name,
    team_name     = team_name,
    salary        = salary,
    is_minor      = is_minor,
    pts_wk        = pts_wk,
    dollar_value  = dollar_value,
    surplus       = surplus,
    keeper_3yr    = keeper_3yr,
    keeper_5yr    = keeper_5yr,
    prospect_tier = prospect_tier,
    prospect_rank = prospect_rank,
    has_valuation = nrow(val_row) > 0
  )
}


# --- Core Evaluation Function ---

#' Evaluate a trade offer
#'
#' Loads player data, applies overrides, computes the trade score, and returns
#' a structured result with formatted output.
#'
#' @param give Character vector: player names you are trading away
#' @param receive Character vector: player names you are receiving
#' @param my_team Character: your team name (default: "Fightin' Irish")
#' @param overrides Named list of per-player overrides. Each entry is a list
#'   with optional keys: real_pts_wk, real_3yr_npv, real_5yr_npv, real_surplus.
#'   Example: list("Gunnar Henderson" = list(real_3yr_npv = 80))
#' @param prospect_bonus Numeric: prospect upside bonus for scoring (0-35)
#' @param positional_fit Numeric: positional fit bonus (0-25)
#' @param strategic_penalty Numeric: league dynamics penalty (0-30)
#' @param sp_ace_bonus Numeric: SP undervaluation adjustment (0-15)
#' @param injury_penalty Numeric: injury penalty (0-15)
#' @param rental_players Character vector: players in receive to treat as 1yr rentals
#' @param context_mode Character: "contend", "middle", or "rebuild" (default: auto-detect)
#' @return List with: score, give_details, receive_details, summary_text
#' @export
evaluate_offer <- function(give, receive, my_team = "Fightin' Irish",
                           overrides = list(),
                           prospect_bonus = 0,
                           positional_fit = 0,
                           strategic_penalty = 0,
                           sp_ace_bonus = 0,
                           injury_penalty = 0,
                           rental_players = character(0),
                           context_mode = NULL) {
  load_trade_data()

  # Auto-detect context mode from standings if not provided
  if (is.null(context_mode)) {
    standings <- .trade_eval_env$standings
    team_row <- standings[tolower(standings$team_name) == tolower(my_team), , drop = FALSE]
    if (nrow(team_row) > 0) {
      pct <- as.numeric(team_row$pct[1])
      gb <- as.numeric(team_row$games_back[1])
      if (!is.na(pct) && pct >= 0.6 && !is.na(gb) && gb <= 2) {
        context_mode <- "contend"
      } else if (!is.na(pct) && pct <= 0.35) {
        context_mode <- "rebuild"
      } else {
        context_mode <- "middle"
      }
    } else {
      context_mode <- "middle"
    }
  }

  # Look up all players
  give_details <- lapply(give, function(p) {
    lookup_player(p, overrides[[p]])
  })
  names(give_details) <- give

  receive_details <- lapply(receive, function(p) {
    lookup_player(p, overrides[[p]])
  })
  names(receive_details) <- receive

  # Compute aggregates
  give_pts <- sum(vapply(give_details, function(x) x$pts_wk, numeric(1)))
  give_surplus <- sum(vapply(give_details, function(x) x$surplus, numeric(1)))
  give_salary <- sum(vapply(give_details, function(x) x$salary, numeric(1)))

  receive_pts <- sum(vapply(receive_details, function(x) x$pts_wk, numeric(1)))
  receive_surplus <- sum(vapply(receive_details, function(x) x$surplus, numeric(1)))
  receive_salary <- sum(vapply(receive_details, function(x) x$salary, numeric(1)))

  # For keeper NPV: exclude rental players
  keepable_receive <- receive_details[!tolower(names(receive_details)) %in% tolower(rental_players)]
  keeper_3yr_receive <- sum(vapply(keepable_receive, function(x) x$keeper_3yr, numeric(1)))

  # Score
  pts_wk_change <- receive_pts - give_pts
  surplus_change <- receive_surplus - give_surplus
  cap_freed <- give_salary - receive_salary

  trade_score <- score_trade(
    pts_wk_change      = pts_wk_change,
    surplus_change     = surplus_change,
    keeper_3yr_receive = keeper_3yr_receive,
    cap_freed          = cap_freed,
    prospect_bonus     = prospect_bonus,
    positional_fit     = positional_fit,
    strategic_penalty  = strategic_penalty,
    sp_ace_bonus       = sp_ace_bonus,
    injury_penalty     = injury_penalty,
    context_mode       = context_mode
  )

  # Build summary text
  give_text <- paste(vapply(give_details, function(x) {
    sprintf("%s ($%d, %.1f pts/wk)", x$player_name, x$salary, x$pts_wk)
  }, character(1)), collapse = " + ")

  receive_text <- paste(vapply(receive_details, function(x) {
    rental_tag <- if (tolower(x$player_name) %in% tolower(rental_players)) " [RENTAL]" else ""
    sprintf("%s ($%d, %.1f pts/wk)%s", x$player_name, x$salary, x$pts_wk, rental_tag)
  }, character(1)), collapse = " + ")

  summary <- sprintf(paste0(
    "=== TRADE EVALUATION ===\n",
    "Give:    %s\n",
    "Receive: %s\n",
    "\n",
    "Pts/wk change: %+.1f | Surplus change: %+.1f\n",
    "Keeper 3yr NPV (keepable): $%.1f | Cap freed: $%d\n",
    "Context: %s mode\n",
    "\n",
    "%s"
  ),
    give_text, receive_text,
    pts_wk_change, surplus_change,
    keeper_3yr_receive, round(cap_freed),
    context_mode,
    format_trade_score(trade_score, paste(receive, collapse = " + "))
  )

  list(
    score          = trade_score,
    give_details   = give_details,
    receive_details = receive_details,
    pts_wk_change  = pts_wk_change,
    surplus_change = surplus_change,
    keeper_3yr     = keeper_3yr_receive,
    cap_freed      = cap_freed,
    context_mode   = context_mode,
    summary        = summary
  )
}


#' Print a trade evaluation result
#' @param result Output from evaluate_offer()
#' @export
print_evaluation <- function(result) {
  cat(result$summary, "\n")
}

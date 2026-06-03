#' Scoring utility functions for H2H Points computation
#'
#' Pure functions for computing fantasy points from stat lines using
#' the La-Z-Boyz of Summer league scoring weights.

#' Load default scoring weights from the league constitution
#'
#' @return List with $batting and $pitching weight sub-lists
#' @keywords internal
load_default_weights <- function() {
  constitution_path <- file.path("data", "league_constitution.rds")
  if (!file.exists(constitution_path)) {
    stop(
      "League constitution not found at '", constitution_path, "'. ",
      "Run parse_constitution.R first or ensure the file exists.",
      call. = FALSE
    )
  }
  constitution <- readRDS(constitution_path)
  if (is.null(constitution$scoring)) {
    stop("Constitution is missing the 'scoring' section.", call. = FALSE)
  }
  constitution$scoring
}

#' Compute batting fantasy points from a stat line
#'
#' Applies league H2H Points scoring weights to a batting stat line.
#' Stats can be a named list or 1-row data.frame. Any stat not present
#' in the input is treated as zero.
#'
#' @param stats Named list or 1-row data.frame with batting stats.
#'   Expected names (mapping to constitution weight keys):
#'   - singles (or X1B) -> singles weight
#'   - doubles (or X2B) -> doubles weight
#'   - triples (or X3B) -> triples weight
#'   - hr (or HR) -> hr weight
#'   - grand_slam_bonus (or grand_slams) -> grand_slam_bonus weight
#'   - cycle -> cycle weight
#'   - runs (or R) -> runs weight
#'   - rbi (or RBI) -> rbi weight
#'   - bb (or BB) -> bb weight
#'   - hbp (or HBP) -> hbp weight
#'   - sb (or SB) -> sb weight
#'   - cs (or CS) -> cs weight
#'   - k_batter (or K, SO) -> k_batter weight
#' @param weights Named list of batting scoring weights. If NULL, loads
#'   defaults from league_constitution.rds.
#' @return Numeric: total fantasy points
#' @export
compute_batting_points <- function(stats, weights = NULL) {
  if (is.null(weights)) {
    weights <- load_default_weights()$batting
  }


  # Convert 1-row data.frame to named list
  if (is.data.frame(stats)) {
    if (nrow(stats) != 1) {
      stop("stats data.frame must have exactly 1 row.", call. = FALSE)
    }
    stats <- as.list(stats[1, , drop = FALSE])
  }

  # Mapping from stat input names (with aliases) to weight keys
  stat_to_weight <- list(
    singles      = "singles",
    X1B          = "singles",
    doubles      = "doubles",
    X2B          = "doubles",
    triples      = "triples",
    X3B          = "triples",
    hr           = "hr",
    HR           = "hr",
    grand_slam_bonus = "grand_slam_bonus",
    grand_slams  = "grand_slam_bonus",
    cycle        = "cycle",
    runs         = "runs",
    R            = "runs",
    rbi          = "rbi",
    RBI          = "rbi",
    bb           = "bb",
    BB           = "bb",
    hbp          = "hbp",
    HBP          = "hbp",
    sb           = "sb",
    SB           = "sb",
    cs           = "cs",
    CS           = "cs",
    k_batter     = "k_batter",
    K            = "k_batter",
    SO           = "k_batter"
  )

  total <- 0
  for (stat_name in names(stats)) {
    weight_key <- stat_to_weight[[stat_name]]
    if (!is.null(weight_key) && !is.null(weights[[weight_key]])) {
      val <- as.numeric(stats[[stat_name]])
      if (!is.na(val)) {
        total <- total + val * weights[[weight_key]]
      }
    }
  }

  total
}

#' Compute pitching fantasy points from a stat line
#'
#' Applies league H2H Points scoring weights to a pitching stat line.
#' Stats can be a named list or 1-row data.frame. Any stat not present
#' in the input is treated as zero.
#'
#' @param stats Named list or 1-row data.frame with pitching stats.
#'   Expected names (mapping to constitution weight keys):
#'   - innings (or IP) -> innings weight
#'   - k_pitcher (or K, SO) -> k_pitcher weight
#'   - wins (or W) -> wins weight
#'   - losses (or L) -> losses weight
#'   - saves (or SV) -> saves weight
#'   - holds (or HLD) -> holds weight
#'   - quality_starts (or QS) -> quality_starts weight
#'   - complete_games (or CG) -> complete_games weight
#'   - no_hitter (or NH) -> no_hitter weight
#'   - perfect_game (or PG) -> perfect_game weight
#'   - hits_allowed (or H) -> hits_allowed weight
#'   - earned_runs (or ER) -> earned_runs weight
#'   - walks_issued (or BB) -> walks_issued weight
#'   - intentional_walks (or IBB) -> intentional_walks weight
#' @param weights Named list of pitching scoring weights. If NULL, loads
#'   defaults from league_constitution.rds.
#' @return Numeric: total fantasy points
#' @export
compute_pitching_points <- function(stats, weights = NULL) {
  if (is.null(weights)) {
    weights <- load_default_weights()$pitching
  }

  # Convert 1-row data.frame to named list
  if (is.data.frame(stats)) {
    if (nrow(stats) != 1) {
      stop("stats data.frame must have exactly 1 row.", call. = FALSE)
    }
    stats <- as.list(stats[1, , drop = FALSE])
  }

  # Mapping from stat input names (with aliases) to weight keys
  stat_to_weight <- list(
    innings          = "innings",
    IP               = "innings",
    k_pitcher        = "k_pitcher",
    K                = "k_pitcher",
    SO               = "k_pitcher",
    wins             = "wins",
    W                = "wins",
    losses           = "losses",
    L                = "losses",
    saves            = "saves",
    SV               = "saves",
    holds            = "holds",
    HLD              = "holds",
    quality_starts   = "quality_starts",
    QS               = "quality_starts",
    complete_games   = "complete_games",
    CG               = "complete_games",
    no_hitter        = "no_hitter",
    NH               = "no_hitter",
    perfect_game     = "perfect_game",
    PG               = "perfect_game",
    hits_allowed     = "hits_allowed",
    H                = "hits_allowed",
    earned_runs      = "earned_runs",
    ER               = "earned_runs",
    walks_issued     = "walks_issued",
    BB               = "walks_issued",
    intentional_walks = "intentional_walks",
    IBB              = "intentional_walks"
  )

  total <- 0
  for (stat_name in names(stats)) {
    weight_key <- stat_to_weight[[stat_name]]
    if (!is.null(weight_key) && !is.null(weights[[weight_key]])) {
      val <- as.numeric(stats[[stat_name]])
      if (!is.na(val)) {
        total <- total + val * weights[[weight_key]]
      }
    }
  }

  total
}

#' Convert total points to projected points per week
#'
#' @param total_points Total fantasy points for the season (numeric)
#' @param games_played Games played so far (numeric)
#' @param games_per_week Average games per week (default 6.5 for batters)
#' @return Numeric: projected points per week, or NA if games_played is 0
#' @export
points_per_week <- function(total_points, games_played, games_per_week = 6.5) {
  if (is.na(games_played) || games_played == 0) {
    warning("games_played is 0 or NA; cannot compute points per week.")
    return(NA_real_)
  }
  if (is.na(total_points)) {
    return(NA_real_)
  }
  pts_per_game <- total_points / games_played
  pts_per_game * games_per_week
}

#' Projection Model — Core Projection Engine
#'
#' Implements a Modified Marcel projection system for rest-of-season projections.
#' Uses historical weighting (5/4/3 for last 3 seasons), regression to league
#' averages, and position-specific age curves.
#'
#' Requirements: 4.1, 4.2, 4.3, 4.4, 4.9

# --- Configuration ---

#' Batting counting stats projected by the model
#' (those used in H2H Points scoring)
BATTING_COUNTING_STATS <- c(

  "X1B", "X2B", "X3B", "HR", "R", "RBI", "BB", "HBP", "SB", "CS", "SO"
)

#' Batting rate stats preserved for context
BATTING_RATE_STATS <- c("BA", "OBP", "SLG", "OPS")

#' Batting playing-time stats used for regression
BATTING_PT_STATS <- c("G", "PA", "AB")

#' Pitching counting stats projected by the model
PITCHING_COUNTING_STATS <- c(

  "IP", "SO", "W", "L", "SV", "H", "ER", "BB", "HR", "HBP"
)

#' Pitching rate stats preserved for context
PITCHING_RATE_STATS <- c("ERA", "WHIP")

#' Pitching playing-time stats used for regression
PITCHING_PT_STATS <- c("G", "GS", "BF")

#' League average rates per plate appearance (batting)
#' Used for regression toward the mean
LEAGUE_AVG_BATTING_PER_PA <- list(

  X1B = 0.150, X2B = 0.045, X3B = 0.005, HR = 0.030,

  R = 0.070, RBI = 0.065, BB = 0.085, HBP = 0.012,
  SB = 0.015, CS = 0.005, SO = 0.220
)

#' League average rates per batter faced (pitching)
LEAGUE_AVG_PITCHING_PER_BF <- list(
  SO = 0.220, H = 0.230, ER = 0.060, BB = 0.080, HR = 0.030, HBP = 0.010
)

#' League average counting rates per game started (pitching decisions)
LEAGUE_AVG_PITCHING_PER_GS <- list(
  W = 0.30, L = 0.25, SV = 0.00, IP = 5.5
)

#' League average rates per game for relievers
LEAGUE_AVG_PITCHING_PER_G_RP <- list(
  W = 0.05, L = 0.04, SV = 0.10, IP = 1.0
)

#' Default regression plate appearances (batter)
#' The number of PAs at which regression is 50%
REGRESSION_PA <- 1200

#' Default regression batters faced (pitcher)
REGRESSION_BF <- 900

# --- Logging helpers ---

log_info <- function(msg) {

  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_warn <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# --- Marcel Projection ---

#' Marcel-style weighted projection for a single player
#'
#' Applies the Modified Marcel method: weight multiple seasons (5/4/3 by default
#' for most recent to oldest), then regress to league average based on sample size.
#'
#' For players with fewer than 3 seasons of data, uses only available seasons
#' with proportionally adjusted weights.
#'
#' @param seasons List of season stat rows for one player (most recent first).
#'   Each element is a named list or 1-row data.frame with counting stats and
#'   playing time (PA for batters, BF/G/GS for pitchers).
#' @param weights Numeric vector of season weights (default c(5, 4, 3)).
#'   Must have length >= number of seasons provided.
#' @param regression_pct Percent regression to mean (0–1) per stat. If NULL,
#'   regression is computed from sample size using the formula:
#'   regression = regression_pa / (regression_pa + weighted_pa)
#' @param player_type Character: "Batter" or "Pitcher" (auto-detected if NULL)
#' @return Named list with:
#'   \describe{
#'     \item{projected_stats}{Named numeric vector of projected counting stats}
#'     \item{projected_pa}{Projected plate appearances or batters faced}
#'     \item{data_quality}{Character: "full", "limited", or "rookie"}
#'     \item{seasons_used}{Integer: number of seasons used in projection}
#'   }
#' @export
marcel_project <- function(seasons, weights = c(5, 4, 3), regression_pct = NULL,
                           player_type = NULL) {
  if (!is.list(seasons) || length(seasons) == 0) {
    stop("seasons must be a non-empty list of stat rows", call. = FALSE)
  }

  n_seasons <- length(seasons)

  # Use only as many weights as we have seasons

  w <- weights[seq_len(min(n_seasons, length(weights)))]
  w_sum <- sum(w)

  # Auto-detect player type from stat columns if not specified
  if (is.null(player_type)) {
    first_season <- if (is.data.frame(seasons[[1]])) as.list(seasons[[1]]) else seasons[[1]]
    if ("PA" %in% names(first_season) || "AB" %in% names(first_season)) {
      player_type <- "Batter"
    } else if ("BF" %in% names(first_season) || "GS" %in% names(first_season)) {
      player_type <- "Pitcher"
    } else {
      player_type <- "Batter"  # default assumption
    }
  }

  # Determine stat categories based on player type
  if (player_type == "Batter") {
    counting_stats <- BATTING_COUNTING_STATS
    league_avg_rates <- LEAGUE_AVG_BATTING_PER_PA
    pt_col <- "PA"
    regression_denom <- REGRESSION_PA
  } else {
    counting_stats <- PITCHING_COUNTING_STATS
    league_avg_rates <- LEAGUE_AVG_PITCHING_PER_BF
    pt_col <- "BF"
    regression_denom <- REGRESSION_BF
  }

  # Compute weighted stats across seasons
  weighted_stats <- setNames(rep(0, length(counting_stats)), counting_stats)
  weighted_pt <- 0

  for (i in seq_along(w)) {
    season <- if (is.data.frame(seasons[[i]])) as.list(seasons[[i]]) else seasons[[i]]

    # Get playing time for this season
    pt <- get_playing_time(season, pt_col, player_type)
    weighted_pt <- weighted_pt + pt * w[i]

    # Weight each counting stat
    for (stat in counting_stats) {
      val <- safe_numeric(season[[stat]])
      weighted_stats[stat] <- weighted_stats[stat] + val * w[i]
    }
  }

  # Normalize by total weight to get weighted average totals
  weighted_stats <- weighted_stats / w_sum
  weighted_pt <- weighted_pt / w_sum

  # Apply regression to league average
  if (is.null(regression_pct)) {
    # Regression based on sample size:
    # regression_factor = regression_denom / (regression_denom + total_weighted_pa)
    total_weighted_pt <- weighted_pt * w_sum  # total weighted PAs across seasons
    reg_factor <- regression_denom / (regression_denom + total_weighted_pt)
  } else {
    reg_factor <- regression_pct
  }

  # Regress each stat
  projected_stats <- weighted_stats
  for (stat in counting_stats) {
    league_rate <- league_avg_rates[[stat]]
    if (!is.null(league_rate)) {
      # Regress: blend player rate with league average rate, then scale to projected PA
      player_rate <- if (weighted_pt > 0) weighted_stats[stat] / weighted_pt else 0
      regressed_rate <- player_rate * (1 - reg_factor) + league_rate * reg_factor
      projected_stats[stat] <- regressed_rate * weighted_pt
    }
  }

  # Handle pitching decisions (W, L, SV, IP) separately since they don't rate per BF
  if (player_type == "Pitcher") {
    projected_stats <- regress_pitching_decisions(
      projected_stats, seasons, w, w_sum, reg_factor
    )
  }

  # Floor negative counting stats at zero (Requirement: floor negatives)
  projected_stats <- pmax(projected_stats, 0)

  # Determine data quality
  data_quality <- if (n_seasons >= 3) {
    "full"
  } else if (n_seasons == 2) {
    "limited"
  } else {
    "rookie"
  }

  list(
    projected_stats = projected_stats,
    projected_pa = weighted_pt,
    data_quality = data_quality,
    seasons_used = n_seasons
  )
}


# --- Age Curve Adjustment ---

#' Apply position-specific age curve adjustment to projected stats
#'
#' Adjusts projections based on where a player is on their aging curve.
#' - Batters peak at age 27, decline ~0.5-1% per year after
#' - Pitchers peak at age 28, decline ~0.7-1.5% per year after
#' - Pre-peak players get a slight positive adjustment
#'
#' @param projected_stats Named numeric vector of projected counting stats
#' @param age Player's current age (numeric)
#' @param position Player position (character). Used to determine if batter or pitcher.
#'   Pitcher positions: "SP", "RP", "P", "CL"
#'   All others treated as batters.
#' @return Named numeric vector of age-adjusted projected stats (floored at zero)
#' @export
apply_age_curve <- function(projected_stats, age, position) {
  if (!is.numeric(age) || length(age) != 1 || is.na(age)) {
    # If age is unknown, return projections unmodified
    return(projected_stats)
  }

  # Determine if player is a pitcher

  pitcher_positions <- c("SP", "RP", "P", "CL")
  is_pitcher <- toupper(position) %in% pitcher_positions

  if (is_pitcher) {
    peak_age <- 28
    decline_rate_per_year <- 0.010   # 1.0% per year post-peak
    pre_peak_gain_per_year <- 0.005  # 0.5% per year pre-peak
  } else {
    peak_age <- 27
    decline_rate_per_year <- 0.007   # 0.7% per year post-peak
    pre_peak_gain_per_year <- 0.004  # 0.4% per year pre-peak
  }

  years_from_peak <- age - peak_age

  if (years_from_peak > 0) {
    # Post-peak: decline accelerates slightly with age
    # Use a mild quadratic: decline = rate * years + 0.001 * years^2
    adjustment <- 1 - (decline_rate_per_year * years_from_peak +
                         0.001 * years_from_peak^2)
  } else if (years_from_peak < 0) {
    # Pre-peak: slight improvement
    years_to_peak <- abs(years_from_peak)
    adjustment <- 1 + (pre_peak_gain_per_year * years_to_peak)
    # Cap pre-peak adjustment (very young players don't get unlimited bonus)
    adjustment <- min(adjustment, 1.03)
  } else {
    # At peak age — no adjustment
    adjustment <- 1
  }

  # Apply adjustment to all stats

  adjusted <- projected_stats * adjustment

  # Floor negative counting stats at zero
  adjusted <- pmax(adjusted, 0)

  adjusted
}


# --- Main Projection Generator ---

#' Generate rest-of-season projections for all MLB players
#'
#' Orchestrates the full projection pipeline:
#' 1. Match players across seasons by name/ID
#' 2. Apply Marcel-style weighted projection
#' 3. Apply age curve adjustments
#' 4. Mark data quality (full/limited/rookie)
#' 5. Floor negative counting stats
#'
#' @param current_stats Data frame of current season stats (from cache).
#'   Expected columns: Name, bbref_id, plus counting stat columns.
#'   Can be batting or pitching stats.
#' @param historical List of prior-year stats data frames (most recent first).
#'   Each should have the same structure as current_stats.
#' @param player_info Data frame from players.rds with columns:
#'   fullname, age, position, cbs_id, etc.
#' @param player_type Character: "Batter" or "Pitcher". Auto-detected if NULL.
#' @return Data frame with columns:
#'   - player_name, bbref_id, position, player_type
#'   - proj_* columns for each counting stat
#'   - projected_pa (or projected_bf for pitchers)
#'   - data_quality: "full", "limited", or "rookie"
#'   - age
#' @export
generate_projections <- function(current_stats, historical, player_info,
                                 player_type = NULL) {
  if (is.null(current_stats) || nrow(current_stats) == 0) {
    log_warn("current_stats is empty — returning empty projections")
    return(data.frame())
  }

  # Auto-detect player type
  if (is.null(player_type)) {
    if ("PA" %in% names(current_stats) || "AB" %in% names(current_stats)) {
      player_type <- "Batter"
    } else {
      player_type <- "Pitcher"
    }
  }

  if (player_type == "Batter") {
    counting_stats <- BATTING_COUNTING_STATS
  } else {
    counting_stats <- PITCHING_COUNTING_STATS
  }

  # Ensure historical is a list of data frames

  if (is.data.frame(historical)) {
    historical <- list(historical)
  }
  if (!is.list(historical)) {
    historical <- list()
  }

  # Build all-seasons list: current + historical (most recent first)
  all_seasons <- c(list(current_stats), historical)

  # Get unique player identifiers from current season

  # Use bbref_id as primary key, fall back to Name
  id_col <- if ("bbref_id" %in% names(current_stats)) "bbref_id" else "Name"
  player_ids <- unique(current_stats[[id_col]])

  # Build projections for each player

  results <- vector("list", length(player_ids))

  for (idx in seq_along(player_ids)) {
    pid <- player_ids[idx]

    # Gather this player's seasons (most recent first)
    player_seasons <- list()
    for (s in seq_along(all_seasons)) {
      season_df <- all_seasons[[s]]
      if (is.null(season_df) || nrow(season_df) == 0) next

      # Match player in this season
      if (id_col %in% names(season_df)) {
        match_rows <- season_df[[id_col]] == pid
        if (any(match_rows, na.rm = TRUE)) {
          row <- season_df[which(match_rows)[1], , drop = FALSE]
          player_seasons <- c(player_seasons, list(as.list(row)))
        }
      }
    }

    if (length(player_seasons) == 0) next

    # Get player name from current season
    player_name <- player_seasons[[1]][["Name"]]
    if (is.null(player_name)) player_name <- as.character(pid)

    # Get player info (age, position) from player database
    player_age <- NA_real_
    player_position <- if (player_type == "Batter") "DH" else "SP"

    if (!is.null(player_info) && nrow(player_info) > 0) {
      # Try to match by name
      info_match <- match_player_info(player_name, player_info)
      if (!is.null(info_match)) {
        player_age <- safe_numeric(info_match$age)
        if (!is.null(info_match$position) && !is.na(info_match$position)) {
          player_position <- info_match$position
        }
      }
    }

    # Run Marcel projection
    projection <- tryCatch(
      marcel_project(player_seasons, player_type = player_type),
      error = function(e) {
        log_warn(sprintf("Projection failed for %s: %s", player_name, e$message))
        NULL
      }
    )
    if (is.null(projection)) next

    # Apply age curve
    adj_stats <- apply_age_curve(projection$projected_stats, player_age, player_position)

    # Build result row
    result <- list(
      player_name = player_name,
      bbref_id = as.character(pid),
      position = player_position,
      player_type = player_type,
      data_quality = projection$data_quality,
      seasons_used = projection$seasons_used,
      age = player_age
    )

    # Add projected PA/BF
    if (player_type == "Batter") {
      result$projected_pa <- projection$projected_pa
    } else {
      result$projected_bf <- projection$projected_pa
    }

    # Add projected stats with proj_ prefix
    for (stat in names(adj_stats)) {
      result[[paste0("proj_", stat)]] <- adj_stats[stat]
    }

    results[[idx]] <- result
  }

  # Remove NULLs and convert to data frame

  results <- results[!vapply(results, is.null, logical(1))]

  if (length(results) == 0) {
    log_warn("No projections generated — returning empty data frame")
    return(data.frame())
  }

  # Convert list of lists to data frame
  projections_df <- do.call(rbind, lapply(results, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  }))

  log_info(sprintf("Generated %d %s projections (%s full, %s limited, %s rookie)",
                   nrow(projections_df), player_type,
                   sum(projections_df$data_quality == "full"),
                   sum(projections_df$data_quality == "limited"),
                   sum(projections_df$data_quality == "rookie")))

  projections_df
}


# --- Internal Helper Functions ---

#' Safely extract numeric value from a stat field
#' @param x Value to convert to numeric
#' @return Numeric value or 0 if NA/NULL
#' @keywords internal
safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) return(0)
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) return(0)
  val
}

#' Get playing time from a season stat row
#' @param season Named list of stats
#' @param pt_col Primary playing time column name ("PA" or "BF")
#' @param player_type "Batter" or "Pitcher"
#' @return Numeric playing time value
#' @keywords internal
get_playing_time <- function(season, pt_col, player_type) {
  pt <- safe_numeric(season[[pt_col]])

  # Fallback: compute PA from AB + BB + HBP for batters

  if (pt == 0 && player_type == "Batter") {
    ab <- safe_numeric(season[["AB"]])
    bb <- safe_numeric(season[["BB"]])
    hbp <- safe_numeric(season[["HBP"]])
    sf <- safe_numeric(season[["SF"]])
    sh <- safe_numeric(season[["SH"]])
    pt <- ab + bb + hbp + sf + sh
  }

  # Fallback: estimate BF from IP for pitchers
  if (pt == 0 && player_type == "Pitcher") {
    ip <- safe_numeric(season[["IP"]])
    if (ip > 0) {
      # Rough estimate: ~4.3 batters per inning
      pt <- round(ip * 4.3)
    }
  }

  pt
}

#' Regress pitching decisions (W, L, SV, IP) which don't rate per BF
#' @param projected_stats Current projected stats vector
#' @param seasons List of season stat rows
#' @param w Weights vector
#' @param w_sum Sum of weights
#' @param reg_factor Regression factor
#' @return Updated projected_stats with regressed decisions
#' @keywords internal
regress_pitching_decisions <- function(projected_stats, seasons, w, w_sum, reg_factor) {
  # For W, L, SV, IP: compute weighted averages per game
  decision_stats <- c("W", "L", "SV", "IP")

  for (stat in decision_stats) {
    if (!(stat %in% names(projected_stats))) next

    weighted_val <- 0
    weighted_games <- 0

    for (i in seq_along(w)) {
      season <- if (is.data.frame(seasons[[i]])) as.list(seasons[[i]]) else seasons[[i]]
      val <- safe_numeric(season[[stat]])
      gs <- safe_numeric(season[["GS"]])
      g <- safe_numeric(season[["G"]])

      # Use GS for starters, G for relievers
      games <- if (gs > 0) gs else g
      if (games > 0) {
        weighted_val <- weighted_val + val * w[i]
        weighted_games <- weighted_games + games * w[i]
      }
    }

    if (weighted_games > 0) {
      # Per-game rate
      player_rate <- weighted_val / weighted_games

      # Get league average rate
      # Determine if predominantly a starter
      avg_gs <- 0
      avg_g <- 0
      for (i in seq_along(w)) {
        season <- if (is.data.frame(seasons[[i]])) as.list(seasons[[i]]) else seasons[[i]]
        avg_gs <- avg_gs + safe_numeric(season[["GS"]]) * w[i]
        avg_g <- avg_g + safe_numeric(season[["G"]]) * w[i]
      }
      is_starter <- (avg_gs / max(avg_g, 1)) > 0.5

      league_rates <- if (is_starter) LEAGUE_AVG_PITCHING_PER_GS else LEAGUE_AVG_PITCHING_PER_G_RP
      league_rate <- if (!is.null(league_rates[[stat]])) league_rates[[stat]] else 0

      # Regress
      regressed_rate <- player_rate * (1 - reg_factor) + league_rate * reg_factor

      # Project to weighted game count
      proj_games <- weighted_games / w_sum
      projected_stats[stat] <- regressed_rate * proj_games
    }
  }

  projected_stats
}

#' Match a player name to the player_info database
#' @param player_name Character name to match
#' @param player_info Data frame with fullname, age, position columns
#' @return Matched row as list, or NULL if no match
#' @keywords internal
match_player_info <- function(player_name, player_info) {
  if (is.null(player_name) || is.na(player_name)) return(NULL)


  # Exact match first
  exact <- which(tolower(player_info$fullname) == tolower(player_name))
  if (length(exact) > 0) {
    return(as.list(player_info[exact[1], ]))
  }

  # Try partial match (last name + first initial)
  # This handles cases like "J.D. Martinez" vs "JD Martinez"
  clean_name <- gsub("[^a-z ]", "", tolower(player_name))
  clean_db <- gsub("[^a-z ]", "", tolower(player_info$fullname))
  partial <- which(clean_db == clean_name)
  if (length(partial) > 0) {
    return(as.list(player_info[partial[1], ]))
  }

  NULL
}


# --- Statcast Quality Adjustments ---

#' Confidence interval width multipliers by data quality
#' "full" (3+ seasons) → ±15%, "limited" (2 seasons) → ±25%, "rookie" (1 season) → ±40%
CI_WIDTH_BY_QUALITY <- list(
  full    = 0.15,
  limited = 0.25,

  rookie  = 0.40
)

#' Apply Statcast batted-ball quality adjustment to batting projections
#'
#' When Statcast metrics are available for a player, adjusts the projected
#' fantasy points based on quality-of-contact indicators. This captures
#' players whose traditional stats under/overperform their underlying quality.
#'
#' Metrics used:
#' - exit_velocity: average exit velocity (league avg ~88.5 mph)
#' - launch_angle: average launch angle (optimal ~12-15 degrees for power)
#' - barrel_rate: % of batted balls that are barrels (league avg ~6.5%)
#' - xBA: expected batting average based on batted ball profile
#' - xSLG: expected slugging based on batted ball profile
#'
#' The adjustment is multiplicative: if Statcast metrics suggest the player
#' is outperforming expectations, apply a downward correction and vice versa.
#' Capped at ±10% to avoid over-weighting Statcast data.
#'
#' @param proj_pts Numeric: baseline projected points per week
#' @param statcast_data Named list or 1-row data.frame with Statcast metrics.
#'   Expected optional fields: exit_velocity, barrel_rate, xBA, xSLG, BA, SLG
#' @return Numeric: adjusted projected points per week
#' @export
apply_statcast_adjustment <- function(proj_pts, statcast_data) {
  if (is.null(statcast_data) || length(statcast_data) == 0) {
    return(proj_pts)
  }

  if (is.data.frame(statcast_data)) {
    statcast_data <- as.list(statcast_data[1, , drop = FALSE])
  }

  adjustment <- 1.0
  n_signals <- 0

  # Signal 1: Exit velocity relative to league average (88.5 mph)
  ev <- safe_numeric(statcast_data[["exit_velocity"]])
  if (ev > 0) {
    ev_league_avg <- 88.5
    # Each mph above average adds ~1% to projection
    ev_adj <- (ev - ev_league_avg) * 0.01
    adjustment <- adjustment + ev_adj
    n_signals <- n_signals + 1
  }

  # Signal 2: Barrel rate relative to league average (6.5%)
  barrel <- safe_numeric(statcast_data[["barrel_rate"]])
  if (barrel > 0) {
    barrel_league_avg <- 6.5
    # Each pct point of barrel above average adds ~1.5%
    barrel_adj <- (barrel - barrel_league_avg) * 0.015
    adjustment <- adjustment + barrel_adj
    n_signals <- n_signals + 1
  }

  # Signal 3: xBA vs actual BA (under/overperformance)
  xba <- safe_numeric(statcast_data[["xBA"]])
  ba <- safe_numeric(statcast_data[["BA"]])
  if (xba > 0 && ba > 0) {
    # If xBA > BA, player is underperforming (positive adjustment)
    ba_diff <- xba - ba
    # Scale: each 0.010 xBA edge = ~1% boost
    xba_adj <- ba_diff * 100 * 0.01
    adjustment <- adjustment + xba_adj
    n_signals <- n_signals + 1
  }

  # Signal 4: xSLG vs actual SLG
  xslg <- safe_numeric(statcast_data[["xSLG"]])
  slg <- safe_numeric(statcast_data[["SLG"]])
  if (xslg > 0 && slg > 0) {
    slg_diff <- xslg - slg
    xslg_adj <- slg_diff * 100 * 0.005
    adjustment <- adjustment + xslg_adj
    n_signals <- n_signals + 1
  }

  # Average the adjustment signals to prevent stacking

  if (n_signals > 1) {
    # Blend: the total adjustment beyond 1.0 is averaged over signals
    total_adj <- adjustment - 1.0
    adjustment <- 1.0 + (total_adj / n_signals)
  }

  # Cap adjustment at ±10% to avoid over-weighting Statcast
  adjustment <- max(0.90, min(1.10, adjustment))

  proj_pts * adjustment
}


# --- Fantasy Points Conversion and Confidence Intervals ---

#' Convert projected stats to projected H2H Points per week with confidence intervals
#'
#' Takes the output of generate_projections() and adds:
#' - proj_pts_per_week: projected fantasy points per week
#' - confidence_lo: lower bound of 80% confidence interval
#' - confidence_hi: upper bound of 80% confidence interval
#'
#' Confidence interval width is based on data_quality:
#' - "full" (3+ seasons): ±15%
#' - "limited" (2 seasons): ±25%
#' - "rookie" (1 season): ±40%
#'
#' When Statcast data columns are present in the projections data frame,
#' the function incorporates quality-of-contact adjustments.
#'
#' @param projections Data frame of projected stats (output from generate_projections).
#'   Must include columns: player_type, data_quality, and proj_* stat columns.
#'   May optionally include Statcast columns: exit_velocity, barrel_rate, xBA, xSLG, BA, SLG.
#' @param scoring_weights List with $batting and $pitching sub-lists of scoring weights.
#'   If NULL, loads defaults from league_constitution.rds.
#' @return The projections data frame augmented with:
#'   - proj_pts_per_week (numeric)
#'   - confidence_lo (numeric, non-negative)
#'   - confidence_hi (numeric, non-negative)
#' @export
#' @examples
#' \dontrun{
#' projections <- generate_projections(batting_stats, hist_list, player_db)
#' scored <- project_fantasy_points(projections, scoring_weights = NULL)
#' }
project_fantasy_points <- function(projections, scoring_weights = NULL) {
  if (is.null(projections) || nrow(projections) == 0) {
    log_warn("Empty projections data frame — returning unchanged")
    return(projections)
  }

  # Load scoring weights if not provided
  if (is.null(scoring_weights)) {
    scoring_weights <- load_default_weights()
  }

  # Ensure scoring_weights has batting and pitching sub-lists
  if (is.null(scoring_weights$batting) || is.null(scoring_weights$pitching)) {
    stop("scoring_weights must have $batting and $pitching sub-lists", call. = FALSE)
  }

  # Statcast columns that may be present for quality adjustment
  statcast_cols <- c("exit_velocity", "barrel_rate", "xBA", "xSLG", "BA", "SLG")

  # Compute fantasy points for each player
  n <- nrow(projections)
  pts_per_week <- numeric(n)

  for (i in seq_len(n)) {
    row <- projections[i, , drop = FALSE]
    ptype <- as.character(row$player_type)

    if (ptype == "Batter") {
      # Build stat line from proj_* columns using scoring weight keys
      stat_line <- build_batting_stat_line(row)
      raw_pts <- compute_batting_points(stat_line, scoring_weights$batting)

      # Estimate games from projected PA (assume ~4 PA per game)
      proj_pa <- safe_numeric(row[["projected_pa"]])
      games <- if (proj_pa > 0) proj_pa / 4 else 162  # default full season
    } else {
      # Pitcher
      stat_line <- build_pitching_stat_line(row)
      raw_pts <- compute_pitching_points(stat_line, scoring_weights$pitching)

      # Estimate games from projected BF or IP
      proj_bf <- safe_numeric(row[["projected_bf"]])
      proj_ip <- safe_numeric(row[["proj_IP"]])
      if (proj_bf > 0) {
        games <- proj_bf / 4.3 / 5.5  # BF per inning / IP per game
        games <- max(games, 1)
      } else if (proj_ip > 0) {
        # Starters: ~6 IP per start => games = IP/6
        # Relievers: ~1 IP per game => games = IP/1
        gs_proxy <- safe_numeric(row[["proj_W"]]) + safe_numeric(row[["proj_L"]])
        if (gs_proxy > 5) {
          games <- proj_ip / 5.5
        } else {
          games <- proj_ip / 1.0
        }
      } else {
        games <- 30  # default assumption
      }
    }

    # Convert total points to per-week (23-week season, ~7 games per week for batters)
    if (ptype == "Batter") {
      games_per_week <- 6.5
    } else {
      # Pitchers: starters ~ 1-2 starts/week, relievers ~ 4-5 apps/week
      games_per_week <- games / 23
    }

    # pts_per_week = (total_season_points / games) * games_per_week
    if (games > 0) {
      ppw <- (raw_pts / games) * games_per_week
    } else {
      ppw <- 0
    }

    # Apply Statcast adjustment if data available
    has_statcast <- any(statcast_cols %in% names(row))
    if (has_statcast && ptype == "Batter") {
      sc_data <- row[, intersect(statcast_cols, names(row)), drop = FALSE]
      # Only apply if at least one Statcast metric has a value
      if (any(vapply(sc_data, function(x) safe_numeric(x) > 0, logical(1)))) {
        ppw <- apply_statcast_adjustment(ppw, sc_data)
      }
    }

    pts_per_week[i] <- ppw
  }

  # Add projected points per week
  projections$proj_pts_per_week <- pts_per_week

  # Generate confidence intervals based on data quality
  # Default CI width lookup
  ci_width <- vapply(projections$data_quality, function(dq) {
    w <- CI_WIDTH_BY_QUALITY[[dq]]
    if (is.null(w)) CI_WIDTH_BY_QUALITY[["limited"]]  # fallback
    else w
  }, numeric(1))

  # Confidence bounds (80% CI)
  projections$confidence_lo <- pmax(0, pts_per_week * (1 - ci_width))
  projections$confidence_hi <- pmax(0, pts_per_week * (1 + ci_width))

  log_info(sprintf("Added fantasy points: mean=%.1f pts/week, range=[%.1f, %.1f]",
                   mean(pts_per_week, na.rm = TRUE),
                   min(pts_per_week, na.rm = TRUE),
                   max(pts_per_week, na.rm = TRUE)))

  projections
}


# --- Stat Line Builders for Fantasy Points ---

#' Build a batting stat line from projection row for scoring computation
#'
#' Maps proj_* columns back to the names expected by compute_batting_points.
#'
#' @param row 1-row data.frame from projections
#' @return Named list suitable for compute_batting_points()
#' @keywords internal
build_batting_stat_line <- function(row) {
  stat_line <- list(
    X1B = safe_numeric(row[["proj_X1B"]]),
    X2B = safe_numeric(row[["proj_X2B"]]),
    X3B = safe_numeric(row[["proj_X3B"]]),
    HR  = safe_numeric(row[["proj_HR"]]),
    R   = safe_numeric(row[["proj_R"]]),
    RBI = safe_numeric(row[["proj_RBI"]]),
    BB  = safe_numeric(row[["proj_BB"]]),
    HBP = safe_numeric(row[["proj_HBP"]]),
    SB  = safe_numeric(row[["proj_SB"]]),
    CS  = safe_numeric(row[["proj_CS"]]),
    SO  = safe_numeric(row[["proj_SO"]])
  )
  # Grand slams and cycles are rare; estimate from HR count
  # Approx 1 in 25 HR is a grand slam
  stat_line$grand_slams <- stat_line$HR / 25
  stat_line$cycle <- 0  # too rare to project

  stat_line
}

#' Build a pitching stat line from projection row for scoring computation
#'
#' Maps proj_* columns back to the names expected by compute_pitching_points.
#'
#' @param row 1-row data.frame from projections
#' @return Named list suitable for compute_pitching_points()
#' @keywords internal
build_pitching_stat_line <- function(row) {
  ip <- safe_numeric(row[["proj_IP"]])
  stat_line <- list(
    IP  = ip,
    SO  = safe_numeric(row[["proj_SO"]]),
    W   = safe_numeric(row[["proj_W"]]),
    L   = safe_numeric(row[["proj_L"]]),
    SV  = safe_numeric(row[["proj_SV"]]),
    H   = safe_numeric(row[["proj_H"]]),
    ER  = safe_numeric(row[["proj_ER"]]),
    BB  = safe_numeric(row[["proj_BB"]])
  )

  # HLD, QS, CG, NH, PG are not in the Marcel counting stats but needed for scoring
  # Estimate from context, SCALED to the player's projected workload
  gs_proxy <- stat_line$W + stat_line$L
  if (gs_proxy > 5) {
    # Likely a starter
    # QS: approx 55% of starts for decent pitchers
    stat_line$QS <- gs_proxy * 0.55
    # CG: very rare in modern era (~2% of starts)
    stat_line$CG <- gs_proxy * 0.02
    stat_line$HLD <- 0
  } else {
    # Likely a reliever — scale holds by projected appearances
    stat_line$QS <- 0
    stat_line$CG <- 0
    # Estimate projected appearances from IP (relievers avg ~1 IP per appearance)
    proj_appearances <- max(ip / 1.0, 0)
    # A middle reliever earns holds at ~0.20 per appearance (~15 HLD / 75 G)
    # Closers (high SV) get fewer holds
    holds_per_appearance <- if (stat_line$SV < 5) 0.20 else 0.0
    stat_line$HLD <- proj_appearances * holds_per_appearance
  }

  # NH and PG are too rare to project meaningfully
  stat_line$NH <- 0
  stat_line$PG <- 0

  # IBB: estimate ~5% of walks for starters, ~10% for relievers
  if (gs_proxy > 5) {
    stat_line$IBB <- stat_line$BB * 0.05
  } else {
    stat_line$IBB <- stat_line$BB * 0.10
  }

  stat_line
}

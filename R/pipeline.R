#!/usr/bin/env Rscript
#' Master Analysis Pipeline
#'
#' Runs the full analysis pipeline in sequence:
#'   ingest (load cached) → project → value → decide
#'
#' Usage from project root:
#'   Rscript R/pipeline.R
#'
#' Requirements: all (integration)
#' This script wires together the ingest, projection, valuation, and decision
#' modules, verifying inter-module data contracts at each step.

# --- Setup ---

# Ensure working directory is the project root
if (!file.exists("data/league_constitution.rds")) {
  # Try to detect project root

  if (file.exists(file.path("..", "data", "league_constitution.rds"))) {
    setwd("..")
  } else {
    stop("Cannot find project root. Run this script from the la-z-boyz directory.",
         call. = FALSE)
  }
}

# --- Source all modules ---

cat("=== Fantasy Baseball Analysis Pipeline ===\n")
cat(sprintf("Started: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# Utility modules
source("R/utils/scoring.R")
source("R/utils/player_linker.R")
source("R/utils/keeper_value.R")
source("R/utils/salary_rules.R")
source("R/utils/serialization.R")

# Analysis modules
source("R/analysis/projection_model.R")
source("R/analysis/player_valuation.R")

cat("[OK] All modules sourced successfully.\n\n")

# --- Helper: Data contract verification ---

#' Verify that a data frame has expected columns with correct types
#' @param df Data frame to check
#' @param name Character name for error messages
#' @param expected_cols Named character vector: name = expected type ("character",
#'   "numeric", "integer", "logical"). Use "any" to skip type check.
#' @return TRUE if valid, FALSE otherwise (with warnings)
verify_data_contract <- function(df, name, expected_cols) {
  if (is.null(df) || !is.data.frame(df)) {
    warning(sprintf("[CONTRACT] %s is not a valid data frame", name))
    return(FALSE)
  }

  if (nrow(df) == 0) {
    warning(sprintf("[CONTRACT] %s has zero rows", name))
    return(FALSE)
  }

  valid <- TRUE

  # Check column existence
  missing_cols <- setdiff(names(expected_cols), names(df))
  if (length(missing_cols) > 0) {
    warning(sprintf("[CONTRACT] %s is missing columns: %s",
                    name, paste(missing_cols, collapse = ", ")))
    valid <- FALSE
  }

  # Check column types for present columns
  present_cols <- intersect(names(expected_cols), names(df))
  for (col in present_cols) {
    expected_type <- expected_cols[[col]]
    if (expected_type == "any") next

    actual_class <- class(df[[col]])[1]

    type_ok <- switch(expected_type,
      "character" = is.character(df[[col]]),
      "numeric"   = is.numeric(df[[col]]),
      "integer"   = is.integer(df[[col]]) || is.numeric(df[[col]]),
      "logical"   = is.logical(df[[col]]),
      TRUE  # unknown type = skip
    )

    if (!type_ok) {
      warning(sprintf("[CONTRACT] %s$%s: expected %s, got %s",
                      name, col, expected_type, actual_class))
      valid <- FALSE
    }
  }

  valid
}

# --- Step 1: Load cached ingest data ---

cat("--- Step 1: Load cached ingest data ---\n")

# Load league constitution (required for scoring weights)
constitution <- load_rds_safe("data/league_constitution.rds")
if (is.null(constitution)) {
  stop("League constitution is required. Run parse_constitution.R first.", call. = FALSE)
}
cat("  [OK] league_constitution.rds loaded\n")

# Load players database
players <- load_rds_safe("data/cache/players.rds")
if (is.null(players)) {
  cat("  [SKIP] players.rds not found — projection quality may be reduced\n")
} else {
  verify_data_contract(players, "players", c(
    cbs_id   = "character",
    fullname = "character",
    position = "character",
    age      = "any"
  ))
  cat(sprintf("  [OK] players.rds loaded (%d players)\n", nrow(players)))
}

# Load rosters
rosters <- load_rds_safe("data/cache/rosters.rds")
if (is.null(rosters)) {
  cat("  [SKIP] rosters.rds not found — surplus/keeper values unavailable\n")
} else {
  verify_data_contract(rosters, "rosters", c(
    team_name        = "character",
    player_name      = "character",
    player_type      = "character",
    salary           = "numeric",
    is_minor_contract = "logical"
  ))
  cat(sprintf("  [OK] rosters.rds loaded (%d rostered players)\n", nrow(rosters)))
}

# Load standings
standings <- load_rds_safe("data/cache/standings.rds")
if (is.null(standings)) {
  cat("  [SKIP] standings.rds not found — competitive context unavailable\n")
} else {
  verify_data_contract(standings, "standings", c(
    team_name    = "character",
    wins         = "any",
    losses       = "any",
    total_points = "numeric"
  ))
  cat(sprintf("  [OK] standings.rds loaded (%d teams)\n", nrow(standings)))
}

# Load current season stats (batting + pitching)
current_year <- as.integer(format(Sys.Date(), "%Y"))
batting_file <- sprintf("data/cache/stats_batting_%d.rds", current_year)
pitching_file <- sprintf("data/cache/stats_pitching_%d.rds", current_year)

stats_batting <- load_rds_safe(batting_file)
stats_pitching <- load_rds_safe(pitching_file)

if (is.null(stats_batting)) {
  cat(sprintf("  [SKIP] %s not found\n", batting_file))
} else {
  cat(sprintf("  [OK] %s loaded (%d batters)\n", batting_file, nrow(stats_batting)))
}
if (is.null(stats_pitching)) {
  cat(sprintf("  [SKIP] %s not found\n", pitching_file))
} else {
  cat(sprintf("  [OK] %s loaded (%d pitchers)\n", pitching_file, nrow(stats_pitching)))
}

# Load historical stats (up to 3 prior seasons)
historical_batting <- list()
historical_pitching <- list()
for (yr in seq(current_year - 1, current_year - 3)) {
  hb <- load_rds_safe(sprintf("data/cache/stats_batting_%d.rds", yr))
  hp <- load_rds_safe(sprintf("data/cache/stats_pitching_%d.rds", yr))
  if (!is.null(hb)) historical_batting <- c(historical_batting, list(hb))
  if (!is.null(hp)) historical_pitching <- c(historical_pitching, list(hp))
}
cat(sprintf("  [OK] Historical data: %d batting seasons, %d pitching seasons\n",
            length(historical_batting), length(historical_pitching)))

cat("\n")

# --- Step 2: Generate projections ---

cat("--- Step 2: Generate projections ---\n")

can_project <- !is.null(stats_batting) || !is.null(stats_pitching)

if (!can_project) {
  cat("  [SKIP] No current stats available — cannot generate projections\n")
  projections <- NULL
} else {
  projections_batting <- NULL
  projections_pitching <- NULL

  # Generate batting projections
  if (!is.null(stats_batting)) {
    projections_batting <- tryCatch(
      generate_projections(
        current_stats = stats_batting,
        historical = historical_batting,
        player_info = players,
        player_type = "Batter"
      ),
      error = function(e) {
        cat(sprintf("  [ERROR] Batting projections failed: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(projections_batting) && nrow(projections_batting) > 0) {
      cat(sprintf("  [OK] Batting projections: %d players\n", nrow(projections_batting)))
    }
  }

  # Generate pitching projections
  if (!is.null(stats_pitching)) {
    projections_pitching <- tryCatch(
      generate_projections(
        current_stats = stats_pitching,
        historical = historical_pitching,
        player_info = players,
        player_type = "Pitcher"
      ),
      error = function(e) {
        cat(sprintf("  [ERROR] Pitching projections failed: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(projections_pitching) && nrow(projections_pitching) > 0) {
      cat(sprintf("  [OK] Pitching projections: %d players\n", nrow(projections_pitching)))
    }
  }

  # Combine batting and pitching projections
  projections <- NULL
  if (!is.null(projections_batting) && nrow(projections_batting) > 0 &&
      !is.null(projections_pitching) && nrow(projections_pitching) > 0) {
    # Harmonize columns before binding
    all_cols <- union(names(projections_batting), names(projections_pitching))
    for (col in setdiff(all_cols, names(projections_batting))) {
      projections_batting[[col]] <- NA
    }
    for (col in setdiff(all_cols, names(projections_pitching))) {
      projections_pitching[[col]] <- NA
    }
    projections <- rbind(
      projections_batting[, all_cols, drop = FALSE],
      projections_pitching[, all_cols, drop = FALSE]
    )
  } else if (!is.null(projections_batting) && nrow(projections_batting) > 0) {
    projections <- projections_batting
  } else if (!is.null(projections_pitching) && nrow(projections_pitching) > 0) {
    projections <- projections_pitching
  }

  if (is.null(projections) || nrow(projections) == 0) {
    cat("  [SKIP] No projections generated\n")
    projections <- NULL
  }
}

cat("\n")

# --- Step 3: Project fantasy points (pts/week + confidence intervals) ---

cat("--- Step 3: Convert projections to fantasy points ---\n")

if (is.null(projections)) {
  cat("  [SKIP] No projections available\n")
} else {
  scoring_weights <- constitution$scoring

  projections <- tryCatch(
    project_fantasy_points(projections, scoring_weights),
    error = function(e) {
      cat(sprintf("  [ERROR] Fantasy points conversion failed: %s\n", e$message))
      projections  # return unmodified
    }
  )

  # Verify output contract
  if ("proj_pts_per_week" %in% names(projections)) {
    verify_data_contract(projections, "projections (scored)", c(
      player_name      = "character",
      position         = "character",
      player_type      = "character",
      proj_pts_per_week = "numeric",
      confidence_lo    = "numeric",
      confidence_hi    = "numeric",
      data_quality     = "character"
    ))

    cat(sprintf("  [OK] Fantasy points computed for %d players\n", nrow(projections)))
    cat(sprintf("       Mean pts/week: %.1f | Range: [%.1f, %.1f]\n",
                mean(projections$proj_pts_per_week, na.rm = TRUE),
                min(projections$proj_pts_per_week, na.rm = TRUE),
                max(projections$proj_pts_per_week, na.rm = TRUE)))

    # Save projections
    save_rds_with_metadata(
      projections,
      "data/cache/projections.rds",
      source = "pipeline.R::project_fantasy_points",
      league_id = "l-z-bs"
    )
    cat("  [OK] Saved data/cache/projections.rds\n")
  } else {
    cat("  [WARN] proj_pts_per_week column missing after scoring conversion\n")
  }
}

cat("\n")

# --- Step 4: Run valuation pipeline ---

cat("--- Step 4: Run player valuation ---\n")

if (is.null(projections) || !("proj_pts_per_week" %in% names(projections))) {
  cat("  [SKIP] No scored projections available for valuation\n")
  valuations <- NULL
} else {
  # Merge salary data from rosters if available
  if (!is.null(rosters)) {
    # Join salary info onto projections by player name
    roster_salary <- rosters[, c("player_name", "salary", "is_minor_contract"),
                             drop = FALSE]
    # Deduplicate (players might appear on multiple lines if multi-position)
    roster_salary <- roster_salary[!duplicated(roster_salary$player_name), ]

    # Merge
    projections_for_val <- merge(
      projections, roster_salary,
      by = "player_name", all.x = TRUE, suffixes = c("", ".roster")
    )
  } else {
    projections_for_val <- projections
  }

  valuations <- tryCatch(
    run_valuation_pipeline(projections_for_val),
    error = function(e) {
      cat(sprintf("  [ERROR] Valuation pipeline failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(valuations) && nrow(valuations) > 0) {
    # Verify valuation output contract
    verify_data_contract(valuations, "valuations", c(
      player_name           = "character",
      position              = "character",
      dollar_value          = "numeric",
      pts_above_replacement = "numeric",
      replacement_level     = "numeric",
      positional_scarcity   = "numeric"
    ))

    cat(sprintf("  [OK] Valuation complete: %d players valued\n", nrow(valuations)))
    cat(sprintf("       Total pool distributed: $%.0f\n",
                sum(valuations$dollar_value[valuations$dollar_value > 0])))
    cat(sprintf("       Top 5 valued players:\n"))
    top5 <- head(valuations[order(-valuations$dollar_value), ], 5)
    for (i in seq_len(nrow(top5))) {
      cat(sprintf("         %d. %s (%s) — $%.1f\n",
                  i, top5$player_name[i], top5$position[i], top5$dollar_value[i]))
    }

    # Save valuations
    save_rds_with_metadata(
      valuations,
      "data/cache/valuations.rds",
      source = "pipeline.R::run_valuation_pipeline",
      league_id = "l-z-bs"
    )
    cat("  [OK] Saved data/cache/valuations.rds\n")
  } else {
    cat("  [WARN] Valuation produced no results\n")
    valuations <- NULL
  }
}

cat("\n")

# --- Summary ---

cat("=== Pipeline Complete ===\n")
cat(sprintf("Finished: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

cat("Generated artifacts:\n")
if (!is.null(projections) && "proj_pts_per_week" %in% names(projections)) {
  cat(sprintf("  - data/cache/projections.rds (%d players)\n", nrow(projections)))
} else {
  cat("  - data/cache/projections.rds [NOT GENERATED]\n")
}
if (!is.null(valuations)) {
  cat(sprintf("  - data/cache/valuations.rds (%d players)\n", nrow(valuations)))
} else {
  cat("  - data/cache/valuations.rds [NOT GENERATED]\n")
}

cat("\nData loaded:\n")
cat(sprintf("  - Constitution: %s\n", if (!is.null(constitution)) "OK" else "MISSING"))
cat(sprintf("  - Players:      %s\n",
            if (!is.null(players)) sprintf("%d players", nrow(players)) else "MISSING"))
cat(sprintf("  - Rosters:      %s\n",
            if (!is.null(rosters)) sprintf("%d entries", nrow(rosters)) else "MISSING"))
cat(sprintf("  - Standings:    %s\n",
            if (!is.null(standings)) sprintf("%d teams", nrow(standings)) else "MISSING"))
cat(sprintf("  - Stats (bat):  %s\n",
            if (!is.null(stats_batting)) sprintf("%d batters", nrow(stats_batting)) else "MISSING"))
cat(sprintf("  - Stats (pit):  %s\n",
            if (!is.null(stats_pitching)) sprintf("%d pitchers", nrow(stats_pitching)) else "MISSING"))
cat(sprintf("  - Historical:   %d bat + %d pitch seasons\n",
            length(historical_batting), length(historical_pitching)))


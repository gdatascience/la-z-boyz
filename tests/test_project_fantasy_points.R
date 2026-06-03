# Test: project_fantasy_points and apply_statcast_adjustment
# Validates Requirements: 4.5, 4.6, 4.7, 4.8

# Source the required files
source("R/utils/scoring.R")
source("R/analysis/projection_model.R")

cat("=== Testing project_fantasy_points ===\n\n")

# --- Setup: Create scoring weights manually ---
scoring_weights <- list(
 batting = list(
    singles = 1, doubles = 2, triples = 3, hr = 4,
    grand_slam_bonus = 2, cycle = 5, runs = 1, rbi = 1,
    bb = 1, hbp = 1, sb = 2, cs = -1, k_batter = -0.5
  ),
  pitching = list(
    innings = 3, k_pitcher = 0.5, wins = 5, losses = -5,
    saves = 7, holds = 5, quality_starts = 3, complete_games = 2,
    no_hitter = 5, perfect_game = 5, hits_allowed = -1,
    earned_runs = -1, walks_issued = -1, intentional_walks = 1
  )
)

# --- Test 1: Basic batter projection ---
cat("Test 1: Batter fantasy points computation...\n")
batter_proj <- data.frame(
  player_name = "Test Batter",
  player_type = "Batter",
  data_quality = "full",
  projected_pa = 600,
  proj_X1B = 100,
  proj_X2B = 30,
  proj_X3B = 5,
  proj_HR = 25,
  proj_R = 80,
  proj_RBI = 85,
  proj_BB = 60,
  proj_HBP = 5,
  proj_SB = 15,
  proj_CS = 5,
  proj_SO = 120,
  stringsAsFactors = FALSE
)

result <- project_fantasy_points(batter_proj, scoring_weights)

# Check columns added
stopifnot("proj_pts_per_week" %in% names(result))
stopifnot("confidence_lo" %in% names(result))
stopifnot("confidence_hi" %in% names(result))

# Check value is positive
stopifnot(result$proj_pts_per_week > 0)
cat(sprintf("  pts_per_week = %.2f\n", result$proj_pts_per_week))

# Check confidence interval invariant: lo <= pts <= hi
stopifnot(result$confidence_lo <= result$proj_pts_per_week)
stopifnot(result$proj_pts_per_week <= result$confidence_hi)

# Check non-negative bounds
stopifnot(result$confidence_lo >= 0)
stopifnot(result$confidence_hi >= 0)

# Check CI width for "full" data quality = ±15%
expected_lo <- result$proj_pts_per_week * 0.85
expected_hi <- result$proj_pts_per_week * 1.15
stopifnot(abs(result$confidence_lo - expected_lo) < 0.01)
stopifnot(abs(result$confidence_hi - expected_hi) < 0.01)
cat("  PASSED: CI width correct for full data quality (±15%)\n")

# --- Test 2: CI width varies by data quality ---
cat("\nTest 2: CI width by data quality...\n")
proj_limited <- batter_proj
proj_limited$data_quality <- "limited"
result_limited <- project_fantasy_points(proj_limited, scoring_weights)

proj_rookie <- batter_proj
proj_rookie$data_quality <- "rookie"
result_rookie <- project_fantasy_points(proj_rookie, scoring_weights)

ci_full <- result$confidence_hi - result$confidence_lo
ci_limited <- result_limited$confidence_hi - result_limited$confidence_lo
ci_rookie <- result_rookie$confidence_hi - result_rookie$confidence_lo

stopifnot(ci_rookie > ci_limited)
stopifnot(ci_limited > ci_full)
cat(sprintf("  CI widths: full=%.2f, limited=%.2f, rookie=%.2f\n",
            ci_full, ci_limited, ci_rookie))
cat("  PASSED: Wider CI for less data\n")

# --- Test 3: Pitcher projection ---
cat("\nTest 3: Pitcher fantasy points computation...\n")
pitcher_proj <- data.frame(
  player_name = "Test Pitcher",
  player_type = "Pitcher",
  data_quality = "full",
  projected_bf = 750,
  proj_IP = 180,
  proj_SO = 200,
  proj_W = 12,
  proj_L = 8,
  proj_SV = 0,
  proj_H = 150,
  proj_ER = 65,
  proj_BB = 50,
  stringsAsFactors = FALSE
)

result_p <- project_fantasy_points(pitcher_proj, scoring_weights)
stopifnot(result_p$proj_pts_per_week > 0)
stopifnot(result_p$confidence_lo <= result_p$proj_pts_per_week)
stopifnot(result_p$proj_pts_per_week <= result_p$confidence_hi)
cat(sprintf("  Pitcher pts/week = %.2f\n", result_p$proj_pts_per_week))
cat("  PASSED: Pitcher projection with valid CI\n")

# --- Test 4: Statcast adjustment ---
cat("\nTest 4: Statcast adjustment...\n")
batter_with_statcast <- batter_proj
batter_with_statcast$exit_velocity <- 92.0  # above league avg (88.5)
batter_with_statcast$barrel_rate <- 12.0    # above league avg (6.5)
batter_with_statcast$xBA <- 0.280
batter_with_statcast$BA <- 0.260  # underperforming
batter_with_statcast$xSLG <- 0.500
batter_with_statcast$SLG <- 0.470

result_sc <- project_fantasy_points(batter_with_statcast, scoring_weights)

# Player with above-average Statcast data should get a boost
stopifnot(result_sc$proj_pts_per_week > result$proj_pts_per_week)
cat(sprintf("  Without Statcast: %.2f, With Statcast: %.2f\n",
            result$proj_pts_per_week, result_sc$proj_pts_per_week))
cat("  PASSED: Statcast adjustment boosts above-avg player\n")

# --- Test 5: Statcast adjustment capped at ±10% ---
cat("\nTest 5: Statcast adjustment cap...\n")
batter_extreme <- batter_proj
batter_extreme$exit_velocity <- 99.0  # way above average
batter_extreme$barrel_rate <- 20.0
batter_extreme$xBA <- 0.350
batter_extreme$BA <- 0.250
batter_extreme$xSLG <- 0.700
batter_extreme$SLG <- 0.400

result_extreme <- project_fantasy_points(batter_extreme, scoring_weights)

# Cap at +10%
max_expected <- result$proj_pts_per_week * 1.10
stopifnot(result_extreme$proj_pts_per_week <= max_expected + 0.01)
cat(sprintf("  Extreme Statcast: %.2f (max allowed: %.2f)\n",
            result_extreme$proj_pts_per_week, max_expected))
cat("  PASSED: Statcast adjustment capped at +10%%\n")

# --- Test 6: Empty projections handled gracefully ---
cat("\nTest 6: Empty projections...\n")
empty_df <- data.frame()
result_empty <- project_fantasy_points(empty_df, scoring_weights)
stopifnot(nrow(result_empty) == 0)
cat("  PASSED: Empty projections returned unchanged\n")

# --- Test 7: Mixed batter/pitcher projections ---
cat("\nTest 7: Mixed batter + pitcher...\n")
# Build a combined data frame with all columns (NAs for missing)
all_cols <- union(names(batter_proj), names(pitcher_proj))
batter_full <- batter_proj
pitcher_full <- pitcher_proj
for (col in setdiff(all_cols, names(batter_full))) batter_full[[col]] <- NA
for (col in setdiff(all_cols, names(pitcher_full))) pitcher_full[[col]] <- NA
mixed_proj <- rbind(batter_full[, all_cols], pitcher_full[, all_cols])

result_mixed <- project_fantasy_points(mixed_proj, scoring_weights)
stopifnot(nrow(result_mixed) == 2)
stopifnot(all(result_mixed$proj_pts_per_week > 0))
stopifnot(all(result_mixed$confidence_lo >= 0))
stopifnot(all(result_mixed$confidence_lo <= result_mixed$proj_pts_per_week))
stopifnot(all(result_mixed$proj_pts_per_week <= result_mixed$confidence_hi))
cat("  PASSED: Mixed projections computed correctly\n")

cat("\n=== All tests PASSED ===\n")

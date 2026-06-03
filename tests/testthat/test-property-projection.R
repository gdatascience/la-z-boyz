# Property-Based Tests for Projection Model (Properties 6, 7, 8, 9)
#
# **Validates: Requirements 4.2, 4.3, 4.4, 4.7**
#
# Property 6: Projection Recency Weighting — projection closer to recent season
#             than oldest when difference > 20%
# Property 7: Age Curve Monotonicity — age 35 produces lower stats than age 27
#             for same input
# Property 8: Confidence Interval Invariant — lo ≤ projected ≤ hi, both non-negative
# Property 9: Confidence Width vs Data Availability — 1 season data → wider CI
#             than 3 seasons
#
# Uses manual randomization with set.seed() and 120 iterations per property.

library(testthat)

# --- League scoring weights (from constitution) ---

batting_weights <- list(
  singles          = 1,
  doubles          = 2,
  triples          = 3,
  hr               = 4,
  grand_slam_bonus = 2,
  cycle            = 5,
  runs             = 1,
  rbi              = 1,
  bb               = 1,
  hbp              = 1,
  sb               = 2,
  cs               = -1,
  k_batter         = -0.5
)

pitching_weights <- list(
  innings          = 3,
  k_pitcher        = 0.5,
  wins             = 5,
  losses           = -5,
  saves            = 7,
  holds            = 5,
  quality_starts   = 3,
  complete_games   = 2,
  no_hitter        = 5,
  perfect_game     = 5,
  hits_allowed     = -1,
  earned_runs      = -1,
  walks_issued     = -1,
  intentional_walks = 1
)

scoring_weights <- list(batting = batting_weights, pitching = pitching_weights)

# --- Generators ---

#' Generate a random batting season stat line
generate_batting_season <- function(hr_override = NULL, pa_override = NULL) {
  pa <- if (!is.null(pa_override)) pa_override else sample(300:650, 1)
  hr <- if (!is.null(hr_override)) hr_override else sample(5:45, 1)
  list(
    PA  = pa,
    AB  = round(pa * 0.88),
    X1B = sample(60:150, 1),
    X2B = sample(15:45, 1),
    X3B = sample(0:10, 1),
    HR  = hr,
    R   = sample(40:120, 1),
    RBI = sample(30:130, 1),
    BB  = sample(20:100, 1),
    HBP = sample(0:15, 1),
    SB  = sample(0:40, 1),
    CS  = sample(0:10, 1),
    SO  = sample(50:180, 1)
  )
}

#' Generate a random pitching season stat line
generate_pitching_season <- function(bf_override = NULL) {
  bf <- if (!is.null(bf_override)) bf_override else sample(300:900, 1)
  ip <- round(bf / 4.3, 1)
  gs <- sample(10:33, 1)
  list(
    BF  = bf,
    G   = gs + sample(0:5, 1),
    GS  = gs,
    IP  = ip,
    SO  = sample(60:250, 1),
    W   = sample(3:18, 1),
    L   = sample(3:15, 1),
    SV  = sample(0:40, 1),
    H   = sample(80:220, 1),
    ER  = sample(30:100, 1),
    BB  = sample(20:80, 1),
    HR  = sample(5:35, 1),
    HBP = sample(2:12, 1)
  )
}

# --- Property 6: Projection Recency Weighting ---
# For any player with 3 seasons where most recent HR differs from oldest by > 20%,
# the projection HR shall be closer to the most recent season than to the oldest.

test_that("Property 6: Projection recency weighting — projection closer to recent season than oldest", {
  # **Validates: Requirements 4.2**
  set.seed(606)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    # Create 3 seasons with most recent HR=40 and oldest HR=10 (>20% difference)
    recent_hr <- 40
    oldest_hr <- 10

    # Use consistent PA to isolate weighting behavior
    consistent_pa <- sample(450:600, 1)
    season_recent <- generate_batting_season(hr_override = recent_hr, pa_override = consistent_pa)
    season_middle <- generate_batting_season(hr_override = sample(20:30, 1), pa_override = consistent_pa)
    season_oldest <- generate_batting_season(hr_override = oldest_hr, pa_override = consistent_pa)

    # marcel_project expects most recent first
    seasons <- list(season_recent, season_middle, season_oldest)

    # Use regression_pct = 0 to isolate the recency weighting behavior
    result <- marcel_project(seasons, weights = c(5, 4, 3), regression_pct = 0, player_type = "Batter")
    projected_hr <- result$projected_stats["HR"]

    # Projection should be closer to recent (40) than to oldest (10)
    dist_to_recent <- abs(projected_hr - recent_hr)
    dist_to_oldest <- abs(projected_hr - oldest_hr)

    expect_true(
      dist_to_recent < dist_to_oldest,
      info = paste0(
        "Iteration ", i, ": projected HR = ", round(projected_hr, 2),
        " should be closer to recent (", recent_hr,
        ") than oldest (", oldest_hr, "). ",
        "dist_to_recent=", round(dist_to_recent, 2),
        ", dist_to_oldest=", round(dist_to_oldest, 2)
      )
    )
  }
})

test_that("Property 6: Recency weighting holds for pitching SO with randomized large differences", {
  # **Validates: Requirements 4.2**
  set.seed(607)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    # Random recent and oldest SO with > 20% difference
    recent_so <- sample(180:280, 1)
    oldest_so <- sample(60:120, 1)

    # Verify > 20% difference
    if (abs(recent_so - oldest_so) / max(recent_so, oldest_so) <= 0.20) next

    # Middle season between recent and oldest to properly test recency weighting
    middle_so <- round((recent_so + oldest_so) / 2)

    # Use consistent BF across seasons to isolate weighting behavior
    consistent_bf <- sample(500:800, 1)
    season_recent <- generate_pitching_season(bf_override = consistent_bf)
    season_recent$SO <- recent_so
    season_middle <- generate_pitching_season(bf_override = consistent_bf)
    season_middle$SO <- middle_so
    season_oldest <- generate_pitching_season(bf_override = consistent_bf)
    season_oldest$SO <- oldest_so

    seasons <- list(season_recent, season_middle, season_oldest)

    # Use regression_pct = 0 to isolate the recency weighting behavior
    result <- marcel_project(seasons, weights = c(5, 4, 3), regression_pct = 0, player_type = "Pitcher")
    projected_so <- result$projected_stats["SO"]

    dist_to_recent <- abs(projected_so - recent_so)
    dist_to_oldest <- abs(projected_so - oldest_so)

    expect_true(
      dist_to_recent < dist_to_oldest,
      info = paste0(
        "Iteration ", i, ": projected SO = ", round(projected_so, 2),
        " should be closer to recent (", recent_so,
        ") than oldest (", oldest_so, ")"
      )
    )
  }
})

# --- Property 7: Age Curve Monotonicity ---
# For any identical stat line, age 35 produces lower stats than age 27 (peak).

test_that("Property 7: Age curve monotonicity — age 35 lower than age 27 for batters", {
  # **Validates: Requirements 4.4**
  set.seed(707)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    # Generate a random stat vector (positive counting stats)
    stats <- c(
      X1B = sample(60:150, 1),
      X2B = sample(15:45, 1),
      X3B = sample(0:10, 1),
      HR  = sample(5:45, 1),
      R   = sample(40:120, 1),
      RBI = sample(30:130, 1),
      BB  = sample(20:100, 1),
      HBP = sample(0:15, 1),
      SB  = sample(0:40, 1),
      CS  = sample(0:10, 1),
      SO  = sample(50:180, 1)
    )

    adjusted_27 <- apply_age_curve(stats, age = 27, position = "1B")
    adjusted_35 <- apply_age_curve(stats, age = 35, position = "1B")

    # All stats at age 35 should be <= stats at age 27
    for (stat_name in names(stats)) {
      expect_true(
        adjusted_35[stat_name] <= adjusted_27[stat_name],
        info = paste0(
          "Iteration ", i, ": batter stat '", stat_name,
          "' at age 35 (", round(adjusted_35[stat_name], 2),
          ") should be <= at age 27 (", round(adjusted_27[stat_name], 2), ")"
        )
      )
    }
  }
})

test_that("Property 7: Age curve monotonicity — age 35 lower than age 28 for pitchers", {
  # **Validates: Requirements 4.4**
  set.seed(708)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    stats <- c(
      IP  = sample(100:220, 1),
      SO  = sample(80:260, 1),
      W   = sample(5:18, 1),
      L   = sample(3:15, 1),
      SV  = sample(0:40, 1),
      H   = sample(80:220, 1),
      ER  = sample(30:100, 1),
      BB  = sample(20:80, 1),
      HR  = sample(5:35, 1),
      HBP = sample(2:12, 1)
    )

    # Pitcher peak is 28
    adjusted_28 <- apply_age_curve(stats, age = 28, position = "SP")
    adjusted_35 <- apply_age_curve(stats, age = 35, position = "SP")

    for (stat_name in names(stats)) {
      expect_true(
        adjusted_35[stat_name] <= adjusted_28[stat_name],
        info = paste0(
          "Iteration ", i, ": pitcher stat '", stat_name,
          "' at age 35 (", round(adjusted_35[stat_name], 2),
          ") should be <= at age 28 (", round(adjusted_28[stat_name], 2), ")"
        )
      )
    }
  }
})

# --- Property 8: Confidence Interval Invariant ---
# confidence_lo <= proj_pts_per_week <= confidence_hi, both >= 0

test_that("Property 8: Confidence interval invariant — lo <= projected <= hi, both non-negative (batters)", {
  # **Validates: Requirements 4.7**
  set.seed(808)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    # Build a minimal projections data frame for a batter
    season <- generate_batting_season()
    proj <- marcel_project(list(season), player_type = "Batter")

    # Determine data quality from number of seasons
    n_seasons <- sample(1:3, 1)
    data_quality <- if (n_seasons >= 3) "full" else if (n_seasons == 2) "limited" else "rookie"

    proj_df <- data.frame(
      player_name = paste0("Player_", i),
      player_type = "Batter",
      data_quality = data_quality,
      projected_pa = proj$projected_pa,
      proj_X1B = proj$projected_stats["X1B"],
      proj_X2B = proj$projected_stats["X2B"],
      proj_X3B = proj$projected_stats["X3B"],
      proj_HR  = proj$projected_stats["HR"],
      proj_R   = proj$projected_stats["R"],
      proj_RBI = proj$projected_stats["RBI"],
      proj_BB  = proj$projected_stats["BB"],
      proj_HBP = proj$projected_stats["HBP"],
      proj_SB  = proj$projected_stats["SB"],
      proj_CS  = proj$projected_stats["CS"],
      proj_SO  = proj$projected_stats["SO"],
      stringsAsFactors = FALSE
    )

    result <- project_fantasy_points(proj_df, scoring_weights = scoring_weights)

    expect_true(
      result$confidence_lo[1] <= result$proj_pts_per_week[1],
      info = paste0(
        "Iteration ", i, ": confidence_lo (", round(result$confidence_lo[1], 2),
        ") should be <= proj_pts_per_week (", round(result$proj_pts_per_week[1], 2), ")"
      )
    )
    expect_true(
      result$proj_pts_per_week[1] <= result$confidence_hi[1],
      info = paste0(
        "Iteration ", i, ": proj_pts_per_week (", round(result$proj_pts_per_week[1], 2),
        ") should be <= confidence_hi (", round(result$confidence_hi[1], 2), ")"
      )
    )
    expect_true(
      result$confidence_lo[1] >= 0,
      info = paste0("Iteration ", i, ": confidence_lo (", result$confidence_lo[1], ") must be >= 0")
    )
    expect_true(
      result$confidence_hi[1] >= 0,
      info = paste0("Iteration ", i, ": confidence_hi (", result$confidence_hi[1], ") must be >= 0")
    )
  }
})

test_that("Property 8: Confidence interval invariant — lo <= projected <= hi, both non-negative (pitchers)", {
  # **Validates: Requirements 4.7**
  set.seed(809)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    season <- generate_pitching_season()
    proj <- marcel_project(list(season), player_type = "Pitcher")

    data_quality <- sample(c("full", "limited", "rookie"), 1)

    proj_df <- data.frame(
      player_name = paste0("Pitcher_", i),
      player_type = "Pitcher",
      data_quality = data_quality,
      projected_bf = proj$projected_pa,
      proj_IP  = proj$projected_stats["IP"],
      proj_SO  = proj$projected_stats["SO"],
      proj_W   = proj$projected_stats["W"],
      proj_L   = proj$projected_stats["L"],
      proj_SV  = proj$projected_stats["SV"],
      proj_H   = proj$projected_stats["H"],
      proj_ER  = proj$projected_stats["ER"],
      proj_BB  = proj$projected_stats["BB"],
      proj_HR  = proj$projected_stats["HR"],
      proj_HBP = proj$projected_stats["HBP"],
      stringsAsFactors = FALSE
    )

    result <- project_fantasy_points(proj_df, scoring_weights = scoring_weights)

    expect_true(
      result$confidence_lo[1] <= result$proj_pts_per_week[1],
      info = paste0(
        "Iteration ", i, ": pitcher confidence_lo (", round(result$confidence_lo[1], 2),
        ") should be <= proj_pts_per_week (", round(result$proj_pts_per_week[1], 2), ")"
      )
    )
    expect_true(
      result$proj_pts_per_week[1] <= result$confidence_hi[1],
      info = paste0(
        "Iteration ", i, ": pitcher proj_pts_per_week (", round(result$proj_pts_per_week[1], 2),
        ") should be <= confidence_hi (", round(result$confidence_hi[1], 2), ")"
      )
    )
    expect_true(
      result$confidence_lo[1] >= 0,
      info = paste0("Iteration ", i, ": pitcher confidence_lo must be >= 0")
    )
    expect_true(
      result$confidence_hi[1] >= 0,
      info = paste0("Iteration ", i, ": pitcher confidence_hi must be >= 0")
    )
  }
})

# --- Property 9: Confidence Width vs Data Availability ---
# 1 season data → wider CI than 3 seasons

test_that("Property 9: Confidence width — 1 season (rookie) wider than 3 seasons (full)", {
  # **Validates: Requirements 4.3**
  set.seed(909)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    # Same base stats for both projections
    season <- generate_batting_season()
    proj <- marcel_project(list(season), player_type = "Batter")

    # Rookie (1 season) projection
    proj_rookie <- data.frame(
      player_name = "Rookie",
      player_type = "Batter",
      data_quality = "rookie",
      projected_pa = proj$projected_pa,
      proj_X1B = proj$projected_stats["X1B"],
      proj_X2B = proj$projected_stats["X2B"],
      proj_X3B = proj$projected_stats["X3B"],
      proj_HR  = proj$projected_stats["HR"],
      proj_R   = proj$projected_stats["R"],
      proj_RBI = proj$projected_stats["RBI"],
      proj_BB  = proj$projected_stats["BB"],
      proj_HBP = proj$projected_stats["HBP"],
      proj_SB  = proj$projected_stats["SB"],
      proj_CS  = proj$projected_stats["CS"],
      proj_SO  = proj$projected_stats["SO"],
      stringsAsFactors = FALSE
    )

    # Full (3 seasons) projection — same stats, different data_quality
    proj_full <- proj_rookie
    proj_full$player_name <- "Veteran"
    proj_full$data_quality <- "full"

    result_rookie <- project_fantasy_points(proj_rookie, scoring_weights = scoring_weights)
    result_full <- project_fantasy_points(proj_full, scoring_weights = scoring_weights)

    ci_width_rookie <- result_rookie$confidence_hi[1] - result_rookie$confidence_lo[1]
    ci_width_full <- result_full$confidence_hi[1] - result_full$confidence_lo[1]

    expect_true(
      ci_width_rookie > ci_width_full,
      info = paste0(
        "Iteration ", i, ": rookie CI width (", round(ci_width_rookie, 2),
        ") should be wider than full CI width (", round(ci_width_full, 2), ")"
      )
    )
  }
})

test_that("Property 9: Confidence width — pitchers with 1 season wider than 3 seasons", {
  # **Validates: Requirements 4.3**
  set.seed(910)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    season <- generate_pitching_season()
    proj <- marcel_project(list(season), player_type = "Pitcher")

    proj_rookie <- data.frame(
      player_name = "Rookie_P",
      player_type = "Pitcher",
      data_quality = "rookie",
      projected_bf = proj$projected_pa,
      proj_IP  = proj$projected_stats["IP"],
      proj_SO  = proj$projected_stats["SO"],
      proj_W   = proj$projected_stats["W"],
      proj_L   = proj$projected_stats["L"],
      proj_SV  = proj$projected_stats["SV"],
      proj_H   = proj$projected_stats["H"],
      proj_ER  = proj$projected_stats["ER"],
      proj_BB  = proj$projected_stats["BB"],
      proj_HR  = proj$projected_stats["HR"],
      proj_HBP = proj$projected_stats["HBP"],
      stringsAsFactors = FALSE
    )

    proj_full <- proj_rookie
    proj_full$player_name <- "Veteran_P"
    proj_full$data_quality <- "full"

    result_rookie <- project_fantasy_points(proj_rookie, scoring_weights = scoring_weights)
    result_full <- project_fantasy_points(proj_full, scoring_weights = scoring_weights)

    ci_width_rookie <- result_rookie$confidence_hi[1] - result_rookie$confidence_lo[1]
    ci_width_full <- result_full$confidence_hi[1] - result_full$confidence_lo[1]

    expect_true(
      ci_width_rookie > ci_width_full,
      info = paste0(
        "Iteration ", i, ": pitcher rookie CI width (", round(ci_width_rookie, 2),
        ") should be wider than full CI width (", round(ci_width_full, 2), ")"
      )
    )
  }
})

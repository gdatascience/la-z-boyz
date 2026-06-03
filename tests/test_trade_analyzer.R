#!/usr/bin/env Rscript
#' Unit tests for R/analysis/trade_analyzer.R
#'
#' Tests core trade evaluation and competitive context assessment.

# Source dependencies
source("R/utils/salary_rules.R")
source("R/utils/keeper_value.R")
source("R/analysis/trade_analyzer.R")

cat("=== Trade Analyzer Unit Tests ===\n\n")

# --- Test fixtures ---

# Mock roster data
mock_rosters <- data.frame(

team_name = c(rep("My Team", 5), rep("Other Team", 3)),
  player_name = c("Player A", "Player B", "Player C", "Player D", "Player E",
                  "Player X", "Player Y", "Player Z"),
  salary = c(20, 15, 10, 5, 3, 25, 12, 8),
  player_type = c("Batter", "Batter", "Pitcher", "Batter", "Pitcher",
                  "Batter", "Pitcher", "Batter"),
  position = c("SS", "OF", "SP", "1B", "RP", "OF", "SP", "3B"),
  stringsAsFactors = FALSE
)

# Mock valuations
mock_valuations <- data.frame(
  player_name = c("Player A", "Player B", "Player C", "Player D", "Player E",
                  "Player X", "Player Y", "Player Z"),
  dollar_value = c(30, 20, 18, 8, 5, 35, 22, 12),
  surplus_value = c(10, 5, 8, 3, 2, 10, 10, 4),
  position = c("SS", "OF", "SP", "1B", "RP", "OF", "SP", "3B"),
  player_type = c("Batter", "Batter", "Pitcher", "Batter", "Pitcher",
                  "Batter", "Pitcher", "Batter"),
  proj_pts_per_week = c(25, 18, 20, 10, 8, 28, 22, 14),
  stringsAsFactors = FALSE
)

# Mock standings
mock_standings <- data.frame(
  team_name = c("My Team", "Other Team", "Third Team", "Fourth Team",
                "Fifth Team", "Sixth Team", "Seventh Team", "Eighth Team",
                "Ninth Team", "Tenth Team", "Eleventh Team", "Twelfth Team",
                "Thirteenth Team", "Fourteenth Team", "Fifteenth Team", "Last Team"),
  division = c(rep("East Division", 4), rep("West Division", 4),
               rep("North Division", 4), rep("South Division", 4)),
  wins = c(15, 12, 10, 8, 14, 11, 9, 7, 13, 10, 8, 6, 11, 9, 7, 5),
  losses = c(5, 8, 10, 12, 6, 9, 11, 13, 7, 10, 12, 14, 9, 11, 13, 15),
  ties = rep(0, 16),
  pct = c(.750, .600, .500, .400, .700, .550, .450, .350,
          .650, .500, .400, .300, .550, .450, .350, .250),
  games_back = c(0, 3, 5, 7, 0, 3, 5, 7, 0, 3, 5, 7, 0, 3, 5, 7),
  total_points = c(1500, 1200, 1000, 800, 1400, 1100, 900, 700,
                   1300, 1000, 800, 600, 1100, 900, 700, 500),
  streak = rep("W1", 16),
  div_record = rep("5-3", 16),
  magic_number = rep(NA_character_, 16),
  points_behind_leader = c(0, 300, 500, 700, 100, 400, 600, 800,
                           200, 500, 700, 900, 400, 600, 800, 1000),
  points_against = rep(1000, 16),
  games_played = c(20, 20, 20, 20, 20, 20, 20, 20,
                   20, 20, 20, 20, 20, 20, 20, 20),
  ppg = c(75, 60, 50, 40, 70, 55, 45, 35, 65, 50, 40, 30, 55, 45, 35, 25),
  stringsAsFactors = FALSE
)

# --- Test 1: Basic trade analysis ---
cat("Test 1: Basic trade analysis works...\n")
result <- analyze_trade(
  give = "Player A",
  receive = "Player X",
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings
)

stopifnot(is.list(result))
stopifnot("summary" %in% names(result))
stopifnot("value_diff" %in% names(result))
stopifnot("salary_impact" %in% names(result))
stopifnot("positional_impact" %in% names(result))
stopifnot("recommendation" %in% names(result))
cat("  PASS: Returns correct structure\n")

# --- Test 2: Net surplus value difference ---
cat("Test 2: Net surplus value computed correctly...\n")
# Player X surplus = 10, Player A surplus = 10 -> diff = 0
stopifnot(result$value_diff == 0)
cat("  PASS: value_diff = 0 (equal surplus)\n")

# --- Test 3: Salary impact computation ---
cat("Test 3: Salary cap impact computed correctly...\n")
# My Team total salary = 20+15+10+5+3 = 53
# Give Player A (salary=20), Receive Player X (salary=25)
# Net change = 25 - 20 = +5
# New total = 53 + 5 = 58
stopifnot(result$salary_impact$net_change == 5)
stopifnot(result$salary_impact$new_total == 58)
stopifnot(result$salary_impact$cap_compliant == TRUE)
cat("  PASS: salary impact correct (net +5, total 58, compliant)\n")

# --- Test 4: Player not found rejection (Requirement 6.10) ---
cat("Test 4: Rejects trade with unknown player...\n")
tryCatch(
  {
    analyze_trade(
      give = "Nonexistent Player",
      receive = "Player X",
      my_team = "My Team",
      valuations = mock_valuations,
      rosters = mock_rosters,
      standings = mock_standings
    )
    stop("Should have raised an error")
  },
  error = function(e) {
    stopifnot(grepl("not found in roster data", e$message))
    stopifnot(grepl("Nonexistent Player", e$message))
    cat("  PASS: Correctly rejects unknown player\n")
  }
)

# --- Test 5: Competitive context - contender ---
cat("Test 5: assess_competitive_context identifies contender...\n")
ctx <- assess_competitive_context("My Team", mock_standings)
stopifnot(ctx$mode == "contend")
stopifnot(ctx$games_back == 0)
stopifnot(ctx$division_rank == 1)
stopifnot(ctx$playoff_prob > 0.6)
cat(sprintf("  PASS: mode=%s, games_back=%.0f, rank=%d, prob=%.3f\n",
            ctx$mode, ctx$games_back, ctx$division_rank, ctx$playoff_prob))

# --- Test 6: Competitive context - rebuilder ---
cat("Test 6: assess_competitive_context identifies rebuilder...\n")
ctx_rebuild <- assess_competitive_context("Last Team", mock_standings)
stopifnot(ctx_rebuild$mode == "rebuild")
stopifnot(ctx_rebuild$games_back == 7)
stopifnot(ctx_rebuild$division_rank == 4)
stopifnot(ctx_rebuild$playoff_prob <= 0.25)
cat(sprintf("  PASS: mode=%s, games_back=%.0f, rank=%d, prob=%.3f\n",
            ctx_rebuild$mode, ctx_rebuild$games_back, ctx_rebuild$division_rank,
            ctx_rebuild$playoff_prob))

# --- Test 7: Competitive context - team not found ---
cat("Test 7: assess_competitive_context handles missing team...\n")
ctx_missing <- assess_competitive_context("Ghost Team", mock_standings)
stopifnot(ctx_missing$mode == "middle")
stopifnot(ctx_missing$playoff_prob == 0.375)
cat("  PASS: Returns default 'middle' context for unknown team\n")

# --- Test 8: Multi-player trade ---
cat("Test 8: Multi-player trade works...\n")
result_multi <- analyze_trade(
  give = c("Player A", "Player B"),
  receive = c("Player X", "Player Z"),
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings
)
# Give surplus: 10 + 5 = 15; Receive surplus: 10 + 4 = 14; diff = -1
stopifnot(result_multi$value_diff == -1)
cat(sprintf("  PASS: Multi-player value_diff = %.1f\n", result_multi$value_diff))

# --- Test 9: Salary cap violation ---
cat("Test 9: Trade that violates salary cap...\n")
# Create a roster with salary near cap
high_salary_rosters <- mock_rosters
high_salary_rosters$salary[high_salary_rosters$team_name == "My Team"] <- c(60, 60, 60, 60, 55)
# Total = 295. Receiving Player X (salary=25) and giving Player A (salary=60)
# Net change = 25 - 60 = -35. New total = 260. Still compliant.
# Let's make it exceed by giving low salary player
high_salary_rosters$salary[high_salary_rosters$team_name == "My Team"] <- c(5, 60, 60, 60, 55)
# Total = 240. Give Player A (salary=5), receive Player X (salary=25)
# Net = 25 - 5 = +20. New total = 260. Still compliant.
# Push it over:
high_salary_rosters$salary[high_salary_rosters$team_name == "My Team"] <- c(5, 80, 80, 80, 55)
# Total = 300. Give Player A (salary=5), receive Player X (salary=25)
# Net = 25 - 5 = +20. New total = 320. VIOLATION!
result_cap <- analyze_trade(
  give = "Player A",
  receive = "Player X",
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = high_salary_rosters,
  standings = mock_standings
)
stopifnot(result_cap$salary_impact$cap_compliant == FALSE)
stopifnot(result_cap$recommendation == "reject")
cat("  PASS: Cap violation detected, recommendation = reject\n")

# --- Test 10: Recommendation is one of accept/reject/counter ---
cat("Test 10: Recommendation is valid value...\n")
stopifnot(result$recommendation %in% c("accept", "reject", "counter"))
cat(sprintf("  PASS: recommendation = '%s'\n", result$recommendation))

# --- Test 11: Pts/week change with batting/pitching breakdown ---
cat("Test 11: Points per week change breakdown...\n")
result_pts <- analyze_trade(
  give = "Player C",  # Pitcher, 20 pts/wk
  receive = "Player Y",  # Pitcher, 22 pts/wk
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings
)
# Both pitchers: batting change = 0, pitching change = 22 - 20 = 2
cat(sprintf("  Summary contains pts/week info: %s\n",
            grepl("Pts/Week", result_pts$summary)))
stopifnot(grepl("Pts/Week", result_pts$summary))
cat("  PASS: Points breakdown included in summary\n")

# --- Test 12: Empty standings handling ---
cat("Test 12: Handles empty standings gracefully...\n")
ctx_empty <- assess_competitive_context("My Team", data.frame())
stopifnot(ctx_empty$mode == "middle")
cat("  PASS: Returns default for empty standings\n")

cat("\n=== All Trade Analyzer Tests PASSED ===\n")

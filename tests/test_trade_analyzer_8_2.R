#!/usr/bin/env Rscript
#' Unit tests for trade_analyzer.R — Task 8.2 additions
#'
#' Tests: analyze_keeper_implications, analyze_positional_impact,
#'        analyze_prospect_value, is_trade_lopsided, and updated analyze_trade.

# Source dependencies
source("R/utils/salary_rules.R")
source("R/utils/keeper_value.R")
source("R/analysis/trade_analyzer.R")

cat("=== Trade Analyzer 8.2 Unit Tests ===\n\n")

# --- Test fixtures ---

mock_rosters <- data.frame(
  team_name = c(rep("My Team", 6), rep("Other Team", 3)),
  player_name = c("Player A", "Player B", "Player C", "Player D",
                  "Player E", "Prospect F",
                  "Player X", "Player Y", "Player Z"),
  salary = c(20, 15, 10, 5, 3, 0, 25, 12, 8),
  player_type = c("Batter", "Batter", "Pitcher", "Batter", "Pitcher", "Batter",
                  "Batter", "Pitcher", "Batter"),
  position = c("SS", "OF", "SP", "1B", "RP", "OF", "OF", "SP", "3B"),
  is_minor_contract = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                         FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

mock_valuations <- data.frame(
  player_name = c("Player A", "Player B", "Player C", "Player D",
                  "Player E", "Prospect F",
                  "Player X", "Player Y", "Player Z"),
  dollar_value = c(30, 20, 18, 8, 5, 3, 35, 22, 12),
  surplus_value = c(10, 5, 8, 3, 2, 3, 10, 10, 4),
  position = c("SS", "OF", "SP", "1B", "RP", "OF", "OF", "SP", "3B"),
  player_type = c("Batter", "Batter", "Pitcher", "Batter", "Pitcher", "Batter",
                  "Batter", "Pitcher", "Batter"),
  proj_pts_per_week = c(25, 18, 20, 10, 8, 4, 28, 22, 14),
  stringsAsFactors = FALSE
)

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
  games_played = rep(20L, 16),
  ppg = c(75, 60, 50, 40, 70, 55, 45, 35, 65, 50, 40, 30, 55, 45, 35, 25),
  stringsAsFactors = FALSE
)

mock_draft_data <- data.frame(
  player_name = c("Prospect F", "Some Other Guy", "Late Round Pick"),
  round = c(1, 3, 10),
  pick_number = c(5, 85, 300),
  signing_bonus = c(5000000, 500000, 100000),
  school = c("Vanderbilt", "LSU", "Small College"),
  year = c(2024, 2024, 2024),
  stringsAsFactors = FALSE
)

# ============================================================
# Test 13: analyze_keeper_implications basic operation
# ============================================================
cat("Test 13: analyze_keeper_implications returns correct structure...\n")
give_vals <- mock_valuations[mock_valuations$player_name == "Player A", ]
receive_vals <- mock_valuations[mock_valuations$player_name == "Player X", ]

keeper_result <- analyze_keeper_implications(give_vals, receive_vals, mock_rosters)

stopifnot(is.list(keeper_result))
stopifnot("give" %in% names(keeper_result))
stopifnot("receive" %in% names(keeper_result))
stopifnot("Player A" %in% names(keeper_result$give))
stopifnot("Player X" %in% names(keeper_result$receive))
cat("  PASS: Returns give/receive structure with player names\n")

# ============================================================
# Test 14: Keeper salary escalation path is correct
# ============================================================
cat("Test 14: Keeper escalation path computed correctly...\n")
pa_keeper <- keeper_result$give[["Player A"]]
# Player A: salary = 20, standard contract
# Escalation: 24, 28, 32 (salary + 4*N)
stopifnot(pa_keeper$current_salary == 20)
stopifnot(all(pa_keeper$escalation_path == c(24, 28, 32)))
stopifnot(pa_keeper$is_minor_contract == FALSE)
stopifnot(pa_keeper$keeper_eligible == TRUE)
cat("  PASS: Player A escalation = 24, 28, 32\n")

# ============================================================
# Test 15: Keeper value 3-year NPV is computed
# ============================================================
cat("Test 15: Keeper 3-year NPV is numeric...\n")
stopifnot(is.numeric(pa_keeper$keeper_value_3yr))
stopifnot(length(pa_keeper$annual_surplus) == 3)
cat(sprintf("  PASS: keeper_value_3yr = %.2f\n", pa_keeper$keeper_value_3yr))

# ============================================================
# Test 16: Keeper eligibility — post-deadline FAAB flag
# ============================================================
cat("Test 16: Keeper eligibility flag for post-deadline FAAB...\n")
# Add acquisition_type to test
rosters_with_acq <- mock_rosters
rosters_with_acq$acquisition_type <- NA_character_
rosters_with_acq$acquisition_type[rosters_with_acq$player_name == "Player X"] <- "faab_post_deadline"

keeper_acq <- analyze_keeper_implications(give_vals, receive_vals, rosters_with_acq)
stopifnot(keeper_acq$receive[["Player X"]]$keeper_eligible == FALSE)
cat("  PASS: Post-deadline FAAB player correctly flagged as not keeper-eligible\n")

# ============================================================
# Test 17: analyze_positional_impact basic operation
# ============================================================
cat("Test 17: analyze_positional_impact identifies position changes...\n")
roster_slots <- c(C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
                  OF = 3, U = 1, SP = 5, RP = 2)

# Give Player A (SS), receive Player X (OF)
give_v <- mock_valuations[mock_valuations$player_name == "Player A", ]
recv_v <- mock_valuations[mock_valuations$player_name == "Player X", ]
my_roster <- mock_rosters[mock_rosters$team_name == "My Team", ]

pos_result <- analyze_positional_impact(give_v, recv_v, my_roster, roster_slots)
stopifnot(is.list(pos_result))
stopifnot("improved" %in% names(pos_result))
stopifnot("weakened" %in% names(pos_result))
stopifnot("overfilled" %in% names(pos_result))
stopifnot("unfilled" %in% names(pos_result))
stopifnot("details" %in% names(pos_result))

# SS should be weakened (losing 1), OF should be improved (gaining 1)
stopifnot("SS" %in% pos_result$weakened)
stopifnot("OF" %in% pos_result$improved)
cat("  PASS: SS weakened, OF improved correctly identified\n")

# ============================================================
# Test 18: Positional impact — unfilled detection
# ============================================================
cat("Test 18: Positional impact detects unfilled positions...\n")
# If we only have 1 SS (Player A) and we trade them away, SS should be unfilled
# My roster has: SS=1 (Player A), OF=2 (Player B, Prospect F), SP=1, 1B=1, RP=1
# Trading away Player A (SS) means SS goes from 1 -> 0 = unfilled
stopifnot("SS" %in% pos_result$unfilled)
cat("  PASS: SS correctly detected as unfilled after trade\n")

# ============================================================
# Test 19: analyze_prospect_value with draft data
# ============================================================
cat("Test 19: analyze_prospect_value returns prospect profile...\n")
prospect <- analyze_prospect_value("Prospect F", mock_rosters, mock_draft_data)

stopifnot(is.list(prospect))
stopifnot(prospect$player_name == "Prospect F")
stopifnot(prospect$salary_track$is_minor_contract == TRUE)
stopifnot(prospect$salary_track$current_salary == 0)
stopifnot(prospect$prospect_tier == "elite")  # Round 1, pick 5
stopifnot(!is.null(prospect$draft_pedigree))
stopifnot(prospect$draft_pedigree$round == 1)
stopifnot(prospect$draft_pedigree$pick == 5)
stopifnot(prospect$draft_pedigree$signing_bonus == 5000000)
stopifnot(is.numeric(prospect$keeper_value_5yr))
cat(sprintf("  PASS: Prospect F tier=%s, 5yr_keeper_val=%.2f\n",
            prospect$prospect_tier, prospect$keeper_value_5yr))

# ============================================================
# Test 20: analyze_prospect_value without draft data
# ============================================================
cat("Test 20: analyze_prospect_value with no draft data defaults to 'low'...\n")
prospect_no_draft <- analyze_prospect_value("Prospect F", mock_rosters, NULL)
stopifnot(prospect_no_draft$prospect_tier == "low")
stopifnot(is.null(prospect_no_draft$draft_pedigree))
cat("  PASS: No draft data → tier=low, pedigree=NULL\n")

# ============================================================
# Test 21: Prospect tier classification
# ============================================================
cat("Test 21: classify_prospect_tier correctly categorizes...\n")
stopifnot(classify_prospect_tier(NULL) == "low")
stopifnot(classify_prospect_tier(list(round = 1, pick = 3, signing_bonus = 6e6)) == "elite")
stopifnot(classify_prospect_tier(list(round = 1, pick = 20, signing_bonus = 3e6)) == "high")
stopifnot(classify_prospect_tier(list(round = 2, pick = 50, signing_bonus = 1e6)) == "high")
stopifnot(classify_prospect_tier(list(round = 4, pick = 120, signing_bonus = 400000)) == "mid")
stopifnot(classify_prospect_tier(list(round = 8, pick = 240, signing_bonus = 100000)) == "low")
# High bonus regardless of round
stopifnot(classify_prospect_tier(list(round = 7, pick = 200, signing_bonus = 2000000)) == "high")
cat("  PASS: All tier classifications correct\n")

# ============================================================
# Test 22: is_trade_lopsided — even trade
# ============================================================
cat("Test 22: is_trade_lopsided identifies fair trades...\n")
# Player A: dollar_value=30, surplus=10
# Player X: dollar_value=35, surplus=10
# Total value = 30 + 35 = 65, net surplus diff = |10-10| = 0
# 0 / 65 = 0% < 20% → NOT lopsided
even_give <- mock_valuations[mock_valuations$player_name == "Player A", ]
even_recv <- mock_valuations[mock_valuations$player_name == "Player X", ]
stopifnot(is_trade_lopsided(even_give, even_recv) == FALSE)
cat("  PASS: Equal surplus trade is NOT lopsided\n")

# ============================================================
# Test 23: is_trade_lopsided — lopsided trade
# ============================================================
cat("Test 23: is_trade_lopsided flags unbalanced trades...\n")
# Create a clearly lopsided scenario
lopsided_give <- data.frame(
  player_name = "Scrub", dollar_value = 5, surplus_value = -5,
  position = "OF", player_type = "Batter", proj_pts_per_week = 3,
  stringsAsFactors = FALSE
)
lopsided_recv <- data.frame(
  player_name = "Star", dollar_value = 40, surplus_value = 25,
  position = "SS", player_type = "Batter", proj_pts_per_week = 30,
  stringsAsFactors = FALSE
)
# Total value = 5 + 40 = 45, net surplus diff = |25 - (-5)| = 30
# 30 / 45 = 66.7% > 20% → LOPSIDED
stopifnot(is_trade_lopsided(lopsided_give, lopsided_recv) == TRUE)
cat("  PASS: Lopsided trade correctly flagged\n")

# ============================================================
# Test 24: is_trade_lopsided — boundary (exactly 20%)
# ============================================================
cat("Test 24: is_trade_lopsided boundary behavior...\n")
# Design: total_value = 100, net_surplus_diff = 20 → exactly 20% → NOT lopsided (> not >=)
boundary_give <- data.frame(
  player_name = "B1", dollar_value = 50, surplus_value = 10,
  position = "OF", player_type = "Batter", proj_pts_per_week = 15,
  stringsAsFactors = FALSE
)
boundary_recv <- data.frame(
  player_name = "B2", dollar_value = 50, surplus_value = 30,
  position = "OF", player_type = "Batter", proj_pts_per_week = 20,
  stringsAsFactors = FALSE
)
# Total value = 50 + 50 = 100, net diff = |30-10| = 20
# 20 / 100 = 20% exactly → NOT lopsided (strictly >)
stopifnot(is_trade_lopsided(boundary_give, boundary_recv) == FALSE)
cat("  PASS: Exactly 20% is NOT flagged (strict >)\n")

# ============================================================
# Test 25: Updated analyze_trade includes new fields
# ============================================================
cat("Test 25: analyze_trade returns all new fields...\n")
full_result <- analyze_trade(
  give = "Player A",
  receive = "Player X",
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings,
  draft_data = mock_draft_data
)

# Check new fields exist
stopifnot("keeper_analysis" %in% names(full_result))
stopifnot("positional_impact" %in% names(full_result))
stopifnot("prospect_analysis" %in% names(full_result))
stopifnot("is_lopsided" %in% names(full_result))
stopifnot("justification" %in% names(full_result))

# Validate types
stopifnot(is.list(full_result$keeper_analysis))
stopifnot(is.list(full_result$positional_impact))
stopifnot(is.list(full_result$prospect_analysis))
stopifnot(is.logical(full_result$is_lopsided))
stopifnot(is.character(full_result$justification))
cat("  PASS: All new fields present with correct types\n")

# ============================================================
# Test 26: Trade with minor league player triggers prospect analysis
# ============================================================
cat("Test 26: Minor league player triggers prospect analysis...\n")
result_prospect <- analyze_trade(
  give = "Prospect F",
  receive = "Player Z",
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings,
  draft_data = mock_draft_data
)

stopifnot(length(result_prospect$prospect_analysis) > 0)
stopifnot("Prospect F" %in% names(result_prospect$prospect_analysis))
stopifnot(result_prospect$prospect_analysis[["Prospect F"]]$prospect_tier == "elite")
cat("  PASS: Prospect F analyzed with tier=elite\n")

# ============================================================
# Test 27: Justification text includes key dimensions
# ============================================================
cat("Test 27: Justification includes multi-dimensional reasoning...\n")
justification <- full_result$justification
stopifnot(grepl("surplus", justification, ignore.case = TRUE))
stopifnot(grepl("salary", justification, ignore.case = TRUE) ||
          grepl("cap", justification, ignore.case = TRUE))
stopifnot(grepl("mode", justification, ignore.case = TRUE) ||
          grepl("playoff", justification, ignore.case = TRUE))
cat("  PASS: Justification covers value, salary, and strategy\n")

# ============================================================
# Test 28: analyze_trade backward compatibility (no new optional args)
# ============================================================
cat("Test 28: analyze_trade still works without new optional args...\n")
compat_result <- analyze_trade(
  give = "Player B",
  receive = "Player Y",
  my_team = "My Team",
  valuations = mock_valuations,
  rosters = mock_rosters,
  standings = mock_standings
)
stopifnot(is.list(compat_result))
stopifnot("recommendation" %in% names(compat_result))
stopifnot(compat_result$recommendation %in% c("accept", "reject", "counter"))
cat(sprintf("  PASS: Backward compatible, recommendation = '%s'\n",
            compat_result$recommendation))

cat("\n=== All Trade Analyzer 8.2 Tests PASSED ===\n")

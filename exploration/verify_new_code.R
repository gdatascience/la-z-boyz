## Verify all new code loads without errors
library(here)

cat("=== Sourcing all modules ===\n")

tryCatch({
  source(here("R/utils/keeper_value.R"))
  cat("  keeper_value.R: OK\n")
}, error = function(e) cat("  keeper_value.R: FAIL -", e$message, "\n"))

tryCatch({
  source(here("R/utils/salary_rules.R"))
  cat("  salary_rules.R: OK\n")
}, error = function(e) cat("  salary_rules.R: FAIL -", e$message, "\n"))

tryCatch({
  source(here("R/utils/player_linker.R"))
  cat("  player_linker.R: OK\n")
}, error = function(e) cat("  player_linker.R: FAIL -", e$message, "\n"))

tryCatch({
  source(here("R/ingest/fetch_prospect_rankings.R"))
  cat("  fetch_prospect_rankings.R: OK\n")
}, error = function(e) cat("  fetch_prospect_rankings.R: FAIL -", e$message, "\n"))

tryCatch({
  source(here("R/analysis/trade_scorer.R"))
  cat("  trade_scorer.R: OK\n")
}, error = function(e) cat("  trade_scorer.R: FAIL -", e$message, "\n"))

tryCatch({
  source(here("R/analysis/trade_analyzer.R"))
  cat("  trade_analyzer.R: OK\n")
}, error = function(e) cat("  trade_analyzer.R: FAIL -", e$message, "\n"))

cat("\n=== Testing new functions ===\n")

# Test 1: age_decay_multiplier
cat("\nTest: age_decay_multiplier(24, 5)\n")
result <- age_decay_multiplier(24, 5)
cat("  Result:", paste(round(result, 3), collapse = ", "), "\n")
stopifnot(length(result) == 5)
stopifnot(result[1] > 1.0)  # young player should grow
cat("  PASS\n")

cat("\nTest: age_decay_multiplier(33, 5)\n")
result <- age_decay_multiplier(33, 5)
cat("  Result:", paste(round(result, 3), collapse = ", "), "\n")
stopifnot(length(result) == 5)
stopifnot(result[5] < 0.6)  # old player should decline sharply
cat("  PASS\n")

# Test 2: compute_age_adjusted_keeper_surplus
cat("\nTest: compute_age_adjusted_keeper_surplus (young player)\n")
salaries <- project_keeper_salary(3, is_minor_contract = FALSE, years_ahead = 3)
result <- compute_age_adjusted_keeper_surplus(40, salaries, current_age = 24)
cat("  NPV surplus:", round(result$npv_surplus, 2), "\n")
cat("  Projected values:", paste(round(result$projected_values, 1), collapse = ", "), "\n")
stopifnot(result$projected_values[1] > 40)  # should grow
cat("  PASS\n")

cat("\nTest: compute_age_adjusted_keeper_surplus (old player)\n")
salaries <- project_keeper_salary(37, is_minor_contract = FALSE, years_ahead = 3)
result <- compute_age_adjusted_keeper_surplus(64, salaries, current_age = 33)
cat("  NPV surplus:", round(result$npv_surplus, 2), "\n")
cat("  Projected values:", paste(round(result$projected_values, 1), collapse = ", "), "\n")
stopifnot(result$projected_values[3] < 50)  # should decline
cat("  PASS\n")

# Test 3: score_trade
cat("\nTest: score_trade (positive trade)\n")
score <- score_trade(
  pts_wk_change = 10,
  surplus_change = 15,
  keeper_3yr_receive = 80,
  cap_freed = 25,
  prospect_bonus = 10,
  positional_fit = 15
)
cat("  Score:", score$total, "| Grade:", score$grade, "\n")
stopifnot(score$total > 50)
stopifnot(score$grade %in% c("A", "B+", "B", "B-"))
cat("  PASS\n")

cat("\nTest: score_trade (terrible trade)\n")
score <- score_trade(
  pts_wk_change = -15,
  surplus_change = -50,
  keeper_3yr_receive = -100,
  cap_freed = 0
)
cat("  Score:", score$total, "| Grade:", score$grade, "\n")
stopifnot(score$total < 20)
stopifnot(score$grade %in% c("D", "F"))
cat("  PASS\n")

# Test 4: format_trade_score
cat("\nTest: format_trade_score\n")
score <- score_trade(pts_wk_change = 5, surplus_change = 10,
                     keeper_3yr_receive = 60, cap_freed = 30)
output <- format_trade_score(score, "JRam for Henderson + Ashcraft")
cat(output, "\n")
cat("  PASS\n")

# Test 5: prospect rankings
cat("\nTest: load_prospect_rankings\n")
rankings <- load_prospect_rankings()
cat("  Loaded", nrow(rankings), "rankings\n")
stopifnot(nrow(rankings) == 10)
cat("  PASS\n")

cat("\nTest: lookup_prospect\n")
tw <- lookup_prospect("Thomas White", rankings)
cat("  Thomas White: tier =", tw$tier, "| rank =", tw$best_rank, "\n")
stopifnot(tw$tier == "elite")
stopifnot(tw$best_rank == 9)
cat("  PASS\n")

cat("\nTest: rank_to_tier\n")
stopifnot(rank_to_tier(1) == "elite")
stopifnot(rank_to_tier(15) == "high")
stopifnot(rank_to_tier(50) == "mid")
stopifnot(rank_to_tier(80) == "low")
cat("  PASS\n")

cat("\n=== ALL TESTS PASSED ===\n")

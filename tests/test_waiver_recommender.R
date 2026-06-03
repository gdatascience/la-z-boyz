# tests/test_waiver_recommender.R
# Unit tests for waiver_recommender.R

source("R/utils/player_linker.R")
source("R/analysis/waiver_recommender.R")

cat("=== Waiver Recommender Tests ===\n\n")

# --- Test suggest_bid ---

cat("Test 1: suggest_bid - minimum bid enforcement\n")
bid <- suggest_bid(player_surplus = 5, remaining_budget = 100, weeks_remaining = 10)
stopifnot(bid >= 1)
stopifnot(bid <= 100)
cat("  PASS: bid =", bid, "(>= $1, <= $100)\n\n")

cat("Test 2: suggest_bid - bid cannot exceed remaining budget\n")
bid <- suggest_bid(player_surplus = 50, remaining_budget = 3, weeks_remaining = 2, competition_factor = 5)
stopifnot(bid <= 3)
stopifnot(bid >= 1)
cat("  PASS: bid =", bid, "(<= $3 budget)\n\n")

cat("Test 3: suggest_bid - higher surplus produces higher or equal bid\n")
bid_low <- suggest_bid(player_surplus = 5, remaining_budget = 200, weeks_remaining = 12, competition_factor = 3)
bid_high <- suggest_bid(player_surplus = 40, remaining_budget = 200, weeks_remaining = 12, competition_factor = 3)
stopifnot(bid_high >= bid_low)
cat("  PASS: bid_high =", bid_high, ">= bid_low =", bid_low, "\n\n")

cat("Test 4: suggest_bid - zero budget returns 0\n")
bid <- suggest_bid(player_surplus = 20, remaining_budget = 0, weeks_remaining = 10)
stopifnot(bid == 0)
cat("  PASS: bid = 0 when budget = 0\n\n")

cat("Test 5: suggest_bid - non-positive surplus returns minimum bid\n")
bid <- suggest_bid(player_surplus = 0, remaining_budget = 100, weeks_remaining = 10)
stopifnot(bid == 1)
bid_neg <- suggest_bid(player_surplus = -5, remaining_budget = 100, weeks_remaining = 10)
stopifnot(bid_neg == 1)
cat("  PASS: bid = $1 for zero/negative surplus\n\n")

# --- Test recommend_faab ---

cat("Test 6: recommend_faab - returns empty for non-contender in playoffs\n")
# Create minimal test data
valuations <- data.frame(
  player_name = c("Player A", "Player B", "Player C"),
  dollar_value = c(30, 20, 10),
  surplus_value = c(20, 10, 5),
  position = c("SS", "OF", "SP"),
  player_type = c("Batter", "Batter", "Pitcher"),
  proj_pts_per_week = c(15, 10, 8),
  stringsAsFactors = FALSE
)

rosters <- data.frame(
  team_name = c("My Team", "My Team", "Other Team"),
  player_name = c("Player B", "Player C", "Player A"),
  salary = c(10, 5, 10),
  player_type = c("Batter", "Pitcher", "Batter"),
  position = c("OF", "SP", "SS"),
  stringsAsFactors = FALSE
)

result <- recommend_faab("My Team", valuations, rosters,
                          remaining_faab = 100, weeks_remaining = 0)
stopifnot(nrow(result$targets) == 0)
stopifnot(grepl("locked", result$message, ignore.case = TRUE) ||
          grepl("playoff", result$message, ignore.case = TRUE))
cat("  PASS: empty targets during playoffs\n\n")

cat("Test 7: recommend_faab - returns empty when no budget\n")
result <- recommend_faab("My Team", valuations, rosters,
                          remaining_faab = 0, weeks_remaining = 10)
stopifnot(nrow(result$targets) == 0)
cat("  PASS: empty targets when budget = $0\n\n")

cat("Test 8: recommend_faab - identifies free agents correctly\n")
# Player A is on Other Team's roster, not free
# Add unrostered players to valuations
valuations_ext <- rbind(valuations, data.frame(
  player_name = c("Free Agent 1", "Free Agent 2"),
  dollar_value = c(25, 15),
  surplus_value = c(25, 15),
  position = c("1B", "RP"),
  player_type = c("Batter", "Pitcher"),
  proj_pts_per_week = c(12, 7),
  stringsAsFactors = FALSE
))

result <- recommend_faab("My Team", valuations_ext, rosters,
                          remaining_faab = 100, weeks_remaining = 10)
# Free Agent 1 and 2 should be in targets (they have positive surplus)
stopifnot(nrow(result$targets) > 0)
stopifnot("Free Agent 1" %in% result$targets$player_name)
cat("  PASS: free agents identified (", nrow(result$targets), "targets)\n\n")

cat("Test 9: recommend_faab - targets sorted by surplus descending\n")
if (nrow(result$targets) >= 2) {
  surpluses <- result$targets$effective_surplus
  stopifnot(all(diff(surpluses) <= 0))
  cat("  PASS: targets sorted descending by surplus\n\n")
} else {
  cat("  SKIP: fewer than 2 targets\n\n")
}

cat("Test 10: recommend_faab - drop candidates sorted by surplus ascending\n")
if (nrow(result$drop_candidates) >= 2) {
  drop_surpluses <- result$drop_candidates$surplus_value
  stopifnot(all(diff(drop_surpluses) >= 0))
  cat("  PASS: drop candidates sorted ascending\n\n")
} else {
  cat("  SKIP: fewer than 2 drop candidates\n\n")
}

cat("Test 11: recommend_faab - no beneficial moves returns empty\n")
# Valuations where all free agents have negative surplus
valuations_bad <- data.frame(
  player_name = c("Rostered 1", "FA Bad"),
  dollar_value = c(20, 0),
  surplus_value = c(10, -5),
  position = c("SS", "OF"),
  player_type = c("Batter", "Batter"),
  proj_pts_per_week = c(10, 0),
  stringsAsFactors = FALSE
)
rosters_small <- data.frame(
  team_name = c("My Team"),
  player_name = c("Rostered 1"),
  salary = c(10),
  player_type = c("Batter"),
  position = c("SS"),
  stringsAsFactors = FALSE
)
result <- recommend_faab("My Team", valuations_bad, rosters_small,
                          remaining_faab = 100, weeks_remaining = 10)
stopifnot(nrow(result$targets) == 0)
stopifnot(grepl("no beneficial", result$message, ignore.case = TRUE) ||
          grepl("No free agents", result$message, ignore.case = TRUE) ||
          nrow(result$targets) == 0)
cat("  PASS: empty when no beneficial moves\n\n")

cat("=== All Waiver Recommender Tests PASSED ===\n")

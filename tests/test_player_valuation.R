# Functional test for R/analysis/player_valuation.R
# Verifies core valuation logic with synthetic data

source("R/utils/keeper_value.R")
source("R/analysis/player_valuation.R")

cat("=== Testing player_valuation.R ===\n\n")

# --- Create synthetic projection data ---
# Need enough players to define replacement level at each position
set.seed(42)

# Helper to create players at a position
make_players <- function(position, n, base_ppw, player_type = "Batter") {
  data.frame(
    player_name = paste0(position, "_Player_", seq_len(n)),
    position = rep(position, n),
    player_type = rep(player_type, n),
    proj_pts_per_week = sort(runif(n, base_ppw * 0.3, base_ppw), decreasing = TRUE),
    salary = sample(1:30, n, replace = TRUE),
    is_minor_contract = rep(FALSE, n),
    minor_track_year = rep(0L, n),
    stringsAsFactors = FALSE
  )
}

# Create players for each position (enough for 16 teams + replacement)
projections <- rbind(
  make_players("C", 20, 25),
  make_players("1B", 20, 30),
  make_players("2B", 20, 28),
  make_players("3B", 20, 29),
  make_players("SS", 20, 27),
  make_players("OF", 55, 30),    # 3 slots × 16 = 48, need 49+ for repl
  make_players("SP", 90, 35, "Pitcher"),  # 5 slots × 16 = 80, need 81+
  make_players("RP", 40, 20, "Pitcher")   # 2 slots × 16 = 32, need 33+
)

# Add a player with no projection (should be excluded)
no_proj <- data.frame(
  player_name = "No_Proj_Player",
  position = "1B",
  player_type = "Batter",
  proj_pts_per_week = NA_real_,
  salary = 5,
  is_minor_contract = FALSE,
  minor_track_year = 0L,
  stringsAsFactors = FALSE
)
projections <- rbind(projections, no_proj)

cat("Created", nrow(projections), "synthetic players\n")

# --- Test 1: compute_replacement_level ---
cat("\n--- Test 1: compute_replacement_level ---\n")
repl_levels <- compute_replacement_level(projections)
cat("Replacement levels:\n")
print(repl_levels)

# Verify: replacement at C = 17th best C player (1 slot × 16 + 1 = 17)
c_players <- projections[projections$position == "C" & !is.na(projections$proj_pts_per_week), ]
c_sorted <- sort(c_players$proj_pts_per_week, decreasing = TRUE)
stopifnot(abs(repl_levels[["C"]] - c_sorted[17]) < 1e-10)
cat("PASS: C replacement level = 17th ranked C player\n")

# Verify: replacement at OF = 49th best OF (3 × 16 + 1 = 49)
of_players <- projections[projections$position == "OF" & !is.na(projections$proj_pts_per_week), ]
of_sorted <- sort(of_players$proj_pts_per_week, decreasing = TRUE)
stopifnot(abs(repl_levels[["OF"]] - of_sorted[49]) < 1e-10)
cat("PASS: OF replacement level = 49th ranked OF player\n")

# Verify: replacement at SP = 81st best SP (5 × 16 + 1 = 81)
sp_players <- projections[projections$position == "SP" & !is.na(projections$proj_pts_per_week), ]
sp_sorted <- sort(sp_players$proj_pts_per_week, decreasing = TRUE)
stopifnot(abs(repl_levels[["SP"]] - sp_sorted[81]) < 1e-10)
cat("PASS: SP replacement level = 81st ranked SP player\n")

# --- Test 2: assign_dollar_values ---
cat("\n--- Test 2: assign_dollar_values ---\n")
valued <- assign_dollar_values(projections, repl_levels, total_salary_pool = 4160)

# The NA player should be excluded
stopifnot(!("No_Proj_Player" %in% valued$player_name))
cat("PASS: Player with NA projections excluded\n")

# Sum of positive dollar values should equal total salary pool (within rounding)
total_positive <- sum(valued$dollar_value[valued$dollar_value > 0])
stopifnot(abs(total_positive - 4160) < 1.0)
cat(sprintf("PASS: Total dollar values = $%.2f (pool = $4160, diff < $1)\n", total_positive))

# Surplus value = dollar_value - salary
has_salary <- !is.na(valued$salary) & !is.na(valued$surplus_value)
stopifnot(all(abs(valued$surplus_value[has_salary] -
               (valued$dollar_value[has_salary] - valued$salary[has_salary])) < 1e-10))
cat("PASS: surplus_value = dollar_value - salary\n")

# Keeper values should be computed for players with salaries
has_keeper <- !is.na(valued$keeper_value_3yr) & !is.na(valued$salary)
stopifnot(sum(has_keeper) > 0)
cat(sprintf("PASS: %d players have keeper_value_3yr computed\n", sum(has_keeper)))

# --- Test 3: adjust_positional_scarcity ---
cat("\n--- Test 3: adjust_positional_scarcity ---\n")
scarcity_adjusted <- adjust_positional_scarcity(valued)

# Dollar values should still sum to ~$4160 after normalization
total_after <- sum(scarcity_adjusted$dollar_value[scarcity_adjusted$dollar_value > 0])
stopifnot(abs(total_after - 4160) < 1.0)
cat(sprintf("PASS: Pool preserved after scarcity adjustment ($%.2f)\n", total_after))

# C and SS (16 slots each) should have higher scarcity than OF (48 slots) and SP (80 slots)
c_scarcity <- unique(scarcity_adjusted$positional_scarcity[scarcity_adjusted$position == "C"])
of_scarcity <- unique(scarcity_adjusted$positional_scarcity[scarcity_adjusted$position == "OF"])
sp_scarcity <- unique(scarcity_adjusted$positional_scarcity[scarcity_adjusted$position == "SP"])
stopifnot(c_scarcity > of_scarcity)
stopifnot(c_scarcity > sp_scarcity)
cat(sprintf("PASS: C scarcity (%.2f) > OF scarcity (%.2f) > SP scarcity (%.2f)\n",
            c_scarcity, of_scarcity, sp_scarcity))

# --- Test 4: run_valuation_pipeline (end-to-end) ---
cat("\n--- Test 4: run_valuation_pipeline ---\n")
pipeline_result <- run_valuation_pipeline(projections)
stopifnot(nrow(pipeline_result) > 0)
stopifnot("dollar_value" %in% names(pipeline_result))
stopifnot("positional_scarcity" %in% names(pipeline_result))
stopifnot("keeper_value_3yr" %in% names(pipeline_result))
stopifnot("keeper_value_5yr" %in% names(pipeline_result))
# Should be sorted by dollar_value descending
stopifnot(all(diff(pipeline_result$dollar_value) <= 0))
cat("PASS: Pipeline produces sorted output with all required columns\n")

# --- Test 5: Edge case - minor league contracts ---
cat("\n--- Test 5: Minor league keeper values ---\n")
minor_player_idx <- which(pipeline_result$position == "SP")[1]
pipeline_result$is_minor_contract[minor_player_idx] <- TRUE
pipeline_result$minor_track_year[minor_player_idx] <- 1L
pipeline_result$salary[minor_player_idx] <- 1

# Rerun scarcity to test minor league path
scarcity_minor <- adjust_positional_scarcity(pipeline_result)
keeper_5 <- scarcity_minor$keeper_value_5yr[minor_player_idx]
stopifnot(!is.na(keeper_5))
cat(sprintf("PASS: Minor league player keeper_value_5yr = $%.2f\n", keeper_5))

cat("\n=== All tests PASSED ===\n")

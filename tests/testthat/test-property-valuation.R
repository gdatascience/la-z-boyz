# Property-Based Test: Valuation Properties 10, 11, 12
#
# **Validates: Requirements 5.2, 5.3, 5.4**
#
# Property 10: Dollar Value Conservation — sum of positive dollar values = $4,160 ± $1
# Property 11: Replacement Level Definition — replacement = (N×16+1)th player at position
# Property 12: Positional Scarcity Premium — scarcer position gets higher value for same PAR
#
# Uses replicated random synthetic projections (100+ iterations) to verify
# properties hold across diverse input scenarios.

library(testthat)

# --- Configuration ---

N_TEAMS <- 16

# League roster construction: slots per position per team
ROSTER_SLOTS <- c(
  C    = 1,
  `1B` = 1,

  `2B` = 1,
  `3B` = 1,
  SS   = 1,
  OF   = 3,
  U    = 1,
  SP   = 5,
  RP   = 2
)

TOTAL_SALARY_POOL <- 4160

# Positions and their total league slots (slots_per_team * 16)
TOTAL_LEAGUE_SLOTS <- ROSTER_SLOTS * N_TEAMS

# --- Synthetic Data Generator ---

#' Generate synthetic projections data frame with enough players per position
#' to establish proper replacement levels (>= N*16 + 1 players per position).
#'
#' @param seed Random seed for reproducibility
#' @param extra_players Additional players beyond replacement level per position
#' @return Data frame with player_name, position, player_type, proj_pts_per_week
generate_synthetic_projections <- function(seed = NULL, extra_players = 10) {
  if (!is.null(seed)) set.seed(seed)

  positions <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")
  player_types <- ifelse(positions %in% c("SP", "RP"), "Pitcher", "Batter")

  all_players <- data.frame(
    player_name = character(0),
    position = character(0),
    player_type = character(0),
    proj_pts_per_week = numeric(0),
    stringsAsFactors = FALSE
  )

  for (idx in seq_along(positions)) {
    pos <- positions[idx]
    ptype <- player_types[idx]

    # Need at least N*16 + 1 + extra_players at each position
    slots <- ROSTER_SLOTS[[pos]]
    n_needed <- slots * N_TEAMS + 1 + extra_players

    # Generate points that decrease (top players are best)
    # Use a base level + random spread to create realistic-looking distributions
    base_pts <- runif(1, min = 5, max = 20)  # random base for this position
    spread <- runif(1, min = 0.1, max = 0.5) # how much pts decrease per rank

    pts <- base_pts + sort(runif(n_needed, min = 0, max = spread * n_needed), decreasing = TRUE)

    players <- data.frame(
      player_name = paste0(pos, "_Player_", seq_len(n_needed)),
      position = rep(pos, n_needed),
      player_type = rep(ptype, n_needed),
      proj_pts_per_week = pts,
      stringsAsFactors = FALSE
    )

    all_players <- rbind(all_players, players)
  }

  all_players
}

# --- Property 10: Dollar Value Conservation ---

test_that("Property 10: Sum of positive dollar values equals total salary pool ($4,160 +/- $1)", {
  # **Validates: Requirements 5.2**
  #
  # For any set of player projections with defined replacement levels,

  # the sum of all positive dollar values assigned by assign_dollar_values()
  # shall equal the total salary pool ($4,160 +/- rounding tolerance of $1).

  set.seed(2024)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    proj <- generate_synthetic_projections(seed = 2024 + i, extra_players = sample(5:20, 1))

    # Compute replacement levels
    repl_levels <- compute_replacement_level(proj, ROSTER_SLOTS)

    # Assign dollar values
    suppressMessages({
      valued <- assign_dollar_values(proj, repl_levels, total_salary_pool = TOTAL_SALARY_POOL)
    })

    # Sum of positive dollar values should equal total salary pool
    positive_sum <- sum(valued$dollar_value[valued$dollar_value > 0])

    expect_true(
      abs(positive_sum - TOTAL_SALARY_POOL) <= 1.0,
      info = sprintf(
        "Iteration %d: sum of positive dollar values = $%.2f, expected $%d +/- $1",
        i, positive_sum, TOTAL_SALARY_POOL
      )
    )
  }
})

test_that("Property 10: Dollar value conservation holds after positional scarcity adjustment", {
  # **Validates: Requirements 5.2**
  #
  # After applying scarcity adjustment, the total positive dollar values
  # should still be conserved (scarcity is a redistribution, not a creation of value).

  set.seed(7777)
  n_iterations <- 100

  for (i in seq_len(n_iterations)) {
    proj <- generate_synthetic_projections(seed = 7777 + i, extra_players = sample(5:15, 1))

    repl_levels <- compute_replacement_level(proj, ROSTER_SLOTS)

    suppressMessages({
      valued <- assign_dollar_values(proj, repl_levels, total_salary_pool = TOTAL_SALARY_POOL)
      adjusted <- adjust_positional_scarcity(valued, ROSTER_SLOTS)
    })

    positive_sum <- sum(adjusted$dollar_value[adjusted$dollar_value > 0])

    expect_true(
      abs(positive_sum - TOTAL_SALARY_POOL) <= 1.0,
      info = sprintf(
        "Iteration %d: after scarcity adjustment, sum = $%.2f, expected $%d +/- $1",
        i, positive_sum, TOTAL_SALARY_POOL
      )
    )
  }
})

# --- Property 11: Replacement Level Definition ---

test_that("Property 11: Replacement level equals the (N*16+1)th player at each position", {
  # **Validates: Requirements 5.3**
  #
  # For any position with N roster slots across 16 teams, the computed
  # replacement level shall equal the projected points-per-week of the
  # (N*16 + 1)th ranked player at that position.

  set.seed(5050)
  n_iterations <- 120

  positions_to_check <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")

  for (i in seq_len(n_iterations)) {
    proj <- generate_synthetic_projections(seed = 5050 + i, extra_players = sample(5:25, 1))

    repl_levels <- compute_replacement_level(proj, ROSTER_SLOTS)

    # Manually verify for each position
    for (pos in positions_to_check) {
      n_slots <- ROSTER_SLOTS[[pos]]
      repl_rank <- n_slots * N_TEAMS + 1

      # Get players at this position sorted by pts/week descending
      pos_players <- proj[proj$position == pos, ]
      pos_players <- pos_players[order(-pos_players$proj_pts_per_week), ]

      # The replacement level should be the (N*16+1)th player's pts/week
      expected_repl <- pos_players$proj_pts_per_week[repl_rank]

      expect_equal(
        repl_levels[[pos]], expected_repl,
        tolerance = 1e-10,
        info = sprintf(
          "Iteration %d, position %s: replacement level = %.6f, expected (N*16+1)th = %.6f",
          i, pos, repl_levels[[pos]], expected_repl
        )
      )
    }
  }
})

test_that("Property 11: Replacement level decreases when more players exist per position", {
  # **Validates: Requirements 5.3**
  #
  # Sanity check: adding more high-quality players to a position should not
  # change the replacement level (it is defined by rank, not average).
  # However adding players BELOW the cut will not change it either.
  # The key invariant is: replacement level = exact (N*16+1)th value.

  set.seed(9999)

  for (i in 1:50) {
    proj <- generate_synthetic_projections(seed = 9999 + i, extra_players = 10)
    repl_base <- compute_replacement_level(proj, ROSTER_SLOTS)

    # Add extra weak players to each position (below current replacement)
    extra_weak <- data.frame(
      player_name = paste0("Weak_", seq_len(20)),
      position = rep("SS", 20),
      player_type = rep("Batter", 20),
      proj_pts_per_week = runif(20, min = 0, max = repl_base[["SS"]] * 0.5),
      stringsAsFactors = FALSE
    )
    proj_expanded <- rbind(proj, extra_weak)
    repl_expanded <- compute_replacement_level(proj_expanded, ROSTER_SLOTS)

    # Replacement level at SS should be unchanged (weak players are below the cut)
    expect_equal(
      repl_expanded[["SS"]], repl_base[["SS"]],
      tolerance = 1e-10,
      info = sprintf("Iteration %d: adding weak SS players should not change replacement level", i)
    )
  }
})

# --- Property 12: Positional Scarcity Premium ---

test_that("Property 12: Scarcer position gets higher value for same PAR", {
  # **Validates: Requirements 5.4**
  #
  # For any two players at different positions with identical points-above-replacement,
  # the player at the scarcer position (fewer total league slots) shall receive
  # a higher or equal dollar value after scarcity adjustment.
  #
  # Scarcity is defined by total league slots: fewer slots = scarcer position.
  # C=16, SS=16 are scarcer than OF=48, SP=80.

  set.seed(1234)
  n_iterations <- 120

  # Pairs of positions to compare: (scarcer, deeper)
  # Scarcer = fewer total slots => should get premium
  position_pairs <- list(
    c("C", "OF"),    # C=16 slots vs OF=48 slots
    c("SS", "SP"),   # SS=16 slots vs SP=80 slots
    c("C", "SP"),    # C=16 slots vs SP=80 slots
    c("1B", "OF"),   # 1B=16 slots vs OF=48 slots
    c("SS", "OF"),   # SS=16 slots vs OF=48 slots
    c("C", "RP")     # C=16 slots vs RP=32 slots
  )

  for (i in seq_len(n_iterations)) {
    proj <- generate_synthetic_projections(seed = 1234 + i, extra_players = sample(8:20, 1))

    repl_levels <- compute_replacement_level(proj, ROSTER_SLOTS)

    suppressMessages({
      valued <- assign_dollar_values(proj, repl_levels, total_salary_pool = TOTAL_SALARY_POOL)
      adjusted <- adjust_positional_scarcity(valued, ROSTER_SLOTS)
    })

    # For each position pair, find players with similar PAR and check scarcity premium
    for (pair in position_pairs) {
      scarce_pos <- pair[1]
      deep_pos <- pair[2]

      scarce_players <- adjusted[adjusted$position == scarce_pos & adjusted$pts_above_replacement > 0, ]
      deep_players <- adjusted[adjusted$position == deep_pos & adjusted$pts_above_replacement > 0, ]

      if (nrow(scarce_players) == 0 || nrow(deep_players) == 0) next

      # Find players with approximately the same PAR
      # Take a mid-range scarce player and find a deep player with closest PAR
      mid_idx <- ceiling(nrow(scarce_players) / 2)
      scarce_player <- scarce_players[mid_idx, ]
      target_par <- scarce_player$pts_above_replacement

      # Find deep player with closest PAR
      par_diffs <- abs(deep_players$pts_above_replacement - target_par)
      closest_idx <- which.min(par_diffs)
      deep_player <- deep_players[closest_idx, ]

      # Only compare if PARs are close enough (within 10% of the target)
      if (abs(deep_player$pts_above_replacement - target_par) > 0.1 * target_par) next

      # The scarce position player should have >= dollar value
      # (due to scarcity multiplier favoring thin positions)
      scarce_slots <- TOTAL_LEAGUE_SLOTS[[scarce_pos]]
      deep_slots <- TOTAL_LEAGUE_SLOTS[[deep_pos]]

      if (scarce_slots < deep_slots) {
        expect_true(
          scarce_player$dollar_value >= deep_player$dollar_value - 0.01,
          info = sprintf(
            "Iteration %d: %s (slots=%d, PAR=%.2f, $%.2f) should have >= value than %s (slots=%d, PAR=%.2f, $%.2f)",
            i, scarce_pos, scarce_slots, scarce_player$pts_above_replacement, scarce_player$dollar_value,
            deep_pos, deep_slots, deep_player$pts_above_replacement, deep_player$dollar_value
          )
        )
      }
    }
  }
})

test_that("Property 12: Scarcity multiplier is inversely proportional to league slots", {
  # **Validates: Requirements 5.4**
  #
  # The scarcity multiplier for each position should be inversely proportional
  # to the number of total league slots. Positions with fewer slots get higher
  # multipliers, positions with more slots get lower multipliers.

  set.seed(4321)

  proj <- generate_synthetic_projections(seed = 4321, extra_players = 15)
  repl_levels <- compute_replacement_level(proj, ROSTER_SLOTS)

  suppressMessages({
    valued <- assign_dollar_values(proj, repl_levels, total_salary_pool = TOTAL_SALARY_POOL)
    adjusted <- adjust_positional_scarcity(valued, ROSTER_SLOTS)
  })

  # Extract scarcity multipliers by looking at players at each position
  positions <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")
  scarcity_by_pos <- numeric(length(positions))
  names(scarcity_by_pos) <- positions

  for (pos in positions) {
    pos_rows <- adjusted[adjusted$position == pos & !is.na(adjusted$positional_scarcity), ]
    if (nrow(pos_rows) > 0) {
      # All players at same position should have same scarcity multiplier
      scarcity_by_pos[pos] <- pos_rows$positional_scarcity[1]
    }
  }

  # Verify: C and SS (16 slots each) should have higher scarcity than OF (48) and SP (80)
  expect_true(scarcity_by_pos[["C"]] > scarcity_by_pos[["OF"]],
    info = "C should have higher scarcity multiplier than OF")
  expect_true(scarcity_by_pos[["SS"]] > scarcity_by_pos[["SP"]],
    info = "SS should have higher scarcity multiplier than SP")
  expect_true(scarcity_by_pos[["C"]] > scarcity_by_pos[["SP"]],
    info = "C should have higher scarcity multiplier than SP")

  # Positions with same number of slots should have same scarcity multiplier
  expect_equal(scarcity_by_pos[["C"]], scarcity_by_pos[["SS"]],
    info = "C and SS (both 16 slots) should have equal scarcity multiplier")
  expect_equal(scarcity_by_pos[["C"]], scarcity_by_pos[["1B"]],
    info = "C and 1B (both 16 slots) should have equal scarcity multiplier")
})

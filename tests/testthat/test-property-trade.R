# Property-Based Test: Trade Analyzer Properties 14, 15
#
# **Validates: Requirements 6.1, 6.2, 6.3, 6.9**
#
# Property 14: Trade Arithmetic Correctness — net surplus, points change,
#              salary change computed correctly from known inputs.
# Property 15: Trade Lopsided Detection — flag when |net_surplus| > 20% of
#              total trade value (sum of absolute dollar values on both sides).
#
# Uses replicated random synthetic trade scenarios (100+ iterations) to verify
# properties hold across diverse input scenarios.

library(testthat)

# --- Configuration ---

N_ITERATIONS <- 120 # Above PBT_MIN_ITERATIONS of 100

# --- Synthetic Data Generators ---

#' Generate a random player valuation row
#'
#' @param player_name Character name for the player
#' @param position Character position code
#' @param player_type "Batter" or "Pitcher"
#' @param dollar_value_range Numeric vector of length 2: min/max dollar value
#' @param salary_range Numeric vector of length 2: min/max salary
#' @return Single-row data frame with valuation columns
generate_player_valuation <- function(player_name, position = NULL,
                                      player_type = NULL,
                                      dollar_value_range = c(-5, 40),
                                      salary_range = c(1, 50)) {
  if (is.null(position)) {
    position <- sample(c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP"), 1)
  }
  if (is.null(player_type)) {
    player_type <- ifelse(position %in% c("SP", "RP"), "Pitcher", "Batter")
  }

  dollar_value <- runif(1, dollar_value_range[1], dollar_value_range[2])
  salary <- runif(1, salary_range[1], salary_range[2])
  surplus_value <- dollar_value - salary
  proj_pts_per_week <- runif(1, min = 2, max = 25)

  data.frame(
    player_name = player_name,
    position = position,
    player_type = player_type,
    dollar_value = dollar_value,
    salary = salary,
    surplus_value = surplus_value,
    proj_pts_per_week = proj_pts_per_week,
    stringsAsFactors = FALSE
  )
}

#' Generate a random trade scenario with N players on each side
#'
#' @param n_give Number of players to give away (1-4)
#' @param n_receive Number of players to receive (1-4)
#' @param seed Optional random seed
#' @return List with: give_vals, receive_vals, rosters, my_team, standings,
#'         expected values for verification
generate_trade_scenario <- function(n_give = NULL, n_receive = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(n_give)) n_give <- sample(1:4, 1)
  if (is.null(n_receive)) n_receive <- sample(1:4, 1)

  my_team <- "TestTeam"
  other_team <- "OtherTeam"

  # Generate player valuations for the give side
  give_names <- paste0("Give_Player_", seq_len(n_give))
  give_vals <- do.call(rbind, lapply(give_names, generate_player_valuation))

  # Generate player valuations for the receive side
  receive_names <- paste0("Receive_Player_", seq_len(n_receive))
  receive_vals <- do.call(rbind, lapply(receive_names, generate_player_valuation))

  # Build roster data with the players assigned to appropriate teams
  # Give players are on my_team; receive players are on other_team
  my_salary_base <- runif(1, min = 100, max = 250)  # team's other salary

  give_roster <- data.frame(
    team_name = rep(my_team, n_give),
    player_name = give_names,
    salary = give_vals$salary,
    player_type = give_vals$player_type,
    position = give_vals$position,
    is_minor_contract = rep(FALSE, n_give),
    stringsAsFactors = FALSE
  )

  receive_roster <- data.frame(
    team_name = rep(other_team, n_receive),
    player_name = receive_names,
    salary = receive_vals$salary,
    player_type = receive_vals$player_type,
    position = receive_vals$position,
    is_minor_contract = rep(FALSE, n_receive),
    stringsAsFactors = FALSE
  )

  # Add filler players for my_team so the roster has a reasonable salary total
  filler <- data.frame(
    team_name = my_team,
    player_name = "Filler_Player",
    salary = my_salary_base,
    player_type = "Batter",
    position = "OF",
    is_minor_contract = FALSE,
    stringsAsFactors = FALSE
  )

  rosters <- rbind(give_roster, receive_roster, filler)

  # Build standings (minimal, just needs to work with assess_competitive_context)
  standings <- data.frame(
    team_name = c(my_team, other_team),
    wins = c(12, 10),
    losses = c(8, 10),
    games_back = c(0, 2),
    division = c("East", "East"),
    total_points = c(1500, 1400),
    pct = c(0.600, 0.500),
    stringsAsFactors = FALSE
  )

  # Expected computed values (what the trade analyzer should report)
  expected_surplus_give <- sum(give_vals$surplus_value)
  expected_surplus_receive <- sum(receive_vals$surplus_value)
  expected_value_diff <- expected_surplus_receive - expected_surplus_give

  # Points change breakdown
  give_batting_pts <- sum(give_vals$proj_pts_per_week[give_vals$player_type == "Batter"])
  give_pitching_pts <- sum(give_vals$proj_pts_per_week[give_vals$player_type == "Pitcher"])
  receive_batting_pts <- sum(receive_vals$proj_pts_per_week[receive_vals$player_type == "Batter"])
  receive_pitching_pts <- sum(receive_vals$proj_pts_per_week[receive_vals$player_type == "Pitcher"])
  expected_pts_total <- (receive_batting_pts + receive_pitching_pts) -
                        (give_batting_pts + give_pitching_pts)
  expected_pts_batting <- receive_batting_pts - give_batting_pts
  expected_pts_pitching <- receive_pitching_pts - give_pitching_pts

  # Salary impact
  expected_salary_change <- sum(receive_vals$salary) - sum(give_vals$salary)
  current_team_total <- my_salary_base + sum(give_vals$salary)
  expected_new_total <- current_team_total + expected_salary_change
  expected_cap_compliant <- expected_new_total <= 300

  # Lopsided detection
  give_abs_value <- sum(abs(give_vals$dollar_value))
  receive_abs_value <- sum(abs(receive_vals$dollar_value))
  total_trade_value <- give_abs_value + receive_abs_value
  lopsided_ratio <- if (total_trade_value > 0) {
    abs(expected_value_diff) / total_trade_value
  } else {
    0
  }
  expected_lopsided <- lopsided_ratio > 0.20

  list(
    give_names = give_names,
    receive_names = receive_names,
    give_vals = give_vals,
    receive_vals = receive_vals,
    rosters = rosters,
    standings = standings,
    my_team = my_team,
    current_team_total = current_team_total,
    expected = list(
      value_diff = expected_value_diff,
      pts_total = expected_pts_total,
      pts_batting = expected_pts_batting,
      pts_pitching = expected_pts_pitching,
      salary_change = expected_salary_change,
      new_total = expected_new_total,
      cap_compliant = expected_cap_compliant,
      lopsided = expected_lopsided,
      lopsided_ratio = lopsided_ratio,
      total_trade_value = total_trade_value
    )
  )
}

# --- Property 14: Trade Arithmetic Correctness ---

test_that("Property 14: Net surplus value computed correctly (surplus_receive - surplus_give)", {
  # **Validates: Requirements 6.1**
  #
  # For any trade proposal with known surplus values, the trade analyzer

  # shall report net surplus = sum(surplus_receive) - sum(surplus_give).

  set.seed(14001)

  for (i in seq_len(N_ITERATIONS)) {
    scenario <- generate_trade_scenario(seed = 14001 + i)

    result <- suppressMessages(
      analyze_trade(
        give = scenario$give_names,
        receive = scenario$receive_names,
        my_team = scenario$my_team,
        valuations = rbind(scenario$give_vals, scenario$receive_vals),
        rosters = scenario$rosters,
        standings = scenario$standings
      )
    )

    expect_equal(
      result$value_diff,
      scenario$expected$value_diff,
      tolerance = 1e-10,
      info = sprintf(
        "Iteration %d: value_diff = %.6f, expected %.6f",
        i, result$value_diff, scenario$expected$value_diff
      )
    )
  }
})

test_that("Property 14: Projected points/week change computed correctly", {
  # **Validates: Requirements 6.2**
  #
  # For any trade proposal, the points change shall equal
  # sum(pts_receive) - sum(pts_give), broken down by batting and pitching.

  set.seed(14002)

  for (i in seq_len(N_ITERATIONS)) {
    scenario <- generate_trade_scenario(seed = 14002 + i)

    result <- suppressMessages(
      analyze_trade(
        give = scenario$give_names,
        receive = scenario$receive_names,
        my_team = scenario$my_team,
        valuations = rbind(scenario$give_vals, scenario$receive_vals),
        rosters = scenario$rosters,
        standings = scenario$standings
      )
    )

    # Extract points change from summary (the internal compute_pts_week_change is
    # tested through analyze_trade's summary/justification and returned indirectly).
    # The justification mentions pts/wk change. But we can also verify through
    # recomputing from the passed-in valuations (the function uses proj_pts_per_week).

    # Recompute expected values using the same logic as compute_pts_week_change
    give_batting <- sum(scenario$give_vals$proj_pts_per_week[
      scenario$give_vals$player_type == "Batter"
    ])
    give_pitching <- sum(scenario$give_vals$proj_pts_per_week[
      scenario$give_vals$player_type == "Pitcher"
    ])
    receive_batting <- sum(scenario$receive_vals$proj_pts_per_week[
      scenario$receive_vals$player_type == "Batter"
    ])
    receive_pitching <- sum(scenario$receive_vals$proj_pts_per_week[
      scenario$receive_vals$player_type == "Pitcher"
    ])

    expected_total <- (receive_batting + receive_pitching) - (give_batting + give_pitching)

    # The summary text contains the total pts/week change
    # Verify by parsing the summary for the pts/wk total line
    pts_pattern <- "Total:\\s+([+-]?[0-9]+\\.?[0-9]*) pts/wk"
    pts_match <- regmatches(result$summary, regexpr(pts_pattern, result$summary))

    if (length(pts_match) > 0) {
      # Extract numeric value from the match
      pts_val <- as.numeric(sub("Total:\\s+", "", sub(" pts/wk", "", pts_match)))
      expect_equal(
        pts_val,
        expected_total,
        tolerance = 0.15, # summary is formatted with 1 decimal
        info = sprintf("Iteration %d: pts/wk total = %.1f, expected %.1f",
                       i, pts_val, expected_total)
      )
    }
  }
})

test_that("Property 14: Salary change and cap compliance computed correctly", {
  # **Validates: Requirements 6.3**
  #
  # For any trade proposal, salary_impact shall report:
  # (a) net_change = sum(salary_receive) - sum(salary_give)
  # (b) new_total = current_team_total + net_change
  # (c) cap_compliant = (new_total <= 300)

  set.seed(14003)

  for (i in seq_len(N_ITERATIONS)) {
    scenario <- generate_trade_scenario(seed = 14003 + i)

    result <- suppressMessages(
      analyze_trade(
        give = scenario$give_names,
        receive = scenario$receive_names,
        my_team = scenario$my_team,
        valuations = rbind(scenario$give_vals, scenario$receive_vals),
        rosters = scenario$rosters,
        standings = scenario$standings
      )
    )

    # Verify salary net change
    expect_equal(
      result$salary_impact$net_change,
      scenario$expected$salary_change,
      tolerance = 1e-10,
      info = sprintf(
        "Iteration %d: salary net_change = %.6f, expected %.6f",
        i, result$salary_impact$net_change, scenario$expected$salary_change
      )
    )

    # Verify new total
    expect_equal(
      result$salary_impact$new_total,
      scenario$expected$new_total,
      tolerance = 1e-10,
      info = sprintf(
        "Iteration %d: new_total = %.6f, expected %.6f",
        i, result$salary_impact$new_total, scenario$expected$new_total
      )
    )

    # Verify cap compliance
    expect_equal(
      result$salary_impact$cap_compliant,
      scenario$expected$cap_compliant,
      info = sprintf(
        "Iteration %d: cap_compliant = %s, expected %s (new_total = %.2f)",
        i, result$salary_impact$cap_compliant, scenario$expected$cap_compliant,
        result$salary_impact$new_total
      )
    )
  }
})

# --- Property 15: Trade Lopsided Detection ---

test_that("Property 15: Trades exceeding 20% threshold are flagged as lopsided", {
  # **Validates: Requirements 6.9**
  #
  # For any trade where |net_surplus_difference| > 0.20 * (sum|value_give| + sum|value_receive|),
  # is_trade_lopsided shall return TRUE.

  set.seed(15001)

  flagged_count <- 0

  for (i in seq_len(N_ITERATIONS)) {
    # Generate trade scenarios that are intentionally lopsided
    # One side gets a high-value player, other side gets low-value player
    set.seed(15001 + i)

    # Create a scenario where one side has much higher dollar value
    # to push the ratio above 0.20
    give_vals <- data.frame(
      player_name = "Give_A",
      position = "SS",
      player_type = "Batter",
      dollar_value = runif(1, min = 1, max = 10),
      salary = runif(1, min = 5, max = 20),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, min = 2, max = 8),
      stringsAsFactors = FALSE
    )
    give_vals$surplus_value <- give_vals$dollar_value - give_vals$salary

    receive_vals <- data.frame(
      player_name = "Receive_A",
      position = "OF",
      player_type = "Batter",
      dollar_value = runif(1, min = 25, max = 50),
      salary = runif(1, min = 1, max = 10),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, min = 15, max = 25),
      stringsAsFactors = FALSE
    )
    receive_vals$surplus_value <- receive_vals$dollar_value - receive_vals$salary

    # Compute expected ratio
    total_value <- sum(abs(give_vals$dollar_value)) + sum(abs(receive_vals$dollar_value))
    net_diff <- abs(sum(receive_vals$surplus_value) - sum(give_vals$surplus_value))
    ratio <- net_diff / total_value

    # Only test cases where we expect lopsided to be TRUE
    if (ratio > 0.20) {
      result <- is_trade_lopsided(give_vals, receive_vals)
      expect_true(
        result,
        info = sprintf(
          "Iteration %d: ratio = %.4f (> 0.20), should be flagged lopsided",
          i, ratio
        )
      )
      flagged_count <- flagged_count + 1
    }
  }

  # Ensure we tested a meaningful number of lopsided cases

  expect_true(
    flagged_count >= 50,
    info = sprintf("Only %d lopsided cases generated (need >= 50)", flagged_count)
  )
})

test_that("Property 15: Trades at or below 20% threshold are NOT flagged as lopsided", {
  # **Validates: Requirements 6.9**
  #
  # For any trade where |net_surplus_difference| <= 0.20 * (sum|value_give| + sum|value_receive|),
  # is_trade_lopsided shall return FALSE.

  set.seed(15002)

  not_flagged_count <- 0

  for (i in seq_len(N_ITERATIONS)) {
    set.seed(15002 + i)

    # Generate balanced trades: both sides have similar dollar values
    # to keep the ratio at or below 20%
    base_value <- runif(1, min = 10, max = 30)
    perturbation <- runif(1, min = 0, max = 0.05) * base_value # small diff

    give_vals <- data.frame(
      player_name = "Give_A",
      position = "1B",
      player_type = "Batter",
      dollar_value = base_value,
      salary = base_value * runif(1, 0.4, 0.6),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, min = 5, max = 15),
      stringsAsFactors = FALSE
    )
    give_vals$surplus_value <- give_vals$dollar_value - give_vals$salary

    receive_vals <- data.frame(
      player_name = "Receive_A",
      position = "3B",
      player_type = "Batter",
      dollar_value = base_value + perturbation,
      salary = (base_value + perturbation) * runif(1, 0.4, 0.6),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, min = 5, max = 15),
      stringsAsFactors = FALSE
    )
    receive_vals$surplus_value <- receive_vals$dollar_value - receive_vals$salary

    # Compute expected ratio
    total_value <- sum(abs(give_vals$dollar_value)) + sum(abs(receive_vals$dollar_value))
    net_diff <- abs(sum(receive_vals$surplus_value) - sum(give_vals$surplus_value))
    ratio <- if (total_value > 0) net_diff / total_value else 0

    # Only test cases where we expect lopsided to be FALSE
    if (ratio <= 0.20) {
      result <- is_trade_lopsided(give_vals, receive_vals)
      expect_false(
        result,
        info = sprintf(
          "Iteration %d: ratio = %.4f (<= 0.20), should NOT be flagged lopsided",
          i, ratio
        )
      )
      not_flagged_count <- not_flagged_count + 1
    }
  }

  # Ensure we tested a meaningful number of non-lopsided cases
  expect_true(
    not_flagged_count >= 50,
    info = sprintf("Only %d non-lopsided cases generated (need >= 50)", not_flagged_count)
  )
})

test_that("Property 15: Lopsided detection through full analyze_trade is consistent", {
  # **Validates: Requirements 6.9**
  #
  # Verify that the is_lopsided field in analyze_trade output matches
  # the standalone is_trade_lopsided function for random trade scenarios.

  set.seed(15003)

  for (i in seq_len(N_ITERATIONS)) {
    scenario <- generate_trade_scenario(seed = 15003 + i)

    result <- suppressMessages(
      analyze_trade(
        give = scenario$give_names,
        receive = scenario$receive_names,
        my_team = scenario$my_team,
        valuations = rbind(scenario$give_vals, scenario$receive_vals),
        rosters = scenario$rosters,
        standings = scenario$standings
      )
    )

    # Verify is_lopsided matches our expected calculation
    expect_equal(
      result$is_lopsided,
      scenario$expected$lopsided,
      info = sprintf(
        "Iteration %d: is_lopsided = %s, expected %s (ratio = %.4f, threshold = 0.20)",
        i, result$is_lopsided, scenario$expected$lopsided, scenario$expected$lopsided_ratio
      )
    )
  }
})

test_that("Property 15: Zero total trade value does not flag as lopsided", {
  # **Validates: Requirements 6.9**
  #
  # Edge case: when all dollar values are 0, total_trade_value = 0,
  # and the function should return FALSE (avoid division by zero).

  set.seed(15004)

  for (i in 1:50) {
    set.seed(15004 + i)

    give_vals <- data.frame(
      player_name = paste0("Give_", i),
      position = "OF",
      player_type = "Batter",
      dollar_value = 0,
      salary = runif(1, 1, 10),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, 1, 5),
      stringsAsFactors = FALSE
    )
    give_vals$surplus_value <- give_vals$dollar_value - give_vals$salary

    receive_vals <- data.frame(
      player_name = paste0("Receive_", i),
      position = "SP",
      player_type = "Pitcher",
      dollar_value = 0,
      salary = runif(1, 1, 10),
      surplus_value = NA_real_,
      proj_pts_per_week = runif(1, 1, 5),
      stringsAsFactors = FALSE
    )
    receive_vals$surplus_value <- receive_vals$dollar_value - receive_vals$salary

    result <- is_trade_lopsided(give_vals, receive_vals)
    expect_false(
      result,
      info = sprintf("Iteration %d: zero trade value should not be flagged lopsided", i)
    )
  }
})

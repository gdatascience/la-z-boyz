# Property-Based Test: Waiver Recommender Properties 16, 17, 18, 21
#
# **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.6, 7.8**
#
# Property 16: Waiver Recommendations Sorted by Surplus — targets descending, drops ascending
# Property 17: FAAB Bid Bounds and Monotonicity — bid >= $1 and <= budget; higher surplus -> higher bid
# Property 18: Recommendation Constraint Compliance — position eligibility + salary cap + playoff lock
# Property 21: Minor League Promotion Threshold Flagging — flag at AB>=110 or IP>=40
#
# Uses replicated random synthetic data (120 iterations) to verify
# properties hold across diverse input scenarios.

library(testthat)

# --- Configuration ---

WAIVER_TEST_ITERATIONS <- 120

# League roster positions for reference
LEAGUE_POSITIONS <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")
BATTER_POSITIONS <- c("C", "1B", "2B", "3B", "SS", "OF")
PITCHER_POSITIONS <- c("SP", "RP")

# --- Synthetic Data Generators ---

#' Generate a synthetic valuations data frame with both rostered and free agent players
#'
#' @param seed Random seed for reproducibility
#' @param n_rostered Number of players on owner's team
#' @param n_free_agents Number of free agents in the pool
#' @param n_other_rostered Number of players on other teams
#' @return Data frame of player valuations
generate_valuations <- function(seed = NULL, n_rostered = 20, n_free_agents = 30,
                                n_other_rostered = 50) {
  if (!is.null(seed)) set.seed(seed)

  n_total <- n_rostered + n_free_agents + n_other_rostered
  positions <- sample(LEAGUE_POSITIONS, n_total, replace = TRUE)
  player_types <- ifelse(positions %in% PITCHER_POSITIONS, "Pitcher", "Batter")

  data.frame(
    player_name = paste0("Player_", seq_len(n_total)),
    dollar_value = runif(n_total, min = -5, max = 50),
    surplus_value = runif(n_total, min = -10, max = 40),
    position = positions,
    player_type = player_types,
    proj_pts_per_week = runif(n_total, min = 0, max = 25),
    eligible_positions = positions,  # simplified: primary position only
    stringsAsFactors = FALSE
  )
}

#' Generate a synthetic roster data frame
#'
#' @param seed Random seed
#' @param team_name Owner team name
#' @param n_players Number of players on team
#' @param valuations Valuations data frame to sample player names from
#' @param other_teams Additional teams with their players
#' @return Data frame of roster data
generate_rosters <- function(seed = NULL, team_name = "My Team", n_players = 20,
                             valuations = NULL, other_teams = 2) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(valuations)) {
    stop("valuations must be provided")
  }

  n_total <- nrow(valuations)
  # First n_players belong to my_team
  my_indices <- seq_len(min(n_players, n_total))
  # Next batch for other teams
  other_start <- n_players + 1
  n_other_each <- min(15, floor((n_total - n_players) / max(other_teams, 1)))

  roster_rows <- list()

  # My team
  for (i in my_indices) {
    roster_rows[[length(roster_rows) + 1]] <- data.frame(
      team_name = team_name,
      player_name = valuations$player_name[i],
      salary = round(runif(1, min = 1, max = 40)),
      player_type = valuations$player_type[i],
      position = valuations$position[i],
      eligible_positions = valuations$position[i],
      is_minor_contract = FALSE,
      roster_position = valuations$position[i],
      stringsAsFactors = FALSE
    )
  }

  # Other teams
  for (t in seq_len(other_teams)) {
    team_start <- other_start + (t - 1) * n_other_each
    team_end <- min(team_start + n_other_each - 1, n_total)
    if (team_start > n_total) break

    for (i in team_start:team_end) {
      roster_rows[[length(roster_rows) + 1]] <- data.frame(
        team_name = paste0("Other_Team_", t),
        player_name = valuations$player_name[i],
        salary = round(runif(1, min = 1, max = 30)),
        player_type = valuations$player_type[i],
        position = valuations$position[i],
        eligible_positions = valuations$position[i],
        is_minor_contract = FALSE,
        roster_position = valuations$position[i],
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, roster_rows)
}


# =============================================================================
# Property 16: Waiver Recommendations Sorted by Surplus
# =============================================================================

test_that("Property 16: FAAB targets are sorted in descending order of surplus value", {
  # **Validates: Requirements 7.1**
  #
  # For any set of available free agents, the FAAB recommendation list shall be
  # sorted in descending order of projected surplus value, containing at most 10 entries.

  set.seed(1600)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1600 + i
    set.seed(seed_i)

    n_roster <- sample(15:25, 1)
    n_fa <- sample(15:50, 1)
    n_other <- sample(20:60, 1)

    valuations <- generate_valuations(
      seed = seed_i, n_rostered = n_roster,
      n_free_agents = n_fa, n_other_rostered = n_other
    )
    rosters <- generate_rosters(
      seed = seed_i, team_name = "My Team",
      n_players = n_roster, valuations = valuations, other_teams = 2
    )

    remaining_faab <- sample(10:250, 1)
    weeks_remaining <- sample(1:20, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    # If targets are returned, they must be sorted by effective_surplus descending
    if (nrow(result$targets) > 0) {
      surpluses <- result$targets$effective_surplus
      expect_true(
        all(diff(surpluses) <= 0),
        info = sprintf(
          "Iteration %d: targets not sorted descending. Surpluses: %s",
          i, paste(round(surpluses, 2), collapse = ", ")
        )
      )
      # At most 10 entries
      expect_true(
        nrow(result$targets) <= 10,
        info = sprintf("Iteration %d: targets has %d entries (max 10)", i, nrow(result$targets))
      )
    }
  }
})

test_that("Property 16: Drop candidates are sorted in ascending order of surplus value", {
  # **Validates: Requirements 7.2**
  #
  # For any owner's roster, the drop candidates list shall be sorted in
  # ascending order of projected surplus value.

  set.seed(1602)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1602 + i
    set.seed(seed_i)

    n_roster <- sample(15:27, 1)
    n_fa <- sample(10:40, 1)
    n_other <- sample(20:50, 1)

    valuations <- generate_valuations(
      seed = seed_i, n_rostered = n_roster,
      n_free_agents = n_fa, n_other_rostered = n_other
    )
    rosters <- generate_rosters(
      seed = seed_i, team_name = "My Team",
      n_players = n_roster, valuations = valuations, other_teams = 2
    )

    remaining_faab <- sample(10:200, 1)
    weeks_remaining <- sample(1:20, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    # Drop candidates should be sorted by surplus ascending (worst first)
    if (nrow(result$drop_candidates) >= 2) {
      drop_surpluses <- result$drop_candidates$surplus_value
      expect_true(
        all(diff(drop_surpluses) >= 0),
        info = sprintf(
          "Iteration %d: drops not sorted ascending. Surpluses: %s",
          i, paste(round(drop_surpluses, 2), collapse = ", ")
        )
      )
    }
  }
})


# =============================================================================
# Property 17: FAAB Bid Bounds and Monotonicity
# =============================================================================

test_that("Property 17: Suggested bids are >= $1 and <= remaining budget", {
  # **Validates: Requirements 7.3**
  #
  # For any recommended FAAB bid, the suggested amount shall be >= $1 (league
  # minimum) and <= remaining_budget.

  set.seed(1700)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1700 + i
    set.seed(seed_i)

    player_surplus <- runif(1, min = 0.1, max = 60)
    remaining_budget <- sample(1:250, 1)
    weeks_remaining <- sample(1:23, 1)
    competition_factor <- sample(1:5, 1)

    bid <- suggest_bid(
      player_surplus = player_surplus,
      remaining_budget = remaining_budget,
      weeks_remaining = weeks_remaining,
      competition_factor = competition_factor
    )

    expect_true(
      bid >= 1,
      info = sprintf(
        "Iteration %d: bid = $%d < $1 (surplus=%.2f, budget=$%d, weeks=%d, comp=%d)",
        i, bid, player_surplus, remaining_budget, weeks_remaining, competition_factor
      )
    )
    expect_true(
      bid <= remaining_budget,
      info = sprintf(
        "Iteration %d: bid = $%d > budget = $%d (surplus=%.2f, weeks=%d, comp=%d)",
        i, bid, remaining_budget, player_surplus, weeks_remaining, competition_factor
      )
    )
  }
})

test_that("Property 17: Higher surplus produces higher or equal bid (monotonicity)", {
  # **Validates: Requirements 7.3**
  #
  # For any two players where player A has strictly higher surplus value than
  # player B (with all other factors equal), the suggested bid for A shall be
  # >= the suggested bid for B.

  set.seed(1710)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1710 + i
    set.seed(seed_i)

    # Generate two surplus values where surplus_high > surplus_low
    surplus_low <- runif(1, min = 0.5, max = 30)
    surplus_high <- surplus_low + runif(1, min = 0.5, max = 25)

    # Use same budget, weeks, and competition for both
    remaining_budget <- sample(20:250, 1)
    weeks_remaining <- sample(1:23, 1)
    competition_factor <- sample(1:5, 1)

    bid_low <- suggest_bid(
      player_surplus = surplus_low,
      remaining_budget = remaining_budget,
      weeks_remaining = weeks_remaining,
      competition_factor = competition_factor
    )

    bid_high <- suggest_bid(
      player_surplus = surplus_high,
      remaining_budget = remaining_budget,
      weeks_remaining = weeks_remaining,
      competition_factor = competition_factor
    )

    expect_true(
      bid_high >= bid_low,
      info = sprintf(
        "Iteration %d: bid_high=$%d < bid_low=$%d (surplus_high=%.2f, surplus_low=%.2f, budget=$%d, weeks=%d, comp=%d)",
        i, bid_high, bid_low, surplus_high, surplus_low, remaining_budget, weeks_remaining, competition_factor
      )
    )
  }
})

test_that("Property 17: Bids from recommend_faab respect bounds", {
  # **Validates: Requirements 7.3**
  #
  # In full recommend_faab output, all suggested bids must be >= $1 and <= budget.

  set.seed(1720)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1720 + i
    set.seed(seed_i)

    n_roster <- sample(15:25, 1)
    n_fa <- sample(15:40, 1)
    n_other <- sample(20:50, 1)

    valuations <- generate_valuations(
      seed = seed_i, n_rostered = n_roster,
      n_free_agents = n_fa, n_other_rostered = n_other
    )
    rosters <- generate_rosters(
      seed = seed_i, team_name = "My Team",
      n_players = n_roster, valuations = valuations, other_teams = 2
    )

    remaining_faab <- sample(5:250, 1)
    weeks_remaining <- sample(1:20, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    if (length(result$suggested_bids) > 0) {
      for (j in seq_along(result$suggested_bids)) {
        bid <- result$suggested_bids[j]
        expect_true(
          bid >= 1,
          info = sprintf("Iteration %d, bid %d: $%d < $1 minimum", i, j, bid)
        )
        expect_true(
          bid <= remaining_faab,
          info = sprintf("Iteration %d, bid %d: $%d > budget $%d", i, j, bid, remaining_faab)
        )
      }
    }
  }
})


# =============================================================================
# Property 18: Recommendation Constraint Compliance
# =============================================================================

test_that("Property 18: Non-contenders locked from FAAB during playoffs (weeks_remaining <= 0)", {
  # **Validates: Requirements 7.8**
  #
  # For any team classified as non-contender during playoff weeks,
  # the FAAB recommendation list shall be empty.

  set.seed(1800)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1800 + i
    set.seed(seed_i)

    n_roster <- sample(15:25, 1)
    n_fa <- sample(10:30, 1)
    n_other <- sample(20:40, 1)

    valuations <- generate_valuations(
      seed = seed_i, n_rostered = n_roster,
      n_free_agents = n_fa, n_other_rostered = n_other
    )
    rosters <- generate_rosters(
      seed = seed_i, team_name = "My Team",
      n_players = n_roster, valuations = valuations, other_teams = 2
    )

    remaining_faab <- sample(50:250, 1)
    # Playoff period: weeks_remaining <= 0
    weeks_remaining <- sample(-3:0, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    expect_equal(
      nrow(result$targets), 0,
      info = sprintf(
        "Iteration %d: targets should be empty during playoffs (weeks_remaining=%d), got %d",
        i, weeks_remaining, nrow(result$targets)
      )
    )
  }
})

test_that("Property 18: Recommendations respect salary cap ($300 limit)", {
  # **Validates: Requirements 7.4**
  #
  # Adding a recommended player (at minimum $1 salary) must keep total team
  # salary <= $300. If team is already at cap, no targets should be returned.

  set.seed(1810)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1810 + i
    set.seed(seed_i)

    n_roster <- sample(15:25, 1)

    # Create valuations with known structure
    n_total <- n_roster + 20 + 30
    positions <- sample(LEAGUE_POSITIONS, n_total, replace = TRUE)
    player_types <- ifelse(positions %in% PITCHER_POSITIONS, "Pitcher", "Batter")

    valuations <- data.frame(
      player_name = paste0("Player_", seq_len(n_total)),
      dollar_value = runif(n_total, min = 5, max = 50),
      surplus_value = runif(n_total, min = 1, max = 40),
      position = positions,
      player_type = player_types,
      proj_pts_per_week = runif(n_total, min = 3, max = 25),
      eligible_positions = positions,
      stringsAsFactors = FALSE
    )

    # Build roster where team is AT the salary cap ($300)
    roster_rows <- list()
    salary_per_player <- floor(300 / n_roster)
    remainder <- 300 - salary_per_player * n_roster

    for (j in seq_len(n_roster)) {
      sal <- salary_per_player + ifelse(j <= remainder, 1, 0)
      roster_rows[[j]] <- data.frame(
        team_name = "My Team",
        player_name = valuations$player_name[j],
        salary = sal,
        player_type = valuations$player_type[j],
        position = valuations$position[j],
        eligible_positions = valuations$position[j],
        is_minor_contract = FALSE,
        roster_position = valuations$position[j],
        stringsAsFactors = FALSE
      )
    }

    # Other teams
    for (j in (n_roster + 21):min(n_total, n_roster + 35)) {
      roster_rows[[length(roster_rows) + 1]] <- data.frame(
        team_name = "Other_Team_1",
        player_name = valuations$player_name[j],
        salary = sample(1:20, 1),
        player_type = valuations$player_type[j],
        position = valuations$position[j],
        eligible_positions = valuations$position[j],
        is_minor_contract = FALSE,
        roster_position = valuations$position[j],
        stringsAsFactors = FALSE
      )
    }

    rosters <- do.call(rbind, roster_rows)

    remaining_faab <- sample(10:200, 1)
    weeks_remaining <- sample(1:15, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    # If team is at $300 cap, there is no room for even a $1 acquisition
    # So targets should be empty
    expect_equal(
      nrow(result$targets), 0,
      info = sprintf(
        "Iteration %d: team at $300 cap should get 0 targets, got %d",
        i, nrow(result$targets)
      )
    )
  }
})

test_that("Property 18: Recommendations require positive surplus (no bad moves)", {
  # **Validates: Requirements 7.4, 7.7**
  #
  # All targets in recommendations must have positive effective surplus.
  # The system should not recommend acquisitions that lose value.

  set.seed(1820)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 1820 + i
    set.seed(seed_i)

    n_roster <- sample(15:22, 1)
    n_fa <- sample(15:40, 1)
    n_other <- sample(20:40, 1)

    valuations <- generate_valuations(
      seed = seed_i, n_rostered = n_roster,
      n_free_agents = n_fa, n_other_rostered = n_other
    )
    rosters <- generate_rosters(
      seed = seed_i, team_name = "My Team",
      n_players = n_roster, valuations = valuations, other_teams = 2
    )

    remaining_faab <- sample(10:200, 1)
    weeks_remaining <- sample(1:20, 1)

    suppressMessages({
      result <- recommend_faab("My Team", valuations, rosters, remaining_faab, weeks_remaining)
    })

    # All targets must have positive effective surplus
    if (nrow(result$targets) > 0) {
      expect_true(
        all(result$targets$effective_surplus > 0),
        info = sprintf(
          "Iteration %d: some targets have non-positive surplus: %s",
          i, paste(round(result$targets$effective_surplus[result$targets$effective_surplus <= 0], 2),
                   collapse = ", ")
        )
      )
    }
  }
})


# =============================================================================
# Property 21: Minor League Promotion Threshold Flagging
# =============================================================================

test_that("Property 21: Players with AB >= 110 on minor league contracts are flagged", {
  # **Validates: Requirements 7.6**
  #
  # For any player on a minor league contract, if their current AB >= 110,
  # the system shall flag them as "approaching promotion threshold."

  set.seed(2100)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 2100 + i
    set.seed(seed_i)

    n_players <- sample(5:20, 1)

    players <- data.frame(
      player_name = paste0("Minor_", seq_len(n_players)),
      ab = sample(0:160, n_players, replace = TRUE),
      ip = runif(n_players, min = 0, max = 30),  # below IP threshold
      is_minor_contract = rep(TRUE, n_players),
      position = sample(BATTER_POSITIONS, n_players, replace = TRUE),
      stringsAsFactors = FALSE
    )

    flags <- flag_promotion_threshold(players)

    for (j in seq_len(n_players)) {
      if (players$ab[j] >= 110) {
        expect_true(
          flags[j],
          info = sprintf(
            "Iteration %d, player %d: AB=%d >= 110 but not flagged",
            i, j, players$ab[j]
          )
        )
      } else {
        # AB < 110 and IP < 40, should NOT be flagged
        if (players$ip[j] < 40) {
          expect_false(
            flags[j],
            info = sprintf(
              "Iteration %d, player %d: AB=%d < 110, IP=%.1f < 40, but was flagged",
              i, j, players$ab[j], players$ip[j]
            )
          )
        }
      }
    }
  }
})

test_that("Property 21: Players with IP >= 40 on minor league contracts are flagged", {
  # **Validates: Requirements 7.6**
  #
  # For any player on a minor league contract, if their current IP >= 40,
  # the system shall flag them as "approaching promotion threshold."

  set.seed(2110)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 2110 + i
    set.seed(seed_i)

    n_players <- sample(5:20, 1)

    players <- data.frame(
      player_name = paste0("MinorP_", seq_len(n_players)),
      ab = sample(0:50, n_players, replace = TRUE),  # below AB threshold
      ip = runif(n_players, min = 0, max = 70),
      is_minor_contract = rep(TRUE, n_players),
      position = sample(PITCHER_POSITIONS, n_players, replace = TRUE),
      stringsAsFactors = FALSE
    )

    flags <- flag_promotion_threshold(players)

    for (j in seq_len(n_players)) {
      if (players$ip[j] >= 40) {
        expect_true(
          flags[j],
          info = sprintf(
            "Iteration %d, player %d: IP=%.1f >= 40 but not flagged",
            i, j, players$ip[j]
          )
        )
      } else {
        # IP < 40 and AB < 110, should NOT be flagged
        if (players$ab[j] < 110) {
          expect_false(
            flags[j],
            info = sprintf(
              "Iteration %d, player %d: IP=%.1f < 40, AB=%d < 110, but was flagged",
              i, j, players$ip[j], players$ab[j]
            )
          )
        }
      }
    }
  }
})

test_that("Property 21: Players NOT on minor league contracts are never flagged", {
  # **Validates: Requirements 7.6**
  #
  # If AB < 110 and IP < 40, they shall not be flagged. Also, non-minor-league
  # players should not be flagged regardless of AB/IP.

  set.seed(2120)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 2120 + i
    set.seed(seed_i)

    n_players <- sample(5:20, 1)

    # Non-minor-league players with high ABs/IPs — should NOT be flagged
    players <- data.frame(
      player_name = paste0("Major_", seq_len(n_players)),
      ab = sample(100:200, n_players, replace = TRUE),  # high ABs
      ip = runif(n_players, min = 30, max = 100),       # high IPs
      is_minor_contract = rep(FALSE, n_players),        # NOT minor league
      position = sample(LEAGUE_POSITIONS, n_players, replace = TRUE),
      stringsAsFactors = FALSE
    )

    flags <- flag_promotion_threshold(players)

    expect_true(
      all(!flags),
      info = sprintf(
        "Iteration %d: non-minor-league players should never be flagged, got %d flagged",
        i, sum(flags)
      )
    )
  }
})

test_that("Property 21: Exact threshold boundary — AB=110 flagged, AB=109 not flagged", {
  # **Validates: Requirements 7.6**
  #
  # Boundary test: AB exactly at 110 should be flagged, AB=109 should not.
  # Similarly IP=40 flagged, IP=39.9 not flagged.

  set.seed(2130)

  for (i in seq_len(WAIVER_TEST_ITERATIONS)) {
    seed_i <- 2130 + i
    set.seed(seed_i)

    # Test AB boundary
    players_ab <- data.frame(
      player_name = c("AtThreshold", "BelowThreshold"),
      ab = c(110, 109),
      ip = c(0, 0),
      is_minor_contract = c(TRUE, TRUE),
      position = c("SS", "OF"),
      stringsAsFactors = FALSE
    )
    flags_ab <- flag_promotion_threshold(players_ab)
    expect_true(flags_ab[1], info = sprintf("Iter %d: AB=110 should be flagged", i))
    expect_false(flags_ab[2], info = sprintf("Iter %d: AB=109 should NOT be flagged", i))

    # Test IP boundary
    players_ip <- data.frame(
      player_name = c("IPAtThreshold", "IPBelowThreshold"),
      ab = c(0, 0),
      ip = c(40, 39.9),
      is_minor_contract = c(TRUE, TRUE),
      position = c("SP", "RP"),
      stringsAsFactors = FALSE
    )
    flags_ip <- flag_promotion_threshold(players_ip)
    expect_true(flags_ip[1], info = sprintf("Iter %d: IP=40 should be flagged", i))
    expect_false(flags_ip[2], info = sprintf("Iter %d: IP=39.9 should NOT be flagged", i))
  }
})

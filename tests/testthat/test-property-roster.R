# Property-Based Test: Roster Optimizer (Properties 19, 20)
#
# **Validates: Requirements 8.1, 8.2, 8.5**
#
# Property 19: Lineup Validity — exactly 16 slots, one player per slot,
#   position-eligible, no injured players in active lineup.
#
# Property 20: Two-Start Pitcher Preference — prefer 2-start SP when
#   per-start quality is within 80% of the best 1-start SP.
#
# Uses replicated random test case generation (100+ iterations for Property 19,
# ~50 targeted scenarios for Property 20).

library(testthat)

# --- Constants matching roster_optimizer.R ---

POSITIONS_BATTER <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
POSITIONS_PITCHER <- c("SP", "RP")

# The 16 active slots: C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2
EXPECTED_SLOTS <- c("C", "1B", "2B", "3B", "SS",
                    "OF", "OF", "OF",
                    "U",
                    "SP", "SP", "SP", "SP", "SP",
                    "RP", "RP")

# --- Generators ---

#' Generate a random player name
generate_player_name <- function(id) {

  first_names <- c("Mike", "Aaron", "Juan", "Shohei", "Mookie",
                   "Ronald", "Trea", "Jose", "Freddie", "Corey",
                   "Bryce", "Rafael", "Marcus", "Julio", "Bobby",
                   "Kyle", "Gerrit", "Spencer", "Zack", "Corbin",
                   "Max", "Shane", "Dylan", "Justin", "Tyler",
                   "Chris", "Matt", "Logan", "Jake", "Nolan")
  last_names <- c("Trout", "Judge", "Soto", "Ohtani", "Betts",
                  "Acuna", "Turner", "Ramirez", "Freeman", "Seager",
                  "Harper", "Devers", "Semien", "Rodriguez", "Witt",
                  "Tucker", "Cole", "Strider", "Wheeler", "Burnes",
                  "Scherzer", "McClanahan", "Cease", "Verlander", "Glasnow",
                  "Sale", "Olson", "Webb", "deGrom", "Arenado")
  paste0(sample(first_names, 1), " ", sample(last_names, 1), " ", id)
}

#' Generate a random roster with enough players to fill 16 active slots
#' Returns a list with: roster_df, projections_df, injuries_df
generate_random_roster <- function(seed) {
  set.seed(seed)

  # Generate 24-30 players (enough to fill 16 active + some reserve)
  n_players <- sample(24:30, 1)

  # Ensure we have enough of each position type to fill all slots

  # Minimum: 1C, 1 1B, 1 2B, 1 3B, 1 SS, 3 OF, 1 DH-eligible, 5 SP, 2 RP
  # Generate with slight excess at each position
  position_pool <- c(
    rep("C", sample(1:3, 1)),
    rep("1B", sample(1:3, 1)),
    rep("2B", sample(1:3, 1)),
    rep("3B", sample(1:3, 1)),
    rep("SS", sample(1:3, 1)),
    rep("OF", sample(4:7, 1)),
    rep("SP", sample(6:9, 1)),
    rep("RP", sample(3:5, 1))
  )

  # Trim or extend to n_players
  if (length(position_pool) > n_players) {
    position_pool <- position_pool[seq_len(n_players)]
  } else if (length(position_pool) < n_players) {
    extra <- sample(c("OF", "SP", "1B", "RP"), n_players - length(position_pool), replace = TRUE)
    position_pool <- c(position_pool, extra)
  }

  # Shuffle positions
  position_pool <- sample(position_pool)

  # Build roster data frame
  players <- data.frame(
    team_name = rep("My Team", n_players),
    player_name = vapply(seq_len(n_players), generate_player_name, character(1)),
    eligible_positions = character(n_players),
    player_type = character(n_players),
    roster_position = character(n_players),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_players)) {
    primary_pos <- position_pool[i]

    # Assign player type
    if (primary_pos %in% c("SP", "RP")) {
      players$player_type[i] <- "Pitcher"
      # Pitchers may have dual eligibility (SP/RP)
      if (primary_pos == "SP" && runif(1) < 0.2) {
        players$eligible_positions[i] <- "SP, RP"
      } else {
        players$eligible_positions[i] <- primary_pos
      }
    } else {
      players$player_type[i] <- "Batter"
      # Batters may have multi-position eligibility
      extra_positions <- character(0)
      if (primary_pos == "OF" && runif(1) < 0.3) {
        extra_positions <- sample(c("LF", "CF", "RF"), sample(1:2, 1))
      }
      if (primary_pos != "OF" && runif(1) < 0.25) {
        # Add a secondary infield position
        other_infield <- setdiff(c("1B", "2B", "3B", "SS"), primary_pos)
        extra_positions <- sample(other_infield, 1)
      }
      all_pos <- unique(c(primary_pos, extra_positions))
      players$eligible_positions[i] <- paste(all_pos, collapse = ", ")
    }

    players$roster_position[i] <- primary_pos
  }

  # Generate projections
  projections <- data.frame(
    player_name = players$player_name,
    proj_pts_per_week = runif(n_players, min = 5, max = 80),
    stringsAsFactors = FALSE
  )

  # Generate injuries (0-4 random players injured)
  n_injured <- sample(0:4, 1)
  injuries <- NULL
  if (n_injured > 0) {
    injured_idx <- sample(seq_len(n_players), min(n_injured, n_players))
    injuries <- data.frame(
      player_name = players$player_name[injured_idx],
      injury = rep("IL", length(injured_idx)),
      pro_status = rep("IL", length(injured_idx)),
      stringsAsFactors = FALSE
    )
  }

  list(
    roster = players,
    projections = projections,
    injuries = injuries,
    n_players = n_players,
    n_injured = n_injured
  )
}

#' Generate a pitcher scenario for Property 20 (two-start preference)
#' Creates a scenario where a 2-start SP has per-start quality within 80%
#' of the best 1-start SP.
generate_two_start_scenario <- function(seed) {
  set.seed(seed)

  # Create 7+ pitchers: mix of 1-start and 2-start SPs plus RPs
  n_sp <- sample(6:9, 1)
  n_rp <- sample(2:4, 1)
  n_total <- n_sp + n_rp

  pitchers <- data.frame(
    player_name = vapply(seq_len(n_total), function(i) generate_player_name(i + 1000),
                         character(1)),
    eligible_positions = c(rep("SP", n_sp), rep("RP", n_rp)),
    player_type = rep("Pitcher", n_total),
    stringsAsFactors = FALSE
  )

  # Best 1-start SP: high per-start quality
  best_1start_quality <- runif(1, min = 20, max = 40)

  # Generate per-start qualities for all SPs
  sp_qualities <- runif(n_sp, min = 10, max = best_1start_quality * 0.95)

  # Make sure at least one is the "best 1-start SP"
  best_1start_idx <- 1
  sp_qualities[best_1start_idx] <- best_1start_quality

  # Pick one SP to be the 2-start pitcher with quality within 80%
  two_start_idx <- sample(2:n_sp, 1)
  # Set quality between 80% and 99% of best (within threshold)
  quality_fraction <- runif(1, min = 0.80, max = 0.99)
  sp_qualities[two_start_idx] <- best_1start_quality * quality_fraction

  # Assign starts: 1 for all except the designated 2-start pitcher
  starts <- rep(1L, n_sp)
  starts[two_start_idx] <- 2L

  # Build schedule
  schedule <- data.frame(
    player_name = pitchers$player_name[seq_len(n_sp)],
    starts_this_week = starts,
    stringsAsFactors = FALSE
  )

  # Build projections using per-start quality × starts
  proj_pts <- numeric(n_total)
  for (i in seq_len(n_sp)) {
    proj_pts[i] <- sp_qualities[i] * starts[i]
  }
  # RPs get moderate projections

  for (i in (n_sp + 1):n_total) {
    proj_pts[i] <- runif(1, min = 5, max = 15)
  }

  projections <- data.frame(
    player_name = pitchers$player_name,
    proj_pts_per_week = proj_pts,
    proj_pts_per_start = c(sp_qualities, rep(NA, n_rp)),
    stringsAsFactors = FALSE
  )

  list(
    pitchers = pitchers,
    projections = projections,
    schedule = schedule,
    two_start_idx = two_start_idx,
    two_start_player = pitchers$player_name[two_start_idx],
    best_1start_player = pitchers$player_name[best_1start_idx],
    two_start_quality = sp_qualities[two_start_idx],
    best_1start_quality = best_1start_quality,
    quality_ratio = sp_qualities[two_start_idx] / best_1start_quality,
    n_sp = n_sp
  )
}

# --- Property 19: Lineup Validity ---

test_that("Property 19: Optimized lineup fills exactly 16 active slots", {
  # **Validates: Requirements 8.1, 8.2**
  set.seed(100)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    scenario <- generate_random_roster(seed = i * 7)

    # Suppress logging messages during test
    result <- suppressMessages(
      optimize_lineup(
        my_team = "My Team",
        rosters = scenario$roster,
        projections = scenario$projections,
        injuries = scenario$injuries
      )
    )

    # Count active slots (status == "active" or "unfilled")
    active_rows <- result[result$status %in% c("active", "unfilled"), , drop = FALSE]
    expect_equal(
      nrow(active_rows), 16,
      info = paste0("Iteration ", i, ": expected 16 active slots, got ", nrow(active_rows))
    )
  }
})

test_that("Property 19: No duplicate players in active lineup", {
  # **Validates: Requirements 8.1, 8.2**
  set.seed(200)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    scenario <- generate_random_roster(seed = i * 13)

    result <- suppressMessages(
      optimize_lineup(
        my_team = "My Team",
        rosters = scenario$roster,
        projections = scenario$projections,
        injuries = scenario$injuries
      )
    )

    active_players <- result[result$status == "active", "player_name", drop = TRUE]
    # Remove NAs (unfilled slots)
    active_players <- active_players[!is.na(active_players)]

    expect_equal(
      length(active_players),
      length(unique(active_players)),
      info = paste0("Iteration ", i, ": duplicate player found in active lineup. ",
                    "Players: ", paste(active_players[duplicated(active_players)], collapse = ", "))
    )
  }
})

test_that("Property 19: Each active player is eligible for their assigned slot", {
  # **Validates: Requirements 8.1, 8.2**
  set.seed(300)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    scenario <- generate_random_roster(seed = i * 17)

    result <- suppressMessages(
      optimize_lineup(
        my_team = "My Team",
        rosters = scenario$roster,
        projections = scenario$projections,
        injuries = scenario$injuries
      )
    )

    active_rows <- result[result$status == "active", , drop = FALSE]

    for (j in seq_len(nrow(active_rows))) {
      slot <- active_rows$slot[j]
      player_name <- active_rows$player_name[j]
      elig_str <- active_rows$eligible_positions[j]

      if (is.na(player_name) || is.na(elig_str)) next

      positions <- parse_eligible_positions(elig_str)
      eligible <- is_eligible_for_slot(positions, slot)

      expect_true(
        eligible,
        info = paste0("Iteration ", i, ", row ", j, ": player '", player_name,
                      "' with positions [", elig_str, "] assigned to slot '",
                      slot, "' but is not eligible")
      )
    }
  }
})

test_that("Property 19: No injured players in active lineup", {
  # **Validates: Requirements 8.1, 8.2**
  set.seed(400)
  n_iterations <- 120

  for (i in seq_len(n_iterations)) {
    scenario <- generate_random_roster(seed = i * 23)

    # Ensure there are some injuries to test
    if (is.null(scenario$injuries) || nrow(scenario$injuries) == 0) {
      # Force at least 1-2 injuries
      n_force <- sample(1:3, 1)
      injured_idx <- sample(seq_len(scenario$n_players), min(n_force, scenario$n_players))
      scenario$injuries <- data.frame(
        player_name = scenario$roster$player_name[injured_idx],
        injury = rep("IL", length(injured_idx)),
        pro_status = rep("IL", length(injured_idx)),
        stringsAsFactors = FALSE
      )
    }

    result <- suppressMessages(
      optimize_lineup(
        my_team = "My Team",
        rosters = scenario$roster,
        projections = scenario$projections,
        injuries = scenario$injuries
      )
    )

    # Active players should NOT include any injured players
    active_players <- result[result$status == "active", "player_name", drop = TRUE]
    active_players <- active_players[!is.na(active_players)]
    injured_names <- tolower(scenario$injuries$player_name)

    overlap <- active_players[tolower(active_players) %in% injured_names]

    expect_equal(
      length(overlap), 0,
      info = paste0("Iteration ", i, ": injured player(s) found in active lineup: ",
                    paste(overlap, collapse = ", "))
    )
  }
})

# --- Property 20: Two-Start Pitcher Preference ---

test_that("Property 20: 2-start SP preferred when per-start quality within 80% of best 1-start", {
  # **Validates: Requirements 8.5**
  set.seed(500)
  n_iterations <- 50

  for (i in seq_len(n_iterations)) {
    scenario <- generate_two_start_scenario(seed = i * 31)

    result <- suppressMessages(
      optimize_pitchers(
        available_pitchers = scenario$pitchers,
        projections = scenario$projections,
        schedule = scenario$schedule
      )
    )

    # The 2-start SP should be selected (in the SP slots)
    selected_sps <- result[result$slot == "SP", "player_name", drop = TRUE]

    # Verify the 2-start pitcher was selected
    two_start_selected <- scenario$two_start_player %in% selected_sps

    expect_true(
      two_start_selected,
      info = paste0(
        "Iteration ", i, ": 2-start pitcher '", scenario$two_start_player,
        "' (quality ratio = ", round(scenario$quality_ratio, 3),
        ", per-start = ", round(scenario$two_start_quality, 2),
        ") was NOT selected over 1-start pitchers. ",
        "Best 1-start quality = ", round(scenario$best_1start_quality, 2),
        ". Selected SPs: ", paste(selected_sps, collapse = ", ")
      )
    )
  }
})

test_that("Property 20: 2-start SP not necessarily preferred when quality below 80% threshold", {
  # **Validates: Requirements 8.5**
  # This tests the inverse: when a 2-start SP has per-start quality
  # BELOW 80% of the best 1-start SP, total-points ordering decides.
  set.seed(600)
  n_iterations <- 50

  for (i in seq_len(n_iterations)) {
    set.seed(i * 37)

    # Create scenario where 2-start SP is clearly worse (below 80%)
    n_sp <- 6
    n_rp <- 2
    n_total <- n_sp + n_rp

    pitchers <- data.frame(
      player_name = vapply(seq_len(n_total), function(j) generate_player_name(j + 2000 + i * 10),
                           character(1)),
      eligible_positions = c(rep("SP", n_sp), rep("RP", n_rp)),
      player_type = rep("Pitcher", n_total),
      stringsAsFactors = FALSE
    )

    # Best 1-start SP: high quality
    best_quality <- runif(1, min = 30, max = 50)

    # Other 1-start SPs: decent quality (between 70-95% of best)
    other_qualities <- runif(n_sp - 2, min = best_quality * 0.70, max = best_quality * 0.95)

    # 2-start SP: well below 80% threshold (between 40-75%)
    bad_2start_quality <- best_quality * runif(1, min = 0.40, max = 0.75)

    # Assign: idx 1 is best, idx 2 is bad 2-start, rest are other 1-starts
    sp_qualities <- c(best_quality, bad_2start_quality, other_qualities)
    starts <- c(1L, 2L, rep(1L, n_sp - 2))

    schedule <- data.frame(
      player_name = pitchers$player_name[seq_len(n_sp)],
      starts_this_week = starts,
      stringsAsFactors = FALSE
    )

    # Effective weekly pts: quality * starts
    proj_pts <- c(
      sp_qualities * starts,
      runif(n_rp, min = 5, max = 15)
    )

    projections <- data.frame(
      player_name = pitchers$player_name,
      proj_pts_per_week = proj_pts,
      proj_pts_per_start = c(sp_qualities, rep(NA, n_rp)),
      stringsAsFactors = FALSE
    )

    result <- suppressMessages(
      optimize_pitchers(
        available_pitchers = pitchers,
        projections = projections,
        schedule = schedule
      )
    )

    # When the 2-start pitcher's total weekly pts (2 * bad_quality)
    # is lower than the 5th best 1-start pitcher, it should NOT be selected.
    # But when 2 * bad_quality is still competitive, it may still be selected
    # based on total weekly value. The key property is that the optimizer
    # uses effective weekly points, which is the correct behavior.
    selected_sps <- result[result$slot == "SP", "player_name", drop = TRUE]

    # Verify the result is ordered by effective weekly points
    selected_pts <- result[result$slot == "SP", "proj_pts_per_week", drop = TRUE]

    # Selected pitchers should be in descending order of effective pts
    expect_true(
      all(diff(selected_pts) <= 0) || length(selected_pts) <= 1,
      info = paste0("Iteration ", i, ": SP selection not in descending order of weekly pts")
    )
  }
})

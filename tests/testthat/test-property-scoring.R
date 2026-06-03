# Property-Based Test: Scoring Computation Correctness (Property 5)
#
# **Validates: Requirements 4.6, 5.1**
#
# Property 5: For any valid batting stat line, compute_batting_points(stats, weights)
# shall equal the sum of each stat multiplied by its corresponding weight.
# The same additive property holds for pitching points.
#
# Since {quickcheck} is not available, we use replicated random stat line
# generation (100 iterations) to verify the property across many inputs.

library(testthat)

# Source scoring module — use path relative to this test file's location
# (tests/testthat/test-property-scoring.R -> ../../R/utils/scoring.R)
scoring_path <- file.path(
  normalizePath(file.path("..", ".."), mustWork = FALSE),
  "R", "utils", "scoring.R"
)
if (!file.exists(scoring_path)) {
  # Fallback when run from project root directly
  scoring_path <- file.path("R", "utils", "scoring.R")
}
source(scoring_path)

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

# --- Generators: random stat lines within realistic bounds ---

generate_batting_stats <- function() {
  list(
    singles          = sample(0:150, 1),
    doubles          = sample(0:50, 1),
    triples          = sample(0:15, 1),
    hr               = sample(0:55, 1),
    grand_slam_bonus = sample(0:5, 1),
    cycle            = sample(0:2, 1),
    runs             = sample(0:130, 1),
    rbi              = sample(0:150, 1),
    bb               = sample(0:120, 1),
    hbp              = sample(0:25, 1),
    sb               = sample(0:70, 1),
    cs               = sample(0:20, 1),
    k_batter         = sample(0:200, 1)
  )
}

generate_pitching_stats <- function() {
  list(
    innings          = sample(0:250, 1),
    k_pitcher        = sample(0:300, 1),
    wins             = sample(0:22, 1),
    losses           = sample(0:20, 1),
    saves            = sample(0:50, 1),
    holds            = sample(0:40, 1),
    quality_starts   = sample(0:35, 1),
    complete_games   = sample(0:5, 1),
    no_hitter        = sample(0:2, 1),
    perfect_game     = sample(0:1, 1),
    hits_allowed     = sample(0:250, 1),
    earned_runs      = sample(0:120, 1),
    walks_issued     = sample(0:100, 1),
    intentional_walks = sample(0:15, 1)
  )
}

# --- Manual expected computation (reference implementation) ---

manual_batting_points <- function(stats, weights) {
  total <- 0
  for (stat_name in names(stats)) {
    if (!is.null(weights[[stat_name]])) {
      total <- total + stats[[stat_name]] * weights[[stat_name]]
    }
  }
  total
}

manual_pitching_points <- function(stats, weights) {
  total <- 0
  for (stat_name in names(stats)) {
    if (!is.null(weights[[stat_name]])) {
      total <- total + stats[[stat_name]] * weights[[stat_name]]
    }
  }
  total
}

# --- Property Tests ---

test_that("Property 5 (Batting): compute_batting_points equals sum of stat * weight for random stat lines", {
  # **Validates: Requirements 4.6, 5.1**
  set.seed(42)
  n_iterations <- 100

  for (i in seq_len(n_iterations)) {
    stats <- generate_batting_stats()
    expected <- manual_batting_points(stats, batting_weights)
    actual <- compute_batting_points(stats, batting_weights)

    expect_equal(
      actual, expected,
      info = paste0(
        "Iteration ", i, ": batting points mismatch. ",
        "Stats: ", paste(names(stats), stats, sep = "=", collapse = ", ")
      )
    )
  }
})

test_that("Property 5 (Pitching): compute_pitching_points equals sum of stat * weight for random stat lines", {
  # **Validates: Requirements 4.6, 5.1**
  set.seed(123)
  n_iterations <- 100

  for (i in seq_len(n_iterations)) {
    stats <- generate_pitching_stats()
    expected <- manual_pitching_points(stats, pitching_weights)
    actual <- compute_pitching_points(stats, pitching_weights)

    expect_equal(
      actual, expected,
      info = paste0(
        "Iteration ", i, ": pitching points mismatch. ",
        "Stats: ", paste(names(stats), stats, sep = "=", collapse = ", ")
      )
    )
  }
})

test_that("Property 5 (Batting): additive property holds — subset sums equal total", {
  # **Validates: Requirements 4.6, 5.1**
  # For any stat line, computing points on two disjoint subsets and summing

  # must equal computing on the full stat line.
  set.seed(99)
  n_iterations <- 50

  for (i in seq_len(n_iterations)) {
    stats <- generate_batting_stats()
    stat_names <- names(stats)

    # Split stats into two random disjoint subsets
    split_idx <- sample(seq_along(stat_names), length(stat_names) %/% 2)
    subset_a <- stats[split_idx]
    subset_b <- stats[-split_idx]

    pts_a <- compute_batting_points(subset_a, batting_weights)
    pts_b <- compute_batting_points(subset_b, batting_weights)
    pts_total <- compute_batting_points(stats, batting_weights)

    expect_equal(
      pts_a + pts_b, pts_total,
      info = paste0("Iteration ", i, ": additivity failed for batting stats")
    )
  }
})

test_that("Property 5 (Pitching): additive property holds — subset sums equal total", {
  # **Validates: Requirements 4.6, 5.1**
  set.seed(77)
  n_iterations <- 50

  for (i in seq_len(n_iterations)) {
    stats <- generate_pitching_stats()
    stat_names <- names(stats)

    split_idx <- sample(seq_along(stat_names), length(stat_names) %/% 2)
    subset_a <- stats[split_idx]
    subset_b <- stats[-split_idx]

    pts_a <- compute_pitching_points(subset_a, pitching_weights)
    pts_b <- compute_pitching_points(subset_b, pitching_weights)
    pts_total <- compute_pitching_points(stats, pitching_weights)

    expect_equal(
      pts_a + pts_b, pts_total,
      info = paste0("Iteration ", i, ": additivity failed for pitching stats")
    )
  }
})

test_that("Property 5 (Edge): zero stat line produces zero points", {
  # **Validates: Requirements 4.6, 5.1**
  zero_batting <- lapply(batting_weights, function(x) 0)
  names(zero_batting) <- names(batting_weights)
  expect_equal(compute_batting_points(zero_batting, batting_weights), 0)

  zero_pitching <- lapply(pitching_weights, function(x) 0)
  names(zero_pitching) <- names(pitching_weights)
  expect_equal(compute_pitching_points(zero_pitching, pitching_weights), 0)
})

test_that("Property 5 (Edge): single stat produces stat * weight", {
  # **Validates: Requirements 4.6, 5.1**
  # For each individual batting stat, check isolation
  for (stat_name in names(batting_weights)) {
    val <- sample(1:50, 1)
    stats <- setNames(list(val), stat_name)
    expected <- val * batting_weights[[stat_name]]
    actual <- compute_batting_points(stats, batting_weights)
    expect_equal(actual, expected,
      info = paste0("Single batting stat '", stat_name, "' = ", val))
  }

  # For each individual pitching stat, check isolation
  for (stat_name in names(pitching_weights)) {
    val <- sample(1:50, 1)
    stats <- setNames(list(val), stat_name)
    expected <- val * pitching_weights[[stat_name]]
    actual <- compute_pitching_points(stats, pitching_weights)
    expect_equal(actual, expected,
      info = paste0("Single pitching stat '", stat_name, "' = ", val))
  }
})

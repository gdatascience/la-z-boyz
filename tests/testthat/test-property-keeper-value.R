# Property-based tests for keeper_value.R (Property 13: Keeper Salary Projection)
# **Validates: Requirements 5.6, 5.7, 6.4**
#
# Tests:
# 1. Standard contract: year N salary = S + 4*N
# 2. Minor league track: full salary progression $0→$1→$2→$3→$7→$11→...
# 3. Keeper surplus for year N = (projected_value_N − projected_salary_N) × discount_rate^N

library(testthat)

source(here::here("R", "utils", "keeper_value.R"))

set.seed(42)

# ---------------------------------------------------------------------------
# Property 1: Standard contract salary projection
# For any standard contract with salary S, year N salary = S + 4*N
# ---------------------------------------------------------------------------
test_that("Property 13.1: Standard contract year N salary = S + 4*N", {
  n_cases <- 150

  for (i in seq_len(n_cases)) {
    salary <- sample(1:50, 1)
    years_ahead <- sample(1:10, 1)

    result <- project_keeper_salary(
      current_salary = salary,
      is_minor_contract = FALSE,
      years_ahead = years_ahead
    )

    # Verify each year
    for (n in seq_len(years_ahead)) {
      expected <- salary + 4 * n
      expect_equal(
        result[n], expected,
        info = sprintf("S=%d, year N=%d: expected %d, got %s", salary, n, expected, result[n])
      )
    }
  }
})

# ---------------------------------------------------------------------------
# Property 2: Minor league track salary progression
# For minor league track at year Y (0-3), verify full salary progression:
# $0→$1→$2→$3→$7→$11→...
# ---------------------------------------------------------------------------
test_that("Property 13.2: Minor league track full salary progression", {
  n_cases <- 150

  for (i in seq_len(n_cases)) {
    minor_track_year <- sample(0:3, 1)
    years_ahead <- sample(1:10, 1)

    result <- project_keeper_salary(
      current_salary = 0,
      is_minor_contract = TRUE,
      minor_track_year = minor_track_year,
      years_ahead = years_ahead
    )

    # Verify each projected year
    for (n in seq_len(years_ahead)) {
      future_track_year <- minor_track_year + n

      if (future_track_year <= 3) {
        # Still on the minor league salary track: $0, $1, $2, $3
        expected <- future_track_year
      } else {
        # Past year 3: $3 base + $4 per year beyond track
        years_past_track <- future_track_year - 3
        expected <- 3 + 4 * years_past_track
      }

      expect_equal(
        result[n], expected,
        info = sprintf(
          "minor_track_year=%d, years_ahead=%d, year n=%d: expected %d, got %s",
          minor_track_year, years_ahead, n, expected, result[n]
        )
      )
    }
  }
})

# Specific deterministic checks for the documented progression
test_that("Property 13.2b: Minor league track known progressions", {
  # From year 0: $0→$1→$2→$3→$7→$11
  result_y0 <- project_keeper_salary(0, is_minor_contract = TRUE, minor_track_year = 0, years_ahead = 5)
  expect_equal(result_y0, c(1, 2, 3, 7, 11))

  # From year 1: $1→$2→$3→$7→$11
  result_y1 <- project_keeper_salary(0, is_minor_contract = TRUE, minor_track_year = 1, years_ahead = 5)
  expect_equal(result_y1, c(2, 3, 7, 11, 15))

  # From year 2: $2→$3→$7→$11→$15

  result_y2 <- project_keeper_salary(0, is_minor_contract = TRUE, minor_track_year = 2, years_ahead = 5)
  expect_equal(result_y2, c(3, 7, 11, 15, 19))

  # From year 3: $3→$7→$11→$15→$19
  result_y3 <- project_keeper_salary(0, is_minor_contract = TRUE, minor_track_year = 3, years_ahead = 5)
  expect_equal(result_y3, c(7, 11, 15, 19, 23))
})

# ---------------------------------------------------------------------------
# Property 3: Keeper surplus for year N
# surplus_N = (projected_value_N − projected_salary_N) × discount_rate^N
# ---------------------------------------------------------------------------
test_that("Property 13.3: Keeper surplus for year N = (value_N - salary_N) * discount_rate^N", {
  n_cases <- 150

  for (i in seq_len(n_cases)) {
    n_years <- sample(1:8, 1)

    # Generate random projected values (positive, realistic range)
    projected_values <- runif(n_years, min = 5, max = 60)

    # Generate random projected salaries (positive, realistic range)
    projected_salaries <- runif(n_years, min = 1, max = 50)

    # Random discount rate between 0.5 and 1.0
    discount_rate <- runif(1, min = 0.5, max = 1.0)

    result <- compute_keeper_surplus(projected_values, projected_salaries, discount_rate)

    # Verify annual_surplus
    expected_annual <- projected_values - projected_salaries
    expect_equal(
      result$annual_surplus, expected_annual,
      tolerance = 1e-10,
      info = sprintf("Case %d: annual_surplus mismatch", i)
    )

    # Verify total_surplus (undiscounted sum)
    expected_total <- sum(expected_annual)
    expect_equal(
      result$total_surplus, expected_total,
      tolerance = 1e-10,
      info = sprintf("Case %d: total_surplus mismatch", i)
    )

    # Verify NPV surplus: each year N's surplus * discount_rate^N
    # Year indexing: year 1 gets discount_rate^1, year 2 gets discount_rate^2, etc.
    expected_npv <- sum(expected_annual * discount_rate^(seq_len(n_years)))
    expect_equal(
      result$npv_surplus, expected_npv,
      tolerance = 1e-10,
      info = sprintf("Case %d: npv_surplus mismatch (discount_rate=%.4f)", i, discount_rate)
    )
  }
})

# ---------------------------------------------------------------------------
# Integration: Standard salary projection feeds into surplus calculation
# ---------------------------------------------------------------------------
test_that("Property 13 Integration: salary projection + surplus computation", {
  n_cases <- 100

  for (i in seq_len(n_cases)) {
    salary <- sample(1:40, 1)
    years_ahead <- sample(2:6, 1)
    discount_rate <- runif(1, min = 0.7, max = 1.0)

    # Project salaries using the standard contract formula
    projected_salaries <- project_keeper_salary(
      current_salary = salary,
      is_minor_contract = FALSE,
      years_ahead = years_ahead
    )

    # Generate random projected values
    projected_values <- runif(years_ahead, min = 10, max = 60)

    # Compute surplus
    result <- compute_keeper_surplus(projected_values, projected_salaries, discount_rate)

    # Verify property: for each year N, the discounted surplus contribution equals
    # (projected_value_N - (salary + 4*N)) * discount_rate^N
    for (n in seq_len(years_ahead)) {
      expected_salary_n <- salary + 4 * n
      expected_surplus_n <- (projected_values[n] - expected_salary_n) * discount_rate^n

      actual_discounted_surplus_n <- result$annual_surplus[n] * discount_rate^n

      expect_equal(
        actual_discounted_surplus_n, expected_surplus_n,
        tolerance = 1e-10,
        info = sprintf(
          "Case %d, year %d: salary=%d, value=%.2f, disc_rate=%.4f",
          i, n, salary, projected_values[n], discount_rate
        )
      )
    }
  }
})

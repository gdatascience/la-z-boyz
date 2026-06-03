#' Keeper Value Utilities
#'
#' Functions for projecting keeper salary escalation over multiple years
#' and computing multi-year surplus value for keeper analysis.

#' Project keeper salary over N years
#'
#' Projects future salaries based on the league's keeper escalation rules:
#' - Standard contract: salary + $4 per year of keeping
#' - Minor league track: $0 (in minors) -> $1* (year 1) -> $2* (year 2) ->
#'   $3* (year 3) -> then +$4/year from $3 base
#'
#' @param current_salary Current salary (numeric)
#' @param is_minor_contract Logical: is this a minor league contract (asterisk)?
#' @param minor_track_year Integer 0-3: current year in minor league track
#'   (0 = still in minors at $0, 1 = first year promoted at $1, etc.)
#' @param years_ahead Integer: how many years to project
#' @return Numeric vector of projected salaries for each future year (length = years_ahead)
#' @examples
#' # Standard contract at $10: projects $14, $18, $22, $26, $30
#' project_keeper_salary(10, is_minor_contract = FALSE, years_ahead = 5)
#'
#' # Minor league at year 0 ($0): projects $1, $2, $3, $7, $11
#' project_keeper_salary(0, is_minor_contract = TRUE, minor_track_year = 0, years_ahead = 5)
project_keeper_salary <- function(current_salary, is_minor_contract = FALSE,
                                   minor_track_year = 0, years_ahead = 5) {
  if (!is.numeric(current_salary) || length(current_salary) != 1) {
    stop("current_salary must be a single numeric value")
  }
  if (!is.numeric(years_ahead) || length(years_ahead) != 1 || years_ahead < 1) {
    stop("years_ahead must be a positive integer")
  }
  if (!is.logical(is_minor_contract) || length(is_minor_contract) != 1) {
    stop("is_minor_contract must be a single logical value")
  }
  if (!is.numeric(minor_track_year) || length(minor_track_year) != 1 ||
      minor_track_year < 0 || minor_track_year > 3) {
    stop("minor_track_year must be an integer between 0 and 3")
  }

  years_ahead <- as.integer(years_ahead)
  minor_track_year <- as.integer(minor_track_year)

  salaries <- numeric(years_ahead)

  if (!is_minor_contract) {
    # Standard contract: salary + $4 per year
    for (i in seq_len(years_ahead)) {
      salaries[i] <- current_salary + 4 * i
    }
  } else {
    # Minor league track progression
    # The minor league salary track is: $0 -> $1 -> $2 -> $3 -> then +$4/year from $3
    # minor_track_year indicates current position in the track (0-3)
    minor_salaries <- c(0, 1, 2, 3)  # track positions 0, 1, 2, 3

    for (i in seq_len(years_ahead)) {
      future_track_year <- minor_track_year + i

      if (future_track_year <= 3) {
        # Still on the minor league salary track
        salaries[i] <- minor_salaries[future_track_year + 1]
      } else {
        # Past year 3: enters +$4/year escalation from the $3 base
        years_past_track <- future_track_year - 3
        salaries[i] <- 3 + 4 * years_past_track
      }
    }
  }

  salaries
}


#' Compute multi-year surplus value for keeper analysis
#'
#' Calculates annual surplus (value minus salary), total surplus, and
#' net present value of surplus using a discount rate for future years.
#'
#' @param projected_values Numeric vector of projected dollar values per year
#' @param projected_salaries Numeric vector of projected salaries per year
#'   (must be same length as projected_values)
#' @param discount_rate Annual discount rate for future value (default 0.9,
#'   meaning each future year is worth 90% of the previous)
#' @return List with:
#'   \describe{
#'     \item{annual_surplus}{Numeric vector: value - salary for each year}
#'     \item{total_surplus}{Numeric: sum of annual surplus (undiscounted)}
#'     \item{npv_surplus}{Numeric: net present value of surplus with discounting}
#'   }
#' @examples
#' values <- c(30, 28, 25, 22, 20)
#' salaries <- c(14, 18, 22, 26, 30)
#' compute_keeper_surplus(values, salaries, discount_rate = 0.9)
compute_keeper_surplus <- function(projected_values, projected_salaries,
                                    discount_rate = 0.9) {
  if (!is.numeric(projected_values) || !is.numeric(projected_salaries)) {
    stop("projected_values and projected_salaries must be numeric vectors")
  }
  if (length(projected_values) != length(projected_salaries)) {
    stop("projected_values and projected_salaries must have the same length")
  }
  if (!is.numeric(discount_rate) || length(discount_rate) != 1 ||
      discount_rate <= 0 || discount_rate > 1) {
    stop("discount_rate must be a single numeric value between 0 (exclusive) and 1 (inclusive)")
  }

  n_years <- length(projected_values)

  # Annual surplus: value minus salary for each year

  annual_surplus <- projected_values - projected_salaries

  # Total surplus: undiscounted sum
  total_surplus <- sum(annual_surplus)

  # NPV surplus: discount each year's surplus by discount_rate^year
  # Year 1 is discounted by discount_rate^1, year 2 by discount_rate^2, etc.
  discount_factors <- discount_rate^(seq_len(n_years))
  npv_surplus <- sum(annual_surplus * discount_factors)

  list(
    annual_surplus = annual_surplus,
    total_surplus = total_surplus,
    npv_surplus = npv_surplus
  )
}

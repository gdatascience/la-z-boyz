# R/utils/salary_rules.R
# Salary cap validation and constraint checking for the La-Z-Boyz of Summer league.
#
# Cap values (from league constitution):
#   auction   = $260 (draft day budget)
#   in_season = $300 (maximum in-season salary)
#   keeper    = $80  (maximum total keeper salary at keeper deadline)

# Named vector of salary cap limits
SALARY_CAPS <- c(

  auction   = 260,
  in_season = 300,

  keeper    = 80
)

#' Check if a roster is salary-cap compliant
#'
#' Sums the salary column from a team roster data frame and checks compliance
#' against the specified cap type.
#'
#' @param roster_df Team roster data frame with a \code{salary} column (numeric).
#' @param cap_type One of \code{"auction"} (260), \code{"in_season"} (300),
#'   or \code{"keeper"} (80). Defaults to \code{"in_season"}.
#' @return A list with:
#'   \describe{
#'     \item{compliant}{Logical. TRUE if total salary is at or below the cap.}
#'     \item{total_salary}{Numeric. Sum of all salaries on the roster.}
#'     \item{remaining_cap}{Numeric. Cap limit minus total salary (can be negative).}
#'   }
#' @examples
#' roster <- data.frame(player_name = c("A", "B"), salary = c(15, 25))
#' check_salary_cap(roster, "in_season")
check_salary_cap <- function(roster_df, cap_type = "in_season") {
  # Validate cap_type

  valid_types <- names(SALARY_CAPS)
  if (!cap_type %in% valid_types) {
    stop(
      sprintf(
        "Invalid cap_type '%s'. Must be one of: %s",
        cap_type,
        paste(valid_types, collapse = ", ")
      )
    )
  }

  # Validate roster_df has salary column

  if (!"salary" %in% names(roster_df)) {
    stop("roster_df must contain a 'salary' column")
  }

  cap_limit <- SALARY_CAPS[[cap_type]]
  total_salary <- sum(roster_df$salary, na.rm = TRUE)
  remaining_cap <- cap_limit - total_salary


  list(
    compliant     = total_salary <= cap_limit,
    total_salary  = total_salary,
    remaining_cap = remaining_cap
  )
}

#' Compute salary impact of a transaction
#'
#' Calculates the net salary change from adding and removing players, and
#' determines whether the resulting total is compliant with the in-season
#' salary cap ($300).
#'
#' @param players_in Data frame of players being added, with a \code{salary}
#'   column (numeric). Can be \code{NULL} or a zero-row data frame if no
#'   players are being added.
#' @param players_out Data frame of players being removed, with a \code{salary}
#'   column (numeric). Can be \code{NULL} or a zero-row data frame if no
#'   players are being removed.
#' @param current_total Numeric. The team's current total salary before the
#'   transaction.
#' @return A list with:
#'   \describe{
#'     \item{net_change}{Numeric. Salary added minus salary removed.}
#'     \item{new_total}{Numeric. current_total + net_change.}
#'     \item{cap_compliant}{Logical. TRUE if new_total <= $300 (in-season cap).}
#'   }
#' @examples
#' incoming <- data.frame(player_name = "C", salary = 10)
#' outgoing <- data.frame(player_name = "D", salary = 5)
#' compute_salary_impact(incoming, outgoing, current_total = 250)
compute_salary_impact <- function(players_in, players_out, current_total) {
  # Sum incoming salaries

  salary_in <- if (is.null(players_in) || nrow(players_in) == 0) {
    0
  } else {
    if (!"salary" %in% names(players_in)) {
      stop("players_in must contain a 'salary' column")
    }
    sum(players_in$salary, na.rm = TRUE)
  }

  # Sum outgoing salaries
  salary_out <- if (is.null(players_out) || nrow(players_out) == 0) {
    0
  } else {
    if (!"salary" %in% names(players_out)) {
      stop("players_out must contain a 'salary' column")
    }
    sum(players_out$salary, na.rm = TRUE)
  }

  net_change <- salary_in - salary_out
  new_total <- current_total + net_change
  cap_compliant <- new_total <= SALARY_CAPS[["in_season"]]

  list(
    net_change    = net_change,
    new_total     = new_total,
    cap_compliant = cap_compliant
  )
}

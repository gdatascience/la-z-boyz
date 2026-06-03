# Property-Based Test: Constitution Validation Completeness (Property 3)
#
# **Validates: Requirements 2.10**
#
# Property 3: For any valid league constitution R list containing all required
# fields, the validation function shall return TRUE. For any constitution with
# one or more required fields removed, the validation function shall return
# FALSE and identify the missing field(s).
#
# Since {quickcheck} is not available, we use manual randomization with
# set.seed() and replicated iterations (100+) to verify the property.

library(testthat)

# --- Source only the validate_constitution function ---
# parse_constitution.R has top-level execution code (HTML parsing, file I/O).
# We extract just the validate_constitution function by sourcing in a local env
# and only keeping the function definitions.

local({
  # Read the source file and extract only function definitions
  src_path <- file.path(
    normalizePath(file.path("..", ".."), mustWork = FALSE),
    "R", "ingest", "parse_constitution.R"
  )
  if (!file.exists(src_path)) {
    src_path <- file.path("R", "ingest", "parse_constitution.R")
  }

  lines <- readLines(src_path)

  # Find the validate_constitution function block
  func_start <- grep("^validate_constitution <- function", lines)
  if (length(func_start) == 0) {
    stop("Could not find validate_constitution function in parse_constitution.R")
  }

  # Also grab load_constitution if present (starts after validate_constitution ends)
  load_start <- grep("^load_constitution <- function", lines)

  # Find end of validate_constitution: next top-level function or top-level assignment
  # We'll extract from func_start to the line before load_constitution starts

  if (length(load_start) > 0) {
    func_end <- load_start[1] - 1
  } else {
    # Find the line that starts the config section
    config_start <- grep("^# --- Config ---", lines)
    if (length(config_start) > 0) {
      func_end <- config_start[1] - 1
    } else {
      func_end <- length(lines)
    }
  }

  # Parse and evaluate just the validate_constitution function
  func_code <- lines[func_start[1]:func_end]
  eval(parse(text = func_code), envir = globalenv())
})

# --- Build a complete valid constitution (mirrors parse_constitution.R structure) ---

build_valid_constitution <- function() {
  list(
    scoring = list(
      batting = list(
        singles = 1, doubles = 2, triples = 3, hr = 4,
        grand_slam_bonus = 2, cycle = 5,
        runs = 1, rbi = 1, bb = 1, hbp = 1,
        sb = 2, cs = -1, k_batter = -0.5
      ),
      pitching = list(
        innings = 3, k_pitcher = 0.5,
        wins = 5, losses = -5, saves = 7, holds = 5,
        quality_starts = 3, complete_games = 2,
        no_hitter = 5, perfect_game = 5,
        hits_allowed = -1, earned_runs = -1,
        walks_issued = -1, intentional_walks = 1
      )
    ),
    salary = list(
      auction_cap = 260,
      in_season_cap = 300,
      keeper_cap = 80
    ),
    keepers = list(
      annual_increase = 4
    ),
    minor_league = list(
      promotion_threshold_ab = 130,
      promotion_threshold_ip = 50
    ),
    roster = list(
      positions = list(C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1, OF = 3, U = 1, SP = 5, RP = 2)
    ),
    playoffs = list(
      regular_season_weeks = 23,
      qualifiers = "4 division winners + 2 wildcards"
    ),
    transactions = list(
      faab_process = "Tuesday, Friday, Sunday at 11 PM ET"
    ),
    drafts = list(
      auction = list(type = "Live Salary Cap Draft", budget = 260),
      minor_league = list(rounds = 5, order = "Worst record first")
    )
  )
}

# --- All required field paths that validate_constitution checks ---
# Each entry: list of path components to reach the field, plus its label

REQUIRED_FIELDS <- list(
  list(path = c("scoring", "batting"), label = "scoring$batting"),
  list(path = c("scoring", "pitching"), label = "scoring$pitching"),
  list(path = c("salary", "auction_cap"), label = "salary$auction_cap"),
  list(path = c("salary", "in_season_cap"), label = "salary$in_season_cap"),
  list(path = c("salary", "keeper_cap"), label = "salary$keeper_cap"),
  list(path = c("keepers", "annual_increase"), label = "keepers$annual_increase"),
  list(path = c("minor_league", "promotion_threshold_ab"), label = "minor_league$promotion_threshold_ab"),
  list(path = c("minor_league", "promotion_threshold_ip"), label = "minor_league$promotion_threshold_ip"),
  list(path = c("roster", "positions"), label = "roster$positions"),
  list(path = c("playoffs"), label = "playoffs"),
  list(path = c("transactions", "faab_process"), label = "transactions$faab_process"),
  list(path = c("drafts", "auction"), label = "drafts$auction"),
  list(path = c("drafts", "minor_league"), label = "drafts$minor_league")
)

# --- Helper: remove a field from a nested list by path ---

remove_field <- function(constitution, path) {
  if (length(path) == 1) {
    constitution[[path[1]]] <- NULL
  } else if (length(path) == 2) {
    constitution[[path[1]]][[path[2]]] <- NULL
  } else {
    stop("Unsupported path depth > 2")
  }
  constitution
}

# --- Property Tests ---

test_that("Property 3: Valid constitution returns TRUE", {
  # **Validates: Requirements 2.10**
  constitution <- build_valid_constitution()
  result <- validate_constitution(constitution)
  expect_true(result)
})

test_that("Property 3: Real league_constitution.rds validates as TRUE", {
  # **Validates: Requirements 2.10**
  rds_path <- file.path(
    normalizePath(file.path("..", ".."), mustWork = FALSE),
    "data", "league_constitution.rds"
  )
  if (!file.exists(rds_path)) {
    rds_path <- file.path("data", "league_constitution.rds")
  }
  skip_if_not(file.exists(rds_path), "league_constitution.rds not found")

  constitution <- readRDS(rds_path)
  result <- validate_constitution(constitution)
  expect_true(result)
})

test_that("Property 3: Removing any single required field returns FALSE with correct label", {
  # **Validates: Requirements 2.10**
  # Systematic: test each of the 13 required fields individually
  for (field in REQUIRED_FIELDS) {
    constitution <- build_valid_constitution()
    broken <- remove_field(constitution, field$path)
    result <- validate_constitution(broken)

    expect_false(
      result,
      info = paste0("Removing '", field$label, "' should make constitution invalid")
    )

    missing_fields <- attr(result, "missing_fields")
    expect_true(
      field$label %in% missing_fields,
      info = paste0(
        "Missing fields should include '", field$label, "' but got: ",
        paste(missing_fields, collapse = ", ")
      )
    )
  }
})

test_that("Property 3 (Randomized): Removing random subsets of fields returns FALSE with all removed fields", {
  # **Validates: Requirements 2.10**
  # For 100+ random subsets (1 to N fields removed), verify FALSE + correct missing list
  set.seed(2024)
  n_iterations <- 120
  n_fields <- length(REQUIRED_FIELDS)

  for (i in seq_len(n_iterations)) {
    # Choose a random number of fields to remove (1 to n_fields)
    n_remove <- sample(1:n_fields, 1)
    # Choose which fields to remove
    remove_indices <- sample(seq_len(n_fields), n_remove)

    constitution <- build_valid_constitution()
    removed_labels <- character(0)

    for (idx in remove_indices) {
      field <- REQUIRED_FIELDS[[idx]]
      constitution <- remove_field(constitution, field$path)
      removed_labels <- c(removed_labels, field$label)
    }

    result <- validate_constitution(constitution)

    expect_false(
      result,
      info = paste0(
        "Iteration ", i, ": Removing ", n_remove, " field(s) [",
        paste(removed_labels, collapse = ", "), "] should return FALSE"
      )
    )

    missing_fields <- attr(result, "missing_fields")
    for (label in removed_labels) {
      expect_true(
        label %in% missing_fields,
        info = paste0(
          "Iteration ", i, ": Missing fields should include '", label,
          "' but got: ", paste(missing_fields, collapse = ", ")
        )
      )
    }
  }
})

test_that("Property 3: Valid constitution with extra fields still returns TRUE", {
  # **Validates: Requirements 2.10**
  # Adding extra fields should not break validation
  set.seed(99)
  n_iterations <- 50

  extra_field_names <- c(
    "custom_rules", "notes", "history", "amendments",
    "commissioner", "established", "motto", "website",
    "tiebreakers", "schedule", "waivers", "trade_review"
  )

  for (i in seq_len(n_iterations)) {
    constitution <- build_valid_constitution()
    # Add 1–5 random extra top-level fields
    n_extra <- sample(1:5, 1)
    extras <- sample(extra_field_names, n_extra)
    for (extra in extras) {
      constitution[[extra]] <- paste("extra_value", i)
    }

    result <- validate_constitution(constitution)
    expect_true(
      result,
      info = paste0(
        "Iteration ", i, ": Adding extra fields [",
        paste(extras, collapse = ", "), "] should not break validation"
      )
    )
  }
})

test_that("Property 3: Empty list returns FALSE with all required fields missing", {
  # **Validates: Requirements 2.10**
  result <- validate_constitution(list())

  expect_false(result)

  missing_fields <- attr(result, "missing_fields")
  expected_labels <- vapply(REQUIRED_FIELDS, function(f) f$label, character(1))

  expect_equal(
    sort(missing_fields),
    sort(expected_labels),
    info = "Empty list should report all 13 required fields as missing"
  )
})

test_that("Property 3: Setting a required field to NULL returns FALSE", {
  # **Validates: Requirements 2.10**
  # Even if the key exists but value is NULL, it should be detected as missing
  for (field in REQUIRED_FIELDS) {
    constitution <- build_valid_constitution()

    # Set the field to NULL (different from removing the key entirely)
    if (length(field$path) == 1) {
      constitution[[field$path[1]]] <- NULL
    } else if (length(field$path) == 2) {
      constitution[[field$path[1]]][[field$path[2]]] <- NULL
    }

    result <- validate_constitution(constitution)
    expect_false(
      result,
      info = paste0("Setting '", field$label, "' to NULL should return FALSE")
    )
  }
})

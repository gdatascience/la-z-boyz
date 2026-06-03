# Property Test: Player Fuzzy Matching Correctness (Property 4)
#
# **Validates: Requirements 3.5**
#
# Property: For any player present in both CBS and BRef with matching MLB team,
# fuzzy_match_players() shall produce confidence >= 0.8 despite name variations:
#   - Jr./III suffixes (e.g., "Ronald Acuna Jr." vs "Ronald Acuna")
#   - Accented characters (e.g., "José Ramírez" vs "Jose Ramirez")
#   - Abbreviated first names (e.g., "J.D. Martinez" vs "JD Martinez")

library(testthat)

# Source the module under test
source(file.path(here::here(), "R", "utils", "player_linker.R"))

# =============================================================================
# Name Variation Generators
# =============================================================================

#' Generate a random base player name
generate_base_name <- function() {
  first_names <- c(
    "Jose", "Juan", "Carlos", "Miguel", "Rafael", "Fernando", "Pedro",
    "Luis", "Ronald", "Vladimir", "Mookie", "Shohei", "Bryce", "Mike",
    "Aaron", "Corey", "Trea", "Marcus", "Wander", "Bobby", "Elly",
    "Corbin", "Gunnar", "Jackson", "Julio", "Adley", "Spencer", "James",
    "Brandon", "Freddie", "Kyle", "Austin", "Jordan", "Tyler", "Shane",
    "Nestor", "Gerrit", "Zack", "Max", "Sandy", "Dylan", "Logan"
  )

  last_names <- c(
    "Rodriguez", "Martinez", "Ramirez", "Alvarez", "Gonzalez", "Garcia",
    "Soto", "Acuna", "Guerrero", "Betts", "Ohtani", "Harper", "Trout",
    "Judge", "Seager", "Turner", "Semien", "Franco", "Witt", "De La Cruz",
    "Carroll", "Henderson", "Merrifield", "Rutschman", "Strider", "Outman",
    "Lowe", "Freeman", "Tucker", "Riley", "Walker", "McClanahan", "Cease",
    "Wheeler", "Scherzer", "Alcantara", "Cole", "Webb", "Bieber", "Gilbert"
  )

  first <- sample(first_names, 1)
  last <- sample(last_names, 1)

  list(
    first_name = first,
    last_name = last,
    full_name = paste(first, last)
  )
}

#' Apply a random suffix variation to a name
apply_suffix_variation <- function(full_name) {
  suffixes <- c(" Jr.", " Jr", " Sr.", " Sr", " II", " III", " IV")
  paste0(full_name, sample(suffixes, 1))
}

#' Apply accent marks to a name (simulate CBS having accented, BRef not)
apply_accent_variation <- function(full_name) {
  accent_map <- list(
    c("a", "\u00e1"),
    c("e", "\u00e9"),
    c("i", "\u00ed"),
    c("o", "\u00f3"),
    c("u", "\u00fa"),
    c("n", "\u00f1")
  )

  result <- full_name
  # Apply 1-2 random accent substitutions
  n_accents <- sample(1:2, 1)
  for (k in seq_len(n_accents)) {
    sub_pair <- sample(accent_map, 1)[[1]]
    result <- sub(sub_pair[1], sub_pair[2], result, fixed = TRUE)
  }
  result
}

#' Apply abbreviation variation — dots vs no-dots in already-abbreviated names
#' This models the real scenario: "J.D. Martinez" vs "JD Martinez"
apply_abbreviation_variation <- function(last_name) {
  # Generate a realistic double-initial first name
  letters_pool <- c("A", "B", "C", "D", "J", "K", "P", "R", "T")
  l1 <- sample(letters_pool, 1)
  l2 <- sample(letters_pool, 1)

  # Return both the dotted and undotted forms
  dotted <- paste0(l1, ".", l2, ". ", last_name)
  undotted <- paste0(l1, l2, " ", last_name)

  list(dotted = dotted, undotted = undotted)
}

#' Generate a random MLB team abbreviation
generate_team <- function() {
  teams <- c(
    "ARI", "ATL", "BAL", "BOS", "CHC", "CIN", "CLE", "COL",
    "CWS", "DET", "HOU", "KC", "LAA", "LAD", "MIA", "MIL",
    "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SD", "SEA",
    "SF", "STL", "TB", "TEX", "TOR", "WAS"
  )
  sample(teams, 1)
}

# =============================================================================
# Property Test: Fuzzy Matching with Name Variations (100+ iterations)
# =============================================================================

test_that("Property 4: Player fuzzy matching produces confidence >= 0.8 for same player with name variations", {
  set.seed(42)

  n_iterations <- 120  # More than the required 100

  failures <- list()

  for (iter in seq_len(n_iterations)) {
    base <- generate_base_name()
    team <- generate_team()

    # Decide which variation to apply (randomly)
    variation_type <- sample(c("suffix", "accent", "abbreviation"), 1)

    # Create the "canonical" name (source A) and "varied" name (source B)
    switch(variation_type,
      "suffix" = {
        # One source has suffix, the other does not
        if (runif(1) > 0.5) {
          name_a <- apply_suffix_variation(base$full_name)
          name_b <- base$full_name
        } else {
          name_a <- base$full_name
          name_b <- apply_suffix_variation(base$full_name)
        }
      },
      "accent" = {
        # One source has accents, the other has ASCII
        if (runif(1) > 0.5) {
          name_a <- apply_accent_variation(base$full_name)
          name_b <- base$full_name
        } else {
          name_a <- base$full_name
          name_b <- apply_accent_variation(base$full_name)
        }
      },
      "abbreviation" = {
        # Dot variation in abbreviated names: "J.D. Smith" vs "JD Smith"
        abbrev <- apply_abbreviation_variation(base$last_name)
        if (runif(1) > 0.5) {
          name_a <- abbrev$dotted
          name_b <- abbrev$undotted
        } else {
          name_a <- abbrev$undotted
          name_b <- abbrev$dotted
        }
      }
    )

    # Run fuzzy_match_players with matching teams
    result <- fuzzy_match_players(
      name_a = name_a,
      name_b = name_b,
      team_a = team,
      team_b = team
    )

    # The property: confidence must be >= 0.8
    if (is.null(result) || nrow(result) == 0 || result$confidence[1] < 0.8) {
      conf <- if (!is.null(result) && nrow(result) > 0) result$confidence[1] else NA
      failures[[length(failures) + 1]] <- list(
        iter = iter,
        variation = variation_type,
        name_a = name_a,
        name_b = name_b,
        team = team,
        confidence = conf
      )
    }
  }

  # Report all failures at once for clear diagnostics
  if (length(failures) > 0) {
    failure_msgs <- vapply(failures, function(f) {
      sprintf(
        "  Iter %d [%s]: '%s' vs '%s' (team=%s) -> confidence=%.4f",
        f$iter, f$variation, f$name_a, f$name_b, f$team,
        if (is.na(f$confidence)) -1 else f$confidence
      )
    }, character(1))

    fail(sprintf(
      "Property 4 violated in %d/%d iterations:\n%s",
      length(failures), n_iterations, paste(failure_msgs, collapse = "\n")
    ))
  }

  expect_true(TRUE)
})

# =============================================================================
# Targeted Property Tests for Each Variation Type
# =============================================================================

test_that("Property 4 (suffix): Suffix variations produce confidence >= 0.8", {
  set.seed(123)

  # Known real-world examples with suffixes
  suffix_cases <- list(
    list(a = "Ronald Acuna Jr.", b = "Ronald Acuna", team = "ATL"),
    list(a = "Vladimir Guerrero Jr.", b = "Vladimir Guerrero", team = "TOR"),
    list(a = "Fernando Tatis Jr.", b = "Fernando Tatis", team = "SD"),
    list(a = "Wander Franco", b = "Wander Franco Jr.", team = "TB"),
    list(a = "Ke'Bryan Hayes III", b = "Ke'Bryan Hayes", team = "PIT"),
    list(a = "Bo Bichette", b = "Bo Bichette II", team = "TOR")
  )

  # Add randomly generated suffix cases
  for (i in 1:20) {
    base <- generate_base_name()
    team <- generate_team()
    suffix_cases[[length(suffix_cases) + 1]] <- list(
      a = apply_suffix_variation(base$full_name),
      b = base$full_name,
      team = team
    )
  }

  for (case in suffix_cases) {
    result <- fuzzy_match_players(
      name_a = case$a,
      name_b = case$b,
      team_a = case$team,
      team_b = case$team
    )
    expect_true(
      result$confidence[1] >= 0.8,
      label = sprintf("Suffix: '%s' vs '%s' (team=%s) confidence=%.4f",
                      case$a, case$b, case$team, result$confidence[1])
    )
  }
})

test_that("Property 4 (accents): Accented name variations produce confidence >= 0.8", {
  set.seed(456)

  # Known real-world examples with accents
  accent_cases <- list(
    list(a = "Jos\u00e9 Ram\u00edrez", b = "Jose Ramirez", team = "CLE"),
    list(a = "Julio Rodr\u00edguez", b = "Julio Rodriguez", team = "SEA"),
    list(a = "Andr\u00e9s Gimenez", b = "Andres Gimenez", team = "CLE"),
    list(a = "Yandy D\u00edaz", b = "Yandy Diaz", team = "TB"),
    list(a = "Jes\u00fas Luzardo", b = "Jesus Luzardo", team = "MIA"),
    list(a = "N\u00e9stor Cortes", b = "Nestor Cortes", team = "NYY"),
    list(a = "Luis Casti\u00f1o", b = "Luis Castino", team = "SEA"),
    list(a = "Adolis Garc\u00eda", b = "Adolis Garcia", team = "TEX")
  )

  # Add randomly generated accent cases
  for (i in 1:20) {
    base <- generate_base_name()
    team <- generate_team()
    accent_cases[[length(accent_cases) + 1]] <- list(
      a = apply_accent_variation(base$full_name),
      b = base$full_name,
      team = team
    )
  }

  for (case in accent_cases) {
    result <- fuzzy_match_players(
      name_a = case$a,
      name_b = case$b,
      team_a = case$team,
      team_b = case$team
    )
    expect_true(
      result$confidence[1] >= 0.8,
      label = sprintf("Accent: '%s' vs '%s' (team=%s) confidence=%.4f",
                      case$a, case$b, case$team, result$confidence[1])
    )
  }
})

test_that("Property 4 (abbreviation): Dot vs no-dot abbreviated names produce confidence >= 0.8", {
  set.seed(789)

  # Known real-world examples: dots vs no dots

  abbrev_cases <- list(
    list(a = "J.D. Martinez", b = "JD Martinez", team = "LAD"),
    list(a = "A.J. Minter", b = "AJ Minter", team = "ATL"),
    list(a = "T.J. Friedl", b = "TJ Friedl", team = "CIN"),
    list(a = "J.P. Crawford", b = "JP Crawford", team = "SEA"),
    list(a = "C.J. Abrams", b = "CJ Abrams", team = "WAS"),
    list(a = "J.T. Realmuto", b = "JT Realmuto", team = "PHI"),
    list(a = "C.J. Cron", b = "CJ Cron", team = "COL"),
    list(a = "J.P. Feyereisen", b = "JP Feyereisen", team = "TB")
  )

  # Add randomly generated dot-variation cases
  for (i in 1:20) {
    base <- generate_base_name()
    team <- generate_team()
    abbrev <- apply_abbreviation_variation(base$last_name)
    abbrev_cases[[length(abbrev_cases) + 1]] <- list(
      a = abbrev$dotted,
      b = abbrev$undotted,
      team = team
    )
  }

  for (case in abbrev_cases) {
    result <- fuzzy_match_players(
      name_a = case$a,
      name_b = case$b,
      team_a = case$team,
      team_b = case$team
    )
    expect_true(
      result$confidence[1] >= 0.8,
      label = sprintf("Abbreviation: '%s' vs '%s' (team=%s) confidence=%.4f",
                      case$a, case$b, case$team, result$confidence[1])
    )
  }
})

# =============================================================================
# Negative property: Different players should NOT get high confidence
# =============================================================================

test_that("Property 4 (negative): Different players on same team do not get confidence >= 0.8", {
  set.seed(999)

  # Pairs of clearly different players on the same team
  different_pairs <- list(
    list(a = "Mike Trout", b = "Shohei Ohtani", team = "LAA"),
    list(a = "Bryce Harper", b = "Kyle Schwarber", team = "PHI"),
    list(a = "Aaron Judge", b = "Gerrit Cole", team = "NYY"),
    list(a = "Mookie Betts", b = "Freddie Freeman", team = "LAD"),
    list(a = "Jose Ramirez", b = "Steven Kwan", team = "CLE")
  )

  for (case in different_pairs) {
    result <- fuzzy_match_players(
      name_a = case$a,
      name_b = case$b,
      team_a = case$team,
      team_b = case$team
    )
    # Different players should have confidence < 0.8
    expect_true(
      result$confidence[1] < 0.8,
      label = sprintf("Different players: '%s' vs '%s' should NOT match (confidence=%.4f)",
                      case$a, case$b, result$confidence[1])
    )
  }
})

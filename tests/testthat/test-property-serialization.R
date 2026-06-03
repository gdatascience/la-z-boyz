# Property-Based Test: Serialization Round-Trip (Properties 1, 2)
#
# **Validates: Requirements 1.5, 9.1, 9.3**
#
# Property 1: RDS Serialization Round-Trip
# For any R object (data frame, list, or nested structure) with metadata
# attributes, saving via saveRDS/readRDS produces an identical object
# including all column types, row values, and custom attributes
# (source_file, parsed_at, league_id).
#
# Property 2: CSV Export Round-Trip
# For any data frame containing text columns with UTF-8 characters
# (including accented names like "José Ramírez"), exporting to CSV with
# write.csv(fileEncoding = "UTF-8") and reading back with
# read.csv(fileEncoding = "UTF-8") preserves all character values exactly.
#
# Uses replicated random data generation (120 iterations) to verify
# properties across many inputs with varying column types, UTF-8 names,
# and custom attributes.

library(testthat)

# --- Configuration ---
PBT_ITERATIONS <- 120

# --- UTF-8 character pools for name generation ---
utf8_first_names <- c(

"José", "Ramón", "André", "François", "Müller", "Ñoño",
  "Björk", "Łukasz", "Ångström", "Übermensch", "Renée", "Zoë",
  "Héctor", "Raúl", "Iñaki", "Jürgen", "Ólafur", "Stéphane",
  "Tomáš", "Dražen", "Gábor", "Nuño", "Seán", "Benoît",
  "Grégoire", "Clémentine", "Adrián", "Álvaro", "César", "Darío"
)

utf8_last_names <- c(
  "Ramírez", "González", "López", "Hernández", "Pérez", "García",
  "Martínez", "Rodríguez", "Sánchez", "Díaz", "Müller", "Böhm",
  "Schröder", "Håkansson", "Jørgensen", "Çelik", "Öztürk", "Ñúñez",
  "Kvíčala", "Dvořák", "Šťastný", "Błaszczykowski", "Łoziński",
  "Ólafsson", "Guðmundsson", "Þórhallsson", "Brøndum", "Ødegård",
  "Väänänen", "Räikkönen"
)

# --- Generators: random data frames with various column types ---

generate_random_df <- function(seed_offset = 0) {
  n_rows <- sample(5:50, 1)
  n_char_cols <- sample(1:3, 1)
  n_num_cols <- sample(1:4, 1)
  n_int_cols <- sample(1:2, 1)
  n_logical_cols <- sample(0:1, 1)

  cols <- list()

  # Character columns with UTF-8 names
  for (i in seq_len(n_char_cols)) {
    col_name <- paste0("name_", i)
    cols[[col_name]] <- paste(
      sample(utf8_first_names, n_rows, replace = TRUE),
      sample(utf8_last_names, n_rows, replace = TRUE)
    )
  }

  # Numeric columns (doubles)
  for (i in seq_len(n_num_cols)) {
    col_name <- paste0("value_", i)
    cols[[col_name]] <- runif(n_rows, min = -1000, max = 1000)
  }

  # Integer columns
  for (i in seq_len(n_int_cols)) {
    col_name <- paste0("count_", i)
    cols[[col_name]] <- sample(-100L:500L, n_rows, replace = TRUE)
  }

  # Logical columns
  if (n_logical_cols > 0) {
    for (i in seq_len(n_logical_cols)) {
      col_name <- paste0("flag_", i)
      cols[[col_name]] <- sample(c(TRUE, FALSE), n_rows, replace = TRUE)
    }
  }

  as.data.frame(cols, stringsAsFactors = FALSE)
}

generate_random_list <- function() {
  list(
    name = paste(sample(utf8_first_names, 1), sample(utf8_last_names, 1)),
    values = runif(sample(3:10, 1)),
    counts = sample(1:100, sample(2:5, 1)),
    flag = sample(c(TRUE, FALSE), 1),
    nested = list(
      sub_name = sample(utf8_first_names, 1),
      sub_value = rnorm(1)
    )
  )
}

generate_utf8_df <- function() {
  n_rows <- sample(10:60, 1)
  data.frame(
    player_name = paste(
      sample(utf8_first_names, n_rows, replace = TRUE),
      sample(utf8_last_names, n_rows, replace = TRUE)
    ),
    team = sample(c("NYY", "BOS", "LAD", "CHC", "ATL", "HOU", "SD", "PHI"),
                  n_rows, replace = TRUE),
    salary = round(runif(n_rows, 1, 50), 1),
    position = sample(c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP"),
                      n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# --- Property Tests ---

test_that("Property 1 (RDS Round-Trip): saveRDS/readRDS produces identical data frame with attributes", {
  # **Validates: Requirements 1.5, 9.1**
  set.seed(2024)
  tmp_dir <- tempdir()

  for (i in seq_len(PBT_ITERATIONS)) {
    df <- generate_random_df(seed_offset = i)

    # Add metadata attributes (mimicking save_rds_with_metadata)
    attr(df, "source_file") <- paste0("test_source_", i, ".html")
    attr(df, "parsed_at") <- Sys.time() + runif(1, -86400, 86400)
    attr(df, "league_id") <- sample(c("l-z-bs", "other-league", "test-league"), 1)

    # Round-trip via temp file
    tmp_file <- file.path(tmp_dir, paste0("pbt_rds_", i, ".rds"))
    on.exit(unlink(tmp_file), add = TRUE)

    saveRDS(df, tmp_file)
    loaded <- readRDS(tmp_file)

    # Verify data identity
    expect_identical(
      loaded, df,
      info = paste0(
        "Iteration ", i, ": RDS round-trip failed. ",
        "Rows=", nrow(df), ", Cols=", ncol(df)
      )
    )

    # Verify attributes explicitly
    expect_identical(
      attr(loaded, "source_file"), attr(df, "source_file"),
      info = paste0("Iteration ", i, ": source_file attribute mismatch")
    )
    expect_identical(
      attr(loaded, "parsed_at"), attr(df, "parsed_at"),
      info = paste0("Iteration ", i, ": parsed_at attribute mismatch")
    )
    expect_identical(
      attr(loaded, "league_id"), attr(df, "league_id"),
      info = paste0("Iteration ", i, ": league_id attribute mismatch")
    )

    unlink(tmp_file)
  }
})

test_that("Property 1 (RDS Round-Trip): saveRDS/readRDS preserves nested list structures", {
  # **Validates: Requirements 1.5, 9.1**
  set.seed(7777)
  tmp_dir <- tempdir()

  for (i in seq_len(PBT_ITERATIONS)) {
    obj <- generate_random_list()

    # Add metadata attributes
    attr(obj, "source_file") <- paste0("api_response_", i)
    attr(obj, "parsed_at") <- Sys.time()
    attr(obj, "league_id") <- "l-z-bs"

    tmp_file <- file.path(tmp_dir, paste0("pbt_rds_list_", i, ".rds"))
    on.exit(unlink(tmp_file), add = TRUE)

    saveRDS(obj, tmp_file)
    loaded <- readRDS(tmp_file)

    expect_identical(
      loaded, obj,
      info = paste0(
        "Iteration ", i, ": RDS list round-trip failed. ",
        "Elements=", length(obj)
      )
    )

    # Verify attributes preserved
    expect_identical(
      attr(loaded, "source_file"), attr(obj, "source_file"),
      info = paste0("Iteration ", i, ": list source_file attribute mismatch")
    )
    expect_identical(
      attr(loaded, "league_id"), attr(obj, "league_id"),
      info = paste0("Iteration ", i, ": list league_id attribute mismatch")
    )

    unlink(tmp_file)
  }
})

test_that("Property 1 (RDS Round-Trip): save_rds_with_metadata preserves object and adds correct attributes", {
  # **Validates: Requirements 1.5, 9.1**
  set.seed(3030)
  tmp_dir <- tempdir()

  for (i in seq_len(PBT_ITERATIONS)) {
    df <- generate_random_df(seed_offset = i * 100)
    source_val <- paste0("https://cbs.example.com/data_", i)
    league_val <- sample(c("l-z-bs", "test-league"), 1)

    tmp_file <- file.path(tmp_dir, paste0("pbt_meta_", i, ".rds"))
    on.exit(unlink(tmp_file), add = TRUE)

    # Use the actual serialization function
    result <- save_rds_with_metadata(df, tmp_file, source = source_val, league_id = league_val)

    # Verify returned object has attributes set
    expect_equal(attr(result, "source_file"), source_val,
      info = paste0("Iteration ", i, ": returned source_file mismatch"))
    expect_equal(attr(result, "league_id"), league_val,
      info = paste0("Iteration ", i, ": returned league_id mismatch"))
    expect_true(inherits(attr(result, "parsed_at"), "POSIXct"),
      info = paste0("Iteration ", i, ": parsed_at should be POSIXct"))

    # Round-trip: load and verify
    loaded <- readRDS(tmp_file)
    expect_equal(attr(loaded, "source_file"), source_val,
      info = paste0("Iteration ", i, ": loaded source_file mismatch"))
    expect_equal(attr(loaded, "league_id"), league_val,
      info = paste0("Iteration ", i, ": loaded league_id mismatch"))
    expect_true(inherits(attr(loaded, "parsed_at"), "POSIXct"),
      info = paste0("Iteration ", i, ": loaded parsed_at should be POSIXct"))

    # Data content preserved (ignoring metadata attributes)
    expect_equal(nrow(loaded), nrow(df),
      info = paste0("Iteration ", i, ": row count mismatch"))
    expect_equal(ncol(loaded), ncol(df),
      info = paste0("Iteration ", i, ": col count mismatch"))

    # Check all column values match
    for (col in names(df)) {
      expect_identical(loaded[[col]], df[[col]],
        info = paste0("Iteration ", i, ": column '", col, "' content mismatch"))
    }

    unlink(tmp_file)
  }
})

test_that("Property 2 (CSV Round-Trip): UTF-8 characters preserved through write/read", {
  # **Validates: Requirements 9.3**
  set.seed(5050)
  tmp_dir <- tempdir()

  for (i in seq_len(PBT_ITERATIONS)) {
    df <- generate_utf8_df()

    tmp_file <- file.path(tmp_dir, paste0("pbt_csv_", i, ".csv"))
    on.exit(unlink(tmp_file), add = TRUE)

    # Write with UTF-8 encoding (matching export_csv implementation)
    con <- file(tmp_file, open = "w", encoding = "UTF-8")
    write.csv(df, con, row.names = FALSE, fileEncoding = "UTF-8")
    close(con)

    # Read back with UTF-8 encoding
    loaded <- read.csv(tmp_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                       encoding = "UTF-8")

    # Verify all character columns preserved exactly
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        expect_identical(
          loaded[[col]], df[[col]],
          info = paste0(
            "Iteration ", i, ": UTF-8 mismatch in column '", col, "'. ",
            "Sample values: ", paste(head(df[[col]], 3), collapse = ", ")
          )
        )
      }
    }

    unlink(tmp_file)
  }
})

test_that("Property 2 (CSV Round-Trip): export_csv preserves UTF-8 accented names", {
  # **Validates: Requirements 9.3**
  set.seed(8080)
  tmp_dir <- tempdir()

  for (i in seq_len(PBT_ITERATIONS)) {
    df <- generate_utf8_df()

    tmp_file <- file.path(tmp_dir, paste0("pbt_export_csv_", i, ".csv"))
    on.exit(unlink(tmp_file), add = TRUE)

    # Use the actual export_csv function
    export_csv(df, tmp_file, encoding = "UTF-8")

    # Read back
    loaded <- read.csv(tmp_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                       encoding = "UTF-8")

    # Verify player_name column (contains accented characters)
    expect_identical(
      loaded$player_name, df$player_name,
      info = paste0(
        "Iteration ", i, ": export_csv UTF-8 round-trip failed for player_name. ",
        "Rows=", nrow(df), ", Sample: ", head(df$player_name, 2)[1]
      )
    )

    # Verify all other character columns
    expect_identical(
      loaded$team, df$team,
      info = paste0("Iteration ", i, ": team column mismatch")
    )
    expect_identical(
      loaded$position, df$position,
      info = paste0("Iteration ", i, ": position column mismatch")
    )

    unlink(tmp_file)
  }
})

test_that("Property 2 (CSV Edge): specific accented names survive round-trip", {
  # **Validates: Requirements 9.3**
  # Targeted test with known tricky UTF-8 characters
  tmp_dir <- tempdir()
  tmp_file <- file.path(tmp_dir, "pbt_csv_edge.csv")
  on.exit(unlink(tmp_file), add = TRUE)

  tricky_names <- c(
    "José Ramírez", "Ñoño Pérez", "André François",
    "Müller Schröder", "Björk Ólafsson", "Łukasz Błaszczykowski",
    "Tomáš Dvořák", "Dražen Šťastný", "Ångström Väänänen",
    "Übermensch Räikkönen", "Seán Ødegård", "Gábor Þórhallsson",
    "Clémentine Brøndum", "Héctor Guðmundsson", "Iñaki Çelik"
  )

  df <- data.frame(
    player_name = tricky_names,
    value = seq_along(tricky_names),
    stringsAsFactors = FALSE
  )

  export_csv(df, tmp_file, encoding = "UTF-8")
  loaded <- read.csv(tmp_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                     encoding = "UTF-8")

  for (j in seq_along(tricky_names)) {
    expect_identical(
      loaded$player_name[j], tricky_names[j],
      info = paste0("Tricky name failed: '", tricky_names[j], "'")
    )
  }
})

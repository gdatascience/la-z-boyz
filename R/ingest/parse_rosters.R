#!/usr/bin/env Rscript
#' Parse Rosters & Salaries from CBS CSV Export
#'
#' Ingests the CSV roster overview exported from CBS
#' (Roster Overview > All Teams > Period dropdown > CSV export button).
#'
#' Usage: Rscript R/ingest/parse_rosters.R [path/to/file.csv]
#'
#' If no path is provided, looks for the most recent CSV matching the pattern
#' `roster-overview-all-*.csv` in data/imports/.
#'
#' Output: data/cache/rosters.rds
#'
#' CSV Format (from CBS):
#'   - Teams are separated by header rows like "Team Name Batters" / "Team Name Pitchers"
#'   - Column headers appear after each team header: Pos, Players, ...salary, ...Total
#'   - Player rows have: Pos, "Name POSITIONS | TEAM", ..., salary, ..., Total FPTS
#'   - Schedule column contains embedded newlines (Home/Away on separate lines)
#'   - Section markers: "Reserves", "Minors"
#'   - Summary rows: "Active: N Reserve: N Minors: N Active salary: N Total salary: N"

library(here)

# --- Config ---
output_file <- here("data/cache/rosters.rds")
EXPECTED_TEAMS <- 16
MIN_PLAYERS <- 350
MAX_PLAYERS <- 450
LEAGUE_ID <- "l-z-bs"

# --- Logging utilities ---
log_error <- function(msg) message(sprintf("[%s] ERROR: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
log_warn  <- function(msg) message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
log_info  <- function(msg) message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))

# --- Find CSV file ---
find_csv_file <- function(path_arg = NULL) {
  if (!is.null(path_arg) && file.exists(path_arg)) {
    return(path_arg)
  }
  
  import_dir <- here("data/imports")
  csvs <- list.files(import_dir, pattern = "^roster-overview-all.*\\.csv$", full.names = TRUE)
  
  if (length(csvs) == 0) {
    stop("No roster CSV files found in data/imports/. Export from CBS: Roster Overview > All teams > CSV export.")
  }
  
  # Return the most recent by filename (contains date)
  csvs <- sort(csvs, decreasing = TRUE)
  csvs[1]
}

# --- Parse player string: "Name POSITIONS | TEAM" ---
#' Parse the Players column into player name, eligible positions, and MLB team
#' @param player_str Character like 'Bobby Witt SS | KC' or 'Jose Caballero 2B,3B,SS,OF | NYY'
#' @return Named list with player_name, eligible_positions, mlb_team
parse_player_string <- function(player_str) {
  if (is.na(player_str) || nchar(trimws(player_str)) == 0) {
    return(list(player_name = NA_character_, eligible_positions = NA_character_, mlb_team = NA_character_))
  }
  
  # Pattern: "Player Name POS1,POS2 | TEAM"
  # The pipe separates position info from team abbreviation
  parts <- strsplit(player_str, "\\s*\\|\\s*")[[1]]
  mlb_team <- if (length(parts) >= 2) trimws(parts[2]) else NA_character_
  
  name_pos <- trimws(parts[1])
  
  # Position abbreviations that can appear
  pos_pattern <- "\\s+(C|1B|2B|3B|SS|OF|U|SP|RP|DH)(,(C|1B|2B|3B|SS|OF|U|SP|RP|DH))*\\s*$"
  
  if (grepl(pos_pattern, name_pos)) {
    pos_match <- regmatches(name_pos, regexpr(pos_pattern, name_pos))
    eligible_positions <- trimws(pos_match)
    player_name <- trimws(sub(pos_pattern, "", name_pos))
  } else {
    eligible_positions <- NA_character_
    player_name <- name_pos
  }
  
  list(player_name = player_name, eligible_positions = eligible_positions, mlb_team = mlb_team)
}

# --- Main parser ---
parse_roster_csv <- function(csv_path) {
  log_info(paste("Parsing rosters from CSV:", csv_path))
  
  raw_lines <- readLines(csv_path, warn = FALSE)
  
  # CBS CSV has multiline schedule fields — "Away:" continuation lines
  # must be joined back to the previous line to form complete CSV rows.
  lines <- character(0)
  i <- 1
  while (i <= length(raw_lines)) {
    if (i < length(raw_lines) && grepl("^Away:", raw_lines[i + 1])) {
      lines <- c(lines, paste0(raw_lines[i], "\n", raw_lines[i + 1]))
      i <- i + 2
    } else {
      lines <- c(lines, raw_lines[i])
      i <- i + 1
    }
  }
  
  all_rosters <- list()
  current_team <- NA_character_
  current_type <- NA_character_  # "Batter" or "Pitcher"
  current_section <- "Active"    # "Active", "Reserves", or "Minors"
  skip_next <- 0                 # Number of header lines to skip
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    trimmed <- trimws(line)
    
    # Skip empty lines
    if (nchar(trimmed) == 0) next
    
    # Skip header lines after team header (2 lines: sub-header + column names)
    if (skip_next > 0) {
      skip_next <- skip_next - 1
      next
    }
    
    # Detect team header: "Team Name Batters" or "Team Name Pitchers"
    if (grepl("\\s+Batters$", trimmed) && !grepl("^Pos,", trimmed) && !grepl(",", trimmed)) {
      current_team <- sub("\\s+Batters$", "", trimmed)
      current_type <- "Batter"
      current_section <- "Active"
      skip_next <- 2  # Skip the two header rows
      next
    }
    if (grepl("\\s+Pitchers$", trimmed) && !grepl("^Pos,", trimmed) && !grepl(",", trimmed)) {
      current_team <- sub("\\s+Pitchers$", "", trimmed)
      current_type <- "Pitcher"
      current_section <- "Active"
      skip_next <- 2  # Skip the two header rows
      next
    }
    
    # Detect section changes
    if (trimmed == "Reserves") {
      current_section <- "Reserves"
      next
    }
    if (trimmed == "Minors") {
      current_section <- "Minors"
      next
    }
    
    # Skip summary rows
    if (grepl("^Active:\\s*\\d+", trimmed)) next
    
    # Skip if no team context yet
    if (is.na(current_team)) next
    
    # Parse player row — CSV with embedded commas and newlines in quoted fields
    parsed <- tryCatch({
      con <- textConnection(line)
      result <- read.csv(con, header = FALSE, stringsAsFactors = FALSE, quote = '"')
      close(con)
      result
    }, error = function(e) NULL)
    
    if (is.null(parsed) || ncol(parsed) < 9) next
    
    # Column layout (after joining multiline schedule field):
    # V1=Pos, V2=Players, V3=First Game, V4=Schedule (Home/Away),
    # V5=Proj Rank, V6=Actual Rank, V7=Rost%, V8=Start%,
    # V9=salary, V10=Period pts, V11=Week pts, V12=Total FPTS
    
    pos <- trimws(as.character(parsed$V1[1]))
    player_str <- trimws(as.character(parsed$V2[1]))
    
    # Skip if pos doesn't look like a valid fantasy position
    valid_positions <- c("C", "1B", "2B", "3B", "SS", "OF", "U", "SP", "RP", "DH")
    if (!pos %in% valid_positions) next
    
    # Parse player info
    player_info <- parse_player_string(player_str)
    if (is.na(player_info$player_name) || nchar(player_info$player_name) == 0) next
    
    # Salary (V9)
    salary_raw <- trimws(as.character(parsed$V9[1]))
    
    # Total FPTS (V12)
    total_fpts_raw <- if (ncol(parsed) >= 12) trimws(as.character(parsed$V12[1])) else NA
    total_fpts <- suppressWarnings(as.numeric(total_fpts_raw))
    
    # Parse salary: detect minor contract (*) and extract number
    is_minor <- grepl("\\*", salary_raw)
    salary_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", salary_raw)))
    
    all_rosters[[length(all_rosters) + 1]] <- data.frame(
      team_name = current_team,
      player_type = current_type,
      roster_position = pos,
      player_name = player_info$player_name,
      eligible_positions = player_info$eligible_positions,
      mlb_team = player_info$mlb_team,
      cbs_id = NA_character_,
      salary_raw = salary_raw,
      salary = salary_num,
      is_minor_contract = is_minor,
      total_fpts = total_fpts,
      roster_section = current_section,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(all_rosters) == 0) {
    stop("No player rows parsed from CSV. Check file format.")
  }
  
  roster_df <- do.call(rbind, all_rosters)
  roster_df
}

# --- Main execution ---
args <- commandArgs(trailingOnly = TRUE)
csv_path <- find_csv_file(if (length(args) > 0) args[1] else NULL)

roster_df <- parse_roster_csv(csv_path)

# --- Validation ---
n_teams <- length(unique(roster_df$team_name))
n_players <- nrow(roster_df)

if (n_teams != EXPECTED_TEAMS) {
  log_warn(sprintf("Expected %d teams but found %d: %s",
    EXPECTED_TEAMS, n_teams, paste(unique(roster_df$team_name), collapse = ", ")))
}
if (n_players < MIN_PLAYERS || n_players > MAX_PLAYERS) {
  log_warn(sprintf("Player count (%d) outside expected range (%d-%d)", n_players, MIN_PLAYERS, MAX_PLAYERS))
}

# --- Add metadata ---
attr(roster_df, "source_file") <- normalizePath(csv_path, mustWork = FALSE)
attr(roster_df, "parsed_at") <- Sys.time()
attr(roster_df, "league_id") <- LEAGUE_ID
attr(roster_df, "team_count") <- n_teams
attr(roster_df, "player_count") <- n_players

# --- Save ---
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
saveRDS(roster_df, output_file)

# --- Report ---
log_info("Rosters parsed successfully from CSV!")
log_info(sprintf("  Source: %s", basename(csv_path)))
log_info(sprintf("  Teams: %d", n_teams))
log_info(sprintf("  Players: %d", n_players))
log_info(sprintf("  Batters: %d", sum(roster_df$player_type == "Batter")))
log_info(sprintf("  Pitchers: %d", sum(roster_df$player_type == "Pitcher")))
log_info(sprintf("  Minor contracts: %d", sum(roster_df$is_minor_contract, na.rm = TRUE)))
log_info(sprintf("  Salary range: $%s - $%s",
  min(roster_df$salary, na.rm = TRUE),
  max(roster_df$salary, na.rm = TRUE)))
log_info(sprintf("  Sections: Active=%d, Reserves=%d, Minors=%d",
  sum(roster_df$roster_section == "Active"),
  sum(roster_df$roster_section == "Reserves"),
  sum(roster_df$roster_section == "Minors")))
log_info(sprintf("  Saved to: %s", output_file))

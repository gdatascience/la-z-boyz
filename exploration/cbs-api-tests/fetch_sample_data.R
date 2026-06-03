#!/usr/bin/env Rscript
# Fetch sample data from CBS API - players and league rules
# No authentication required for these endpoints

library(httr2)
library(jsonlite)
library(xml2)

cat("=== CBS Fantasy API - Sample Data ===\n\n")

league_id <- "l-z-bs"
base <- paste0("https://", league_id, ".baseball.cbssports.com")

# --- Fetch Players ---
cat("Fetching player list (this is ~3MB, may take a moment)...\n")
resp <- request(paste0(base, "/api/players/list")) |>
  req_url_query(response_format = "JSON") |>
  req_timeout(60) |>
  req_perform()

players_json <- resp_body_json(resp)
players <- players_json$body$players

cat("Total players in database:", length(players), "\n\n")

# Convert to data frame for easier searching
players_df <- do.call(rbind, lapply(players, function(p) {
  data.frame(
    id = p$id %||% NA,
    fullname = p$fullname %||% NA,
    firstname = p$firstname %||% NA,
    lastname = p$lastname %||% NA,
    position = p$position %||% NA,
    eligible_positions = p$eligible_positions_display %||% NA,
    pro_team = p$pro_team %||% NA,
    pro_status = p$pro_status %||% NA,
    age = p$age %||% NA,
    bats = p$bats %||% NA,
    throws = p$throws %||% NA,
    jersey = p$jersey %||% NA,
    injury = if (!is.null(p$icons$injury)) p$icons$injury else NA,
    stringsAsFactors = FALSE
  )
}))

# --- Show specific players ---
target_players <- c("Bobby Witt", "Jose Ramirez", "Max Fried")

cat("=== PLAYER DATA ===\n\n")
for (name in target_players) {
  matches <- players_df[grepl(name, players_df$fullname, ignore.case = TRUE), ]
  if (nrow(matches) > 0) {
    for (i in 1:nrow(matches)) {
      cat(sprintf("--- %s ---\n", matches$fullname[i]))
      cat(sprintf("  CBS ID:              %s\n", matches$id[i]))
      cat(sprintf("  Position:            %s\n", matches$position[i]))
      cat(sprintf("  Eligible Positions:  %s\n", matches$eligible_positions[i]))
      cat(sprintf("  MLB Team:            %s\n", matches$pro_team[i]))
      cat(sprintf("  Pro Status:          %s\n", matches$pro_status[i]))
      cat(sprintf("  Age:                 %s\n", matches$age[i]))
      cat(sprintf("  Bats/Throws:         %s/%s\n", matches$bats[i], matches$throws[i]))
      cat(sprintf("  Jersey:              %s\n", matches$jersey[i]))
      if (!is.na(matches$injury[i])) {
        cat(sprintf("  Injury:              %s\n", matches$injury[i]))
      }
      cat("\n")
    }
  } else {
    cat(sprintf("--- %s: NOT FOUND ---\n\n", name))
  }
}

# --- Show raw JSON for one player ---
cat("=== RAW JSON (Bobby Witt Jr.) ===\n")
witt <- players[sapply(players, function(p) grepl("Bobby Witt", p$fullname %||% ""))]
if (length(witt) > 0) {
  cat(toJSON(witt[[1]], pretty = TRUE, auto_unbox = TRUE), "\n\n")
}

# --- Fetch League Rules ---
cat("\n=== LEAGUE RULES (Full XML) ===\n\n")
resp_rules <- request(paste0(base, "/api/league/rules")) |>
  req_timeout(30) |>
  req_perform()

rules_body <- resp_body_string(resp_rules)
doc <- read_xml(rules_body)

# Pretty print
cat(as.character(doc), "\n")

# --- Summary stats ---
cat("\n\n=== PLAYER DATABASE SUMMARY ===\n")
cat("Total players:", nrow(players_df), "\n")
cat("By position:\n")
print(sort(table(players_df$position), decreasing = TRUE))
cat("\nBy pro_status:\n")
cat("  A (Active 26-man):", sum(players_df$pro_status == "A", na.rm = TRUE), "\n")
cat("  M (Minors):       ", sum(players_df$pro_status == "M", na.rm = TRUE), "\n")
cat("  IL (Injured List):", sum(players_df$pro_status == "IL", na.rm = TRUE), "\n")
cat("  Other/NA:         ", sum(is.na(players_df$pro_status) | !players_df$pro_status %in% c("A","M","IL")), "\n")

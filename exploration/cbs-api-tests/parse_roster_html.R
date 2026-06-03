#!/usr/bin/env Rscript
# Parse the saved CBS league rosters/salaries HTML page
# This demonstrates how to extract data from a browser-saved "Save Webpage Complete" export

library(rvest)
library(dplyr)
library(tidyr)

cat("=== Parsing Rosters & Salaries from Saved HTML ===\n\n")

html_file <- "exploration/sample-data/rosters_and_salaries.html"
page <- read_html(html_file)

# Each team has a table with class "data"
# Team name is in the <tr class="title"> row
# Column headers: Pos, Players, First Game, Schedule, Proj, Actual, Rost, Start, salary, Prd, Weekly, Total

# Find all team tables
tables <- page |> html_elements("table.data")
cat("Found", length(tables), "team tables\n\n")

all_rosters <- list()

for (tbl in tables) {
  # Get team name from the title row
  title_row <- tbl |> html_element("tr.title td")
  if (is.na(title_row)) next
  
  team_raw <- title_row |> html_text(trim = TRUE)
  
  # Team names end with "Batters" or "Pitchers" - extract both
  # The actual team name is everything before "Batters" or "Pitchers"
  player_type <- ifelse(grepl("Pitchers$", team_raw), "Pitcher", "Batter")
  team_name <- gsub("\\s*(Batters|Pitchers)$", "", team_raw)
  
  # Get player rows
  player_rows <- tbl |> html_elements("tr.playerRow")
  
  for (row in player_rows) {
    cells <- row |> html_elements("td")
    if (length(cells) < 9) next
    
    pos <- cells[[1]] |> html_text(trim = TRUE)
    
    # Player name from the link
    player_link <- cells[[2]] |> html_element("a.playerLink")
    player_name <- if (!is.na(player_link)) html_text(player_link, trim = TRUE) else ""
    
    # Position and team from span
    pos_team <- cells[[2]] |> html_element("span.playerPositionAndTeam")
    pos_team_text <- if (!is.na(pos_team)) html_text(pos_team, trim = TRUE) else ""
    
    # CBS player ID from the link href
    href <- if (!is.na(player_link)) html_attr(player_link, "href") else ""
    cbs_id <- gsub(".*playerpage/", "", href)
    
    # Salary is the 9th column
    salary_text <- cells[[9]] |> html_text(trim = TRUE)
    
    # Total points is the last column (12th)
    total_pts <- if (length(cells) >= 12) cells[[12]] |> html_text(trim = TRUE) else NA
    
    all_rosters[[length(all_rosters) + 1]] <- data.frame(
      team = team_name,
      player_type = player_type,
      roster_pos = pos,
      player_name = player_name,
      eligible_positions = pos_team_text,
      cbs_id = cbs_id,
      salary = salary_text,
      total_fpts = total_pts,
      stringsAsFactors = FALSE
    )
  }
}

roster_df <- bind_rows(all_rosters)

cat("=== PARSED DATA SUMMARY ===\n")
cat("Total players across all teams:", nrow(roster_df), "\n")
cat("Teams found:", length(unique(roster_df$team)), "\n")
cat("Teams:\n")
for (tm in sort(unique(roster_df$team))) {
  n <- sum(roster_df$team == tm)
  cat(sprintf("  %-40s (%d players)\n", tm, n))
}

cat("\n=== SAMPLE: First team's roster ===\n\n")
first_team <- sort(unique(roster_df$team))[1]
team_data <- roster_df |> filter(team == first_team)
print(team_data |> select(roster_pos, player_name, eligible_positions, salary, total_fpts), n = 30)

cat("\n\n=== SALARY DISTRIBUTION ===\n")
# Clean salary - remove * suffix and convert
roster_df$salary_clean <- as.numeric(gsub("[^0-9.]", "", roster_df$salary))
roster_df$has_asterisk <- grepl("\\*", roster_df$salary)

cat("Salary range: $", min(roster_df$salary_clean, na.rm = TRUE), 
    "to $", max(roster_df$salary_clean, na.rm = TRUE), "\n")
cat("Players with * (likely minor league/minimum):", sum(roster_df$has_asterisk, na.rm = TRUE), "\n")
cat("Players with salary > $20:", sum(roster_df$salary_clean > 20, na.rm = TRUE), "\n")

cat("\n=== TOP 10 HIGHEST SALARIES ===\n")
top_salaries <- roster_df |> 
  arrange(desc(salary_clean)) |> 
  head(10) |>
  select(team, player_name, roster_pos, salary, total_fpts)
print(top_salaries)

cat("\n=== DATA SAVED ===\n")
# Save to CSV for easy use
write.csv(roster_df, "exploration/sample-data/parsed_rosters_salaries.csv", row.names = FALSE)
cat("Saved to: exploration/sample-data/parsed_rosters_salaries.csv\n")

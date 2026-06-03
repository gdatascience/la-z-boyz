#!/usr/bin/env Rscript
library(rvest)
library(dplyr)

page <- read_html("exploration/sample-data/rosters_and_salaries.html")
tables <- page |> html_elements("table.data")

all_rosters <- list()
for (tbl in tables) {
  title_row <- tbl |> html_element("tr.title td")
  if (is.na(title_row)) next
  team_raw <- title_row |> html_text(trim = TRUE)
  player_type <- ifelse(grepl("Pitchers$", team_raw), "Pitcher", "Batter")
  team_name <- gsub("\\s*(Batters|Pitchers)$", "", team_raw)
  player_rows <- tbl |> html_elements("tr.playerRow")
  for (row in player_rows) {
    cells <- row |> html_elements("td")
    if (length(cells) < 9) next
    pos <- cells[[1]] |> html_text(trim = TRUE)
    player_link <- cells[[2]] |> html_element("a.playerLink")
    player_name <- if (!is.na(player_link)) html_text(player_link, trim = TRUE) else ""
    pos_team <- cells[[2]] |> html_element("span.playerPositionAndTeam")
    pos_team_text <- if (!is.na(pos_team)) html_text(pos_team, trim = TRUE) else ""
    href <- if (!is.na(player_link)) html_attr(player_link, "href") else ""
    cbs_id <- gsub(".*playerpage/", "", href)
    salary_text <- cells[[9]] |> html_text(trim = TRUE)
    total_pts <- if (length(cells) >= 12) cells[[12]] |> html_text(trim = TRUE) else NA
    all_rosters[[length(all_rosters) + 1]] <- data.frame(
      team = team_name, player_type = player_type, roster_pos = pos,
      player_name = player_name, eligible_positions = pos_team_text,
      cbs_id = cbs_id, salary = salary_text, total_fpts = total_pts,
      stringsAsFactors = FALSE
    )
  }
}
roster_df <- bind_rows(all_rosters)

cat("Total players:", nrow(roster_df), "\n")
cat("Teams:", length(unique(roster_df$team)), "\n\n")

# Show Ask Me About My Willy roster
cat("=== Ask Me About My Willy ===\n")
my_team <- roster_df |> filter(team == "Ask Me About My Willy")
for (i in seq_len(nrow(my_team))) {
  r <- my_team[i, ]
  cat(sprintf("  %-3s %-22s %-14s $%-5s %s pts\n", 
    r$roster_pos, r$player_name, r$eligible_positions, r$salary, r$total_fpts))
}

cat("\n=== TOP 15 SALARIES ACROSS LEAGUE ===\n")
roster_df$salary_num <- as.numeric(gsub("[^0-9.]", "", roster_df$salary))
top <- roster_df |> arrange(desc(salary_num)) |> head(15)
for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  cat(sprintf("  $%-5s %-22s %-14s (%s)\n", r$salary, r$player_name, r$eligible_positions, r$team))
}

write.csv(roster_df, "exploration/sample-data/parsed_rosters_salaries.csv", row.names = FALSE)
cat("\nSaved CSV to exploration/sample-data/parsed_rosters_salaries.csv\n")

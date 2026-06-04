library(here)
valuations <- readRDS(here("data/cache/valuations.rds"))

f <- valuations[grepl("fuentes", tolower(valuations$player_name)), ]
if (nrow(f) > 0) {
  cat("Didier Fuentes in valuations:\n")
  print(f[, c("player_name", "position", "player_type", "proj_pts_per_week",
              "dollar_value", "surplus_value", "keeper_value_3yr", "keeper_value_5yr")])
} else {
  cat("Didier Fuentes: NOT in valuations\n")
}

## Seed prospect rankings with data gathered during 2026-06-04 trade analysis
library(here)
source(here("R/ingest/fetch_prospect_rankings.R"))

rankings <- create_empty_rankings()

# Thomas White — LHP, Miami Marlins
rankings <- update_prospect_ranking(rankings, "Thomas White", rank = 9,
  source = "FanGraphs", tier = "elite", fv_grade = 60L, age = 21L,
  position = "SP", mlb_team = "MIA", level = "AAA", eta = 2026L,
  notes = "Top LHP prospect in baseball. Dominating AAA.")

rankings <- update_prospect_ranking(rankings, "Thomas White", rank = 14,
  source = "MLB Pipeline", tier = "elite", age = 21L,
  position = "SP", mlb_team = "MIA", level = "AAA", eta = 2026L,
  notes = "Was #17 preseason, rose to #14 midseason. Near MLB-ready.")

# Zyhir Hope — OF, Los Angeles Dodgers
rankings <- update_prospect_ranking(rankings, "Zyhir Hope", rank = 22,
  source = "MLB Pipeline", tier = "high", age = 21L,
  position = "OF", mlb_team = "LAD", level = "AA", eta = 2027L,
  notes = "Dodgers #2 prospect. Power-hitting OF. .291 at AA with grand slams.")

# Konnor Griffin — SS, Pittsburgh Pirates
rankings <- update_prospect_ranking(rankings, "Konnor Griffin", rank = 1,
  source = "MLB Pipeline", tier = "elite", age = 20L,
  position = "SS", mlb_team = "PIT", level = "MLB", eta = 2026L,
  notes = "On 10-day IL (flexor strain, R elbow) as of May 31 2026. Expected back ~June 10. .333/21HR/65SB in minors.")

# Luis Pena — SS, Milwaukee Brewers
rankings <- update_prospect_ranking(rankings, "Luis Pena", rank = 19,
  source = "MLB Pipeline", tier = "high", age = 19L,
  position = "SS", mlb_team = "MIL", level = "A+", eta = 2028L,
  notes = "Medical scare in April. Returned May 27. .372/.462/.512 before IL.")

# Ryan Sloan — RHP, Seattle Mariners
rankings <- update_prospect_ranking(rankings, "Ryan Sloan", rank = 10,
  source = "Baseball America", tier = "elite", age = 20L,
  position = "SP", mlb_team = "SEA", level = "AA", eta = 2027L,
  notes = "Best pitching prospect in baseball per some outlets. 6 perfect IP, 11K recently.")

# Noah Schultz — LHP, Chicago White Sox
rankings <- update_prospect_ranking(rankings, "Noah Schultz", rank = 27,
  source = "MLB Pipeline", tier = "high", age = 22L,
  position = "SP", mlb_team = "CHW", level = "MLB", eta = 2026L,
  notes = "Made MLB debut April 2026. 5.82 ERA in 8 starts. On 15-day IL. Elite stuff (6-10 LHP).")

# Edwin Arroyo — SS, Cincinnati Reds
rankings <- update_prospect_ranking(rankings, "Edwin Arroyo", rank = 100,
  source = "MLB Pipeline", tier = "low", age = 22L,
  position = "SS", mlb_team = "CIN", level = "MLB", eta = 2026L,
  notes = "Called up June 2026 (Elly on IL). .323/.945 OPS/11HR at AAA.")

# Jackson Jobe — RHP, Detroit Tigers
rankings <- update_prospect_ranking(rankings, "Jackson Jobe", rank = 6,
  source = "MLB Pipeline", tier = "elite", age = 22L,
  position = "SP", mlb_team = "DET", level = "AAA", eta = 2026L,
  notes = "On Pocket Pancakes fantasy roster.")

# Jacob Misiorowski — RHP, Milwaukee Brewers
rankings <- update_prospect_ranking(rankings, "Jacob Misiorowski", rank = 35,
  source = "MLB Pipeline", tier = "mid", age = 23L,
  position = "SP", mlb_team = "MIL", level = "MLB", eta = 2026L,
  notes = "On Pocket Pancakes fantasy roster.")

save_prospect_rankings(rankings)
cat("Done! Seeded", nrow(rankings), "prospect ranking entries.\n")

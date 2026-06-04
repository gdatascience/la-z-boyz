library(here)
source(here("R/analysis/trade_scorer.R"))

cat("==========================================================\n")
cat("  GELOF PLAYING TIME ADJUSTMENT\n")
cat("==========================================================\n")
cat("Muncy (hand) on rehab assignment June 2. Return ~mid-June.\n")
cat("Gelof goes from everyday 3B back to utility role (60-70% starts).\n")
cat("Adjusted pts/wk: 15.3 * 0.65 = ~10.0 pts/wk (part-time)\n")
cat("However: CBS fantasy lets you start him regardless of MLB lineup.\n")
cat("Realistic fantasy impact: ~12 pts/wk (starts most days, sits occasionally)\n\n")

GELOF_ADJ_PTS <- 12.0  # down from 15.3 with full-time assumption

# Re-score the key Gelof trades with adjusted pts/wk

cat("--- (1) Henderson + Fuentes + Gelof (was 72.6) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 4.5 + GELOF_ADJ_PTS) - 17.3,  # = +14.1 (was +17.4)
  surplus_change = (5.2 + 0 + 10.0) - 27.1,   # adjusted surplus too: -11.9
  keeper_3yr_receive = 80 + 12 + 8,           # Gelof NPV down slightly
  cap_freed = 37 - 8,
  prospect_bonus = 12,
  positional_fit = 20,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson + Fuentes + Gelof (Gelof adjusted)"), "\n\n")

cat("--- (2) Gelof + Early + Ashcraft (was 72.7) ---\n")
s <- score_trade(
  pts_wk_change = (GELOF_ADJ_PTS + 6.2 + 7.5) - 17.3,  # = +8.4 (was +11.7)
  surplus_change = (10.0 + 4.7 + 32.3) - 27.1,  # = +19.9
  keeper_3yr_receive = 8 + 9.1 + 60.0,         # = 77.1
  cap_freed = 37 - 3,
  prospect_bonus = 10,
  positional_fit = 20,
  injury_penalty = 0
)
cat(format_trade_score(s, "Gelof + Early + Ashcraft (Gelof adjusted)"), "\n\n")

cat("--- (3) Gelof + Fuentes + Ashcraft (was 72.6) ---\n")
s <- score_trade(
  pts_wk_change = (GELOF_ADJ_PTS + 4.5 + 7.5) - 17.3,  # = +6.7 (was +10.0)
  surplus_change = (10.0 + 0 + 32.3) - 27.1,   # = +15.2
  keeper_3yr_receive = 8 + 12 + 60.0,          # = 80.0
  cap_freed = 37 - 2,
  prospect_bonus = 12,
  positional_fit = 20,
  sp_ace_bonus = 5,
  injury_penalty = 0
)
cat(format_trade_score(s, "Gelof + Fuentes + Ashcraft (Gelof adjusted)"), "\n\n")

cat("--- (4) Griffin + Fuentes + Gelof (was 69.1) ---\n")
s <- score_trade(
  pts_wk_change = (0 + 4.5 + GELOF_ADJ_PTS) - 17.3,  # = -0.8 (was +2.5)
  surplus_change = (35.4 + 0 + 10.0) - 27.1,   # = +18.3
  keeper_3yr_receive = 84.1 + 12 + 8,          # = 104.1
  cap_freed = 37 - 2,
  prospect_bonus = 25,
  positional_fit = 20,
  injury_penalty = 8
)
cat(format_trade_score(s, "Griffin + Fuentes + Gelof (Gelof adjusted)"), "\n\n")

cat("--- (5) Henderson + Gelof (was 65.6) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + GELOF_ADJ_PTS) - 17.3,  # = +9.6 (was +12.9)
  surplus_change = (5.2 + 10.0) - 27.1,        # = -11.9
  keeper_3yr_receive = 80 + 8,                  # = 88
  cap_freed = 37 - 8,
  prospect_bonus = 10,
  positional_fit = 15,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson + Gelof (Gelof adjusted)"), "\n\n")

cat("--- (6) Gelof + Ashcraft (was 64.0) ---\n")
s <- score_trade(
  pts_wk_change = (GELOF_ADJ_PTS + 7.5) - 17.3,  # = +2.2 (was +5.5)
  surplus_change = (10.0 + 32.3) - 27.1,       # = +15.2
  keeper_3yr_receive = 8 + 60.0,                # = 68.0
  cap_freed = 37 - 2,
  prospect_bonus = 5,
  positional_fit = 20,
  injury_penalty = 0
)
cat(format_trade_score(s, "Gelof + Ashcraft (Gelof adjusted)"), "\n\n")

cat("=== TRADES WITHOUT GELOF (unchanged) ===\n\n")

cat("--- Henderson + Ashcraft (71.6 - no change) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 7.5) - 17.3,
  surplus_change = (5.2 + 32.3) - 27.1,
  keeper_3yr_receive = 80 + 60.0,
  cap_freed = 37 - 8,
  prospect_bonus = 10,
  positional_fit = 20,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson + Ashcraft (unchanged)"), "\n\n")

cat("--- Henderson + Fuentes + Early (63.2 - no change) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 4.5 + 6.2) - 17.3,
  surplus_change = (5.2 + 0 + 4.7) - 27.1,
  keeper_3yr_receive = 80 + 12 + 9.1,
  cap_freed = 37 - 8,
  prospect_bonus = 15,
  positional_fit = 15,
  sp_ace_bonus = 5,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson + Fuentes + Early (unchanged)"), "\n\n")

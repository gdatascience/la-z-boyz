library(here)
source(here("R/analysis/trade_scorer.R"))

cat("==========================================================\n")
cat("  BIG AMISH TRADES INCLUDING DIDIER FUENTES ($0)\n")
cat("==========================================================\n")
cat("Fuentes: 20yo RHP, ATL #3 prospect, $0 salary\n")
cat("2026 MLB: 2-0, 3.45 ERA, 19K in 15.2 IP (relief/spot starts)\n")
cat("2026 AAA: 1-1, 2.16 ERA, 20K in 16.2 IP\n")
cat("Profile: Power arm (94-98 mph), debuted at 20, still developing\n")
cat("Model: 0.84 pts/wk (severely undervalued - limited data)\n")
cat("Real value: ~$0 salary, top-30 org prospect, MLB contributor at 20\n\n")

# Key reference values:
# JRam: pts 17.3, surplus $27.1, 3yr NPV $47.3, salary $37
# Henderson: pts 14.9, surplus $5.2, 3yr NPV adjusted ~$80, salary $7
# Griffin: pts 16.1, surplus $35.4, 3yr NPV $84.1, salary $1* (ON IL)
# Gelof: pts 15.3, surplus $13.2, 3yr NPV $13.3, salary $1
# Fuentes: pts 0.84 (model), real ~4-5 pts/wk as SP prospect, salary $0
#   Real 3yr NPV: ~$10-15 (young arm, cheap, MLB-adjacent, high ceiling)

# (A) Henderson + Fuentes
cat("--- (A) JRam for Henderson ($7) + Fuentes ($0) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 4.5) - 17.3,        # Henderson + Fuentes ~4.5 real pts
  surplus_change = (5.2 + 0) - 27.1,           # = -21.9
  keeper_3yr_receive = 80 + 12,                # Henderson adjusted + Fuentes ~$12
  cap_freed = 37 - 7,                           # = 30
  prospect_bonus = 12,   # Fuentes is a 20yo MLB pitcher, org top-3
  positional_fit = 15,   # Henderson fills SS
  sp_ace_bonus = 0,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson ($7) + Fuentes ($0)"), "\n\n")

# (B) Henderson + Fuentes + Gelof
cat("--- (B) JRam for Henderson ($7) + Fuentes ($0) + Gelof ($1) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 4.5 + 15.3) - 17.3, # = +17.4
  surplus_change = (5.2 + 0 + 13.2) - 27.1,   # = -8.7
  keeper_3yr_receive = 80 + 12 + 13.3,        # = 105.3
  cap_freed = 37 - 8,                           # = 29
  prospect_bonus = 12,
  positional_fit = 20,   # Henderson SS + Gelof 3B + Fuentes SP
  sp_ace_bonus = 0,
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson ($7) + Fuentes ($0) + Gelof ($1)"), "\n\n")

# (C) Griffin + Fuentes
cat("--- (C) JRam for Griffin ($1*) + Fuentes ($0) ---\n")
s <- score_trade(
  pts_wk_change = (0 + 4.5) - 17.3,           # Griffin on IL = 0 immediate pts
  surplus_change = (35.4 + 0) - 27.1,         # = +8.3
  keeper_3yr_receive = 84.1 + 12,             # = 96.1
  cap_freed = 37 - 1,                          # = 36
  prospect_bonus = 25,   # Griffin #1 prospect + Fuentes org top-3
  positional_fit = 5,    # Griffin is SS (have Witt), no 3B fill, Fuentes SP
  sp_ace_bonus = 0,
  injury_penalty = 10    # Griffin flexor strain
)
cat(format_trade_score(s, "Griffin ($1*) + Fuentes ($0)"), "\n\n")

# (D) Griffin + Fuentes + Gelof (Gelof covers 3B!)
cat("--- (D) JRam for Griffin ($1*) + Fuentes ($0) + Gelof ($1) ---\n")
s <- score_trade(
  pts_wk_change = (0 + 4.5 + 15.3) - 17.3,   # = +2.5 (Griffin 0 on IL)
  surplus_change = (35.4 + 0 + 13.2) - 27.1,  # = +21.5
  keeper_3yr_receive = 84.1 + 12 + 13.3,      # = 109.4
  cap_freed = 37 - 2,                          # = 35
  prospect_bonus = 25,   # Griffin #1 + Fuentes
  positional_fit = 20,   # Gelof covers 3B immediately, Griffin SS when back
  sp_ace_bonus = 0,
  injury_penalty = 8     # Griffin flexor strain (short-term per Pirates)
)
cat(format_trade_score(s, "Griffin ($1*) + Fuentes ($0) + Gelof ($1)"), "\n\n")

# (E) Henderson + Fuentes + Early
cat("--- (E) JRam for Henderson ($7) + Fuentes ($0) + Early ($1*) ---\n")
s <- score_trade(
  pts_wk_change = (14.9 + 4.5 + 6.2) - 17.3, # = +8.3
  surplus_change = (5.2 + 0 + 4.7) - 27.1,   # = -17.2
  keeper_3yr_receive = 80 + 12 + 9.1,         # = 101.1
  cap_freed = 37 - 8,                          # = 29
  prospect_bonus = 15,   # Fuentes + Early both young arms
  positional_fit = 15,   # Henderson SS + two SP prospects
  sp_ace_bonus = 5,      # two young arms for rotation
  injury_penalty = 0
)
cat(format_trade_score(s, "Henderson ($7) + Fuentes ($0) + Early ($1*)"), "\n\n")

# (F) Gelof + Fuentes + Ashcraft (no Henderson or Griffin)
cat("--- (F) JRam for Gelof ($1) + Fuentes ($0) + Ashcraft ($1) ---\n")
s <- score_trade(
  pts_wk_change = (15.3 + 4.5 + 7.5) - 17.3, # = +10.0
  surplus_change = (13.2 + 0 + 32.3) - 27.1,  # = +18.4
  keeper_3yr_receive = 13.3 + 12 + 60.0,      # = 85.3
  cap_freed = 37 - 2,                          # = 35
  prospect_bonus = 12,   # Fuentes young arm
  positional_fit = 20,   # Gelof 3B + Ashcraft SP + Fuentes SP depth
  sp_ace_bonus = 5,
  injury_penalty = 0
)
cat(format_trade_score(s, "Gelof ($1) + Fuentes ($0) + Ashcraft ($1)"), "\n\n")

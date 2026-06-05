---
name: Trade Evaluation
description: Evaluate a proposed fantasy baseball trade across surplus value, pts/week, salary cap, keeper implications, and positional fit.
---

# Skill: Trade Evaluation

Evaluate a proposed fantasy baseball trade using the project's trade analysis engine.

## Workflow

1. User provides: players they would give, players they would receive, and their team name
2. Load required data from `data/cache/`:
   - `valuations.rds` (player dollar values, surplus, keeper value)
   - `rosters.rds` (full league rosters with salaries)
   - `standings.rds` (current standings for competitive context)
   - Optionally: `draft_*.rds` files for prospect analysis
3. Call `analyze_trade(give, receive, my_team, valuations, rosters, standings)`
4. Present results in a clear summary
5. When comparing multiple trades, use the **Trade Scoring System** below

## Analysis Dimensions

The trade analyzer evaluates across these dimensions:
- **Net surplus value**: Dollar value difference (positive = trade favors you)
- **Pts/week change**: Projected points per week gained/lost, split by batting and pitching
- **Salary cap impact**: Net salary change, new team total, cap compliance ($300 in-season cap)
- **Keeper implications**: 3-year NPV surplus for each player, salary escalation paths
- **Positional impact**: Positions improved, weakened, overfilled, or left empty
- **Prospect analysis**: For minor league contract players — tier, draft pedigree, 5-year keeper value
- **Lopsidedness detection**: Flags trades where value difference exceeds 20%
- **Competitive context**: Contend/rebuild/middle mode adjusts value vs. pts weighting
- **Trade partner motivation**: Assess whether the other owner's roster makes this realistic
- **Injury & playing time check** (REQUIRED): For each player being received, web-search for:
  1. Current IL status or recent IL stint
  2. Playing time context — are they starting due to a teammate's injury?
  3. Teammate return timelines that could reduce their role
  4. Apply injury_penalty to score and adjust pts/wk projections downward for part-time players

## Trade Scoring System (for comparing multiple trades)

Three components weighted by competitive context:

| Component | Weight | Metric |
|-----------|--------|--------|
| Immediate Impact | 30% | Pts/wk change (0 = 50pts, +17.28 = 100, -17.28 = 0) |
| Value & Keeper | 50% | Surplus change (40%) + 3yr keeper NPV comparison (60%) |
| Strategic Fit | 20% | Cap freed + prospect bonuses + positional fit - league dynamic penalties |

**Grade scale:** A (80+), B+ (65–79), B (56–64), B- (53–55), C+ (48–55), C (40–47), D (below 40), F (below 10)

### Known Model Limitations — Apply Manual Adjustments

- **SP pts/wk is structurally broken for aces**: The model averages SP production across all weeks (including IL/bye weeks), producing ~4-8 pts/wk for aces. In reality, a healthy elite SP produces 15-25 pts on start weeks and 30-40+ on two-start weeks. In H2H, this lumpiness is enormously valuable — a single dominant start can swing a weekly matchup. When evaluating SP in trades:
  - Do NOT use the model's raw pts/wk for any SP performing at ace level. Manually estimate: healthy ace = 10-12 pts/wk (averaging start and non-start weeks over a season).
  - Apply SP ace bonus aggressively (+10-15) for any top-10 SP who is currently healthy and pitching well.
  - Losing an ace mid-season is worse than the numbers suggest: replacement-level SP starts actively lose points (negative outings), they don't just produce less.
  - Two-start weeks from an ace are worth ~35-40 pts — equivalent to an elite batter's best week. Factor this into immediate impact assessment.
- **New/limited-data players are undervalued**: Players with <50 games of 2026 data (e.g., Chourio returning from IL, Henderson in a slump) get punished by the projection model. Override 3yr keeper NPV with real-world assessment for established young stars.
- **Prospects with no MLB stats get $0 valuation**: Thomas White, Zyhir Hope, etc. Use industry prospect rankings (FanGraphs FV, MLB Pipeline, Just Baseball Top 100) to assign prospect bonus points (elite top-10: +30, top-25: +20, top-50: +15, top-100: +10).
- **"Rental" players with toxic contracts**: When receiving a player you plan to drop after the season (e.g., Bregman $44), only count their pts/wk for immediate impact — exclude them from keeper NPV entirely.

## Recommendation Logic

- **Contend mode** (playoff prob ≥60%): weights immediate pts/week higher (60% pts, 40% value)
- **Rebuild mode** (playoff prob ≤25%): weights surplus/keeper value higher (80% value, 20% pts)
- **Middle mode**: balanced (60% value, 40% pts)
- Recommendation output: "accept", "reject", or "counter"

## Strategic Considerations (beyond raw numbers)

- **Don't feed the league leader**: Apply a strategic penalty (-20 to -30 strategic fit) when trading with a team that's dominating the standings.
- **Evaluate trade partner's roster for realism**: A trade only works if the other owner would accept. Check:
  - Does the player you're asking for fill a redundancy on their roster? (Good)
  - Is it their best player at a position? (Bad — they won't trade it)
  - Are they contending? (They want immediate impact players)
  - Are they rebuilding? (They want cheap young keepers)
- **Salary dump awareness**: When the other team offers a high-salary player alongside value pieces, they may be trying to dump a bad contract on you. Evaluate whether the value pieces compensate for the salary anchor.

## Key Constants

- Salary cap: $300 (in-season)
- League size: 16 teams
- Keeper escalation: +$4/year standard; minor track: $0→$1→$2→$3 then +$4/year
- Discount rate for NPV: 0.9
- Max roster: 27 (22 active + 5 minor league)

## Output Format

Present the trade evaluation with:
1. One-line recommendation (accept/reject/counter)
2. Summary table of value exchanged on each side
3. Pts/week impact (total, batting, pitching)
4. Salary cap status
5. Keeper value comparison (3-year NPV)
6. Positional fit assessment
7. Trade score (if comparing multiple options)
8. Justification paragraph explaining the reasoning

When evaluating multiple trades, output a ranked comparison table with scores and grades.

## Output Location

Save comprehensive trade analyses to `analysis/` folder (gitignored) with format:
`analysis/YYYY-MM-DD_<description>.md`

## Key Files

- #[[file:R/analysis/trade_analyzer.R]]
- #[[file:R/utils/player_linker.R]]
- #[[file:R/utils/keeper_value.R]]
- #[[file:R/utils/salary_rules.R]]

## Example Prompts

- "Evaluate trading Jose Ramirez for Jackson Chourio and Max Meyer."
- "Grade these 3 trade offers for Jose Ramirez and rank them."
- "Pocket Pancakes offered me X, Y, Z for Jose Ramirez. Should I accept?"

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

## Recommendation Logic

- **Contend mode** (playoff prob ≥60%): weights immediate pts/week higher (60% pts, 40% value)
- **Rebuild mode** (playoff prob ≤25%): weights surplus/keeper value higher (80% value, 20% pts)
- **Middle mode**: balanced (60% value, 40% pts)
- Recommendation output: "accept", "reject", or "counter"

## Key Constants

- Salary cap: $300 (in-season)
- League size: 16 teams
- Keeper escalation: +$4/year standard; minor track: $0→$1→$2→$3 then +$4/year
- Discount rate for NPV: 0.9

## Output Format

Present the trade evaluation with:
1. One-line recommendation (accept/reject/counter)
2. Summary table of value exchanged on each side
3. Pts/week impact (total, batting, pitching)
4. Salary cap status
5. Keeper value comparison (3-year NPV)
6. Positional fit assessment
7. Justification paragraph explaining the reasoning

## Key Files

- #[[file:R/analysis/trade_analyzer.R]]
- #[[file:R/utils/player_linker.R]]
- #[[file:R/utils/keeper_value.R]]
- #[[file:R/utils/salary_rules.R]]

## Example Prompt

"Evaluate trading Corbin Carroll and Marcus Stroman for Julio Rodriguez. My team is La-Z-Boyz."

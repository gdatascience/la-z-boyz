---
name: Keeper Selection
description: Evaluate which players to keep based on multi-year NPV surplus, salary escalation, and positional need.
---

# Skill: Keeper Selection

Evaluate which players to keep for next season based on multi-year surplus value, salary trajectory, and positional need.

## Workflow

1. User provides: their team name and optionally specific players they're considering
2. Load required data from `data/cache/`:
   - `valuations.rds` (dollar values, keeper_value_3yr, keeper_value_5yr)
   - `rosters.rds` (salaries, contract types, minor league track info)
   - Optionally: `draft_*.rds` for prospect tier assessment
3. For each rostered player, compute:
   - Salary escalation path (3 and 5 years)
   - Projected dollar value over time
   - NPV surplus using `compute_keeper_surplus()`
4. Rank players by keeper value and present recommendations

## Salary Escalation Rules

- **Standard contracts**: Current salary + $4 per year of keeping
  - Example: $10 today → $14, $18, $22, $26, $30 over 5 years
- **Minor league contracts**: $0 → $1 → $2 → $3 → then +$4/year from $3
  - Example (year 0): $0 → $1, $2, $3, $7, $11 over 5 years
  - Example (year 2): $2 → $3, $7, $11, $15, $19 over 5 years

## Valuation Methodology

- `keeper_value_3yr`: NPV of (projected_value - projected_salary) over 3 years, discounted at 0.9/year
- `keeper_value_5yr`: Same over 5 years
- Value projection assumes slight annual decay (0.95× per year for established players)
- Prospects use tier-based value ramps: elite ($25 base), high ($18), mid ($12), low ($7)

## Decision Framework

Rank keepers by:
1. **NPV surplus (3yr)** — primary sort for win-now teams
2. **NPV surplus (5yr)** — primary sort for rebuilding teams
3. **Positional scarcity** — premium positions (C, SS) get tiebreaker edge
4. **Age/trajectory** — younger players with rising value preferred over declining veterans
5. **Minor league upside** — prospects on cheap contracts have asymmetric upside
6. **Industry prospect rankings** — for players without MLB stats, use FanGraphs FV, MLB Pipeline Top 100, or Just Baseball rankings to manually assess keeper value (the model assigns $0 to unproven prospects)

## Key Constants

- Salary cap: $260 (draft/keeper budget per team)
- Keeper escalation: +$4/year standard
- Minor track: $0→$1→$2→$3 then +$4/year
- Discount rate: 0.9 per year
- Value decay: 0.95× per year for established players
- Total pool: $4,160 (16 teams × $260)

## Output Format

Present keeper recommendations with:
1. Ranked list of recommended keepers (best to worst)
2. For each player:
   - Current salary → next year salary → 3-year salary path
   - Projected dollar value
   - 3-year NPV surplus
   - 5-year NPV surplus (for prospects/young players)
   - Position and contract type
3. Players to cut (negative keeper value — better to release and re-draft)
4. Total keeper salary committed vs. remaining draft budget
5. Positional coverage assessment of kept roster

## Key Files

- #[[file:R/utils/keeper_value.R]]
- #[[file:R/analysis/player_valuation.R]]
- #[[file:R/utils/salary_rules.R]]

## Example Prompt

"Help me pick my keepers. My team is La-Z-Boyz. I'm especially unsure about keeping Gunnar Henderson at $28 vs. Jackson Holliday at $3*."

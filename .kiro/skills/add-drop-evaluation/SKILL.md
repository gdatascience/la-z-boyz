---
name: Add/Drop Evaluation
description: Evaluate a waiver wire pickup with FAAB bid recommendation, surplus comparison, and roster fit analysis.
---

# Skill: Add/Drop Evaluation

Evaluate a potential waiver wire pickup using the project's FAAB recommendation engine.

## Workflow

1. User provides: target player to add, candidate to drop (optional), their team name, and remaining FAAB budget
2. Load required data from `data/cache/`:
   - `valuations.rds` (player dollar values, surplus)
   - `rosters.rds` (full league rosters with salaries)
3. Call `recommend_faab(my_team, valuations, rosters, remaining_faab, weeks_remaining)` for broad recommendations, OR perform a targeted comparison between the add target and drop candidate
4. Present results with a clear bid recommendation

## Analysis Dimensions

- **Surplus value comparison**: Target's projected value vs. drop candidate's projected value
- **Suggested FAAB bid**: Based on surplus value, remaining budget, weeks remaining, and competition
- **Positional fit**: Does the target fill a need or create a logjam?
- **Salary cap compliance**: Adding at FAAB bid amount must keep team under $300
- **Roster size**: 27-man maximum — must drop if full
- **Recency weighting**: 30% recent performance + 70% ROS projection
- **Promotion threshold flags**: Minor leaguers approaching 130 AB or 50 IP

## Bid Calculation Formula

The bid accounts for:
- **Surplus factor**: Player's surplus value normalized to 0–1 scale (where $50 surplus = 1.0)
- **Urgency factor**: Early season (0.7×) → mid (0.9×) → late (1.1×) → final weeks (1.3×)
- **Competition multiplier**: Scales 1–5 based on player's surplus percentile among free agents
- Base bid = remaining_budget × (surplus_factor × 0.30) × urgency × competition
- Floor: $1 minimum bid; Ceiling: remaining budget

## Key Constants

- In-season salary cap: $300
- Max roster size: 27
- FAAB minimum bid: $1
- Season FAAB budget: $250
- Regular season: 23 weeks
- Promotion thresholds: 130 AB (batters), 50 IP (pitchers)
- Recent performance weight: 30%; ROS projection weight: 70%

## Output Format

Present the add/drop evaluation with:
1. One-line recommendation (add/pass)
2. Target player profile: projected pts/week, dollar value, position
3. Drop candidate profile (if applicable): current surplus value, salary freed
4. Suggested FAAB bid amount with reasoning
5. Net roster improvement (surplus gained)
6. Risk factors (promotion threshold, injury, small sample)

## Key Files

- #[[file:R/analysis/waiver_recommender.R]]
- #[[file:R/utils/player_linker.R]]
- #[[file:R/utils/salary_rules.R]]
- #[[file:R/analysis/player_valuation.R]]

## Example Prompt

"Should I pick up Evan Carter? I'd drop Bryan De La Cruz. I have $180 FAAB remaining. My team is La-Z-Boyz."

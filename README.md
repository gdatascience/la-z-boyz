# La-Z-Boyz of Summer

Fantasy baseball analytics tool for the La-Z-Boyz of Summer league — a 16-team CBS Head-to-Head Points keeper league.

## What It Does

- **Player projections** — Marcel-style weighted projections using current + historical stats with aging curves
- **Dollar valuations** — Points Above Replacement converted to auction dollar values with positional scarcity adjustments
- **Keeper analysis** — Multi-year NPV surplus calculations factoring salary escalation
- **Trade evaluation** — Composite scoring system (0–100, letter grades A through F) comparing trades across immediate impact, keeper value, and strategic fit. Context-aware weighting for contend/middle/rebuild modes.
- **Prospect rankings** — Manually-maintained industry consensus rankings (FanGraphs, MLB Pipeline, Just Baseball) for players without MLB stats
- **Waiver recommendations** — FAAB bid suggestions based on surplus value, competition, and season timing
- **Roster optimization** — Lineup slot recommendations for weekly matchups

## Quick Start

```sh
# 1. Refresh player data (no CBS login needed)
Rscript R/ingest/fetch_players.R
Rscript R/ingest/fetch_rules.R

# 2. Update rosters (CSV export from CBS)
#    Roster Overview > All Teams > select period > CSV export button
#    Drop the file into data/imports/ (auto-detected by filename)
Rscript R/ingest/parse_rosters.R

# 3. Update standings (CSV export from CBS)
#    Standings > Overall > CSV export button
#    Save as data/imports/overall.csv
Rscript R/ingest/parse_standings.R

# 4. Run the full analysis pipeline
Rscript R/pipeline.R
```

## Project Structure

```
R/
├── pipeline.R          # Master orchestrator
├── ingest/             # Data ingestion (CBS API, CSV parsing, HTML parsing)
│   └── fetch_prospect_rankings.R  # Prospect tier lookups (manually maintained)
├── analysis/           # Core analytics
│   ├── evaluate_trade_offer.R     # One-call trade evaluation (lookups + scoring + output)
│   ├── trade_scorer.R             # 0-100 composite scoring for trade comparisons
│   ├── trade_analyzer.R           # Trade value engine
│   ├── player_valuation.R         # Dollar values via PAR + scarcity
│   ├── projection_model.R         # Statistical projections
│   ├── roster_optimizer.R         # Lineup optimization
│   └── waiver_recommender.R       # Waiver pickup recommendations
├── utils/              # Shared pure functions (scoring, salary rules, serialization)
└── reports/            # R Markdown report templates

data/
├── imports/            # CBS exports — CSV rosters/standings, HTML constitution (gitignored)
├── cache/              # Processed .rds data files (gitignored)
└── league_constitution.rds  # Parsed league rules (committed)

analysis/               # Trade evals, waiver recs, keeper analyses (gitignored, local only)

tests/
├── testthat/           # Property-based test suite (100+ iterations per property)
└── fixtures/           # Test HTML/data fixtures
```

## Data Refresh

| Data | Source | Command |
|------|--------|---------|
| Player database | CBS public API | `Rscript R/ingest/fetch_players.R` |
| League rules | CBS public API | `Rscript R/ingest/fetch_rules.R` |
| Stats (batting) | CBS public API | `Rscript R/ingest/fetch_stats.R` |
| Draft results | CBS public API | `Rscript R/ingest/fetch_draft.R` |
| Prospect rankings | Manual (web research) | Update via `fetch_prospect_rankings.R` functions |
| Rosters/salaries | CBS CSV export | Roster Overview → All Teams → CSV export → `data/imports/` → `Rscript R/ingest/parse_rosters.R` |
| Standings | CBS CSV export | Standings → Overall → CSV export → `data/imports/overall.csv` → `Rscript R/ingest/parse_standings.R` |
| Constitution | Manual HTML export | Save from CBS → `data/imports/constitution.html` → `Rscript R/ingest/parse_constitution.R` |

## League Configuration

- **Platform**: CBS Sports Fantasy Baseball
- **Format**: H2H Points (custom scoring weights)
- **Teams**: 16
- **Salary cap**: $260/team (keeper/draft budget), $300 in-season
- **Keeper rules**: Standard contracts +$4/year; minor league track $0→$1→$2→$3 then +$4/year
- **Roster**: C, 1B, 2B, 3B, SS, OF×3, U, SP×5, RP×2 (27-man)
- **FAAB**: $250 budget, $1 minimum bid

## Methodology

The analytics engine uses several established data science techniques adapted for fantasy baseball economics.

### Marcel Projections (Weighted Ensemble)

Rest-of-season projections use a [Modified Marcel system](https://github.com/gdatascience/la-z-boyz/blob/main/R/analysis/projection_model.R) — a multi-season weighted average with regression to the mean. Recent seasons carry more weight (5/4/3), then each stat is regressed toward league-average rates based on sample size:

```r
# Regression factor: more data → less regression toward league mean
reg_factor <- REGRESSION_PA / (REGRESSION_PA + total_weighted_pa)

# Blend player rate with league average
regressed_rate <- player_rate * (1 - reg_factor) + league_rate * reg_factor
```

Players with 3+ seasons of data get tight confidence intervals (±15%); rookies with one season get wide bands (±40%). This is a Bayesian-flavored shrinkage estimator — the prior is the league average, and the posterior converges toward the player's true talent as sample size grows.

### Age Curves

Projections are adjusted with position-specific [aging curves](https://github.com/gdatascience/la-z-boyz/blob/main/R/analysis/projection_model.R) that model the non-linear decline of MLB performance. Pre-peak players (< 27) get a slight growth adjustment; post-peak players decline with mild quadratic acceleration:

```r
# Post-peak decline accelerates with age (quadratic term)
adjustment <- 1 - (decline_rate * years_from_peak + 0.001 * years_from_peak^2)
```

### Dollar Valuation (Points Above Replacement)

[Player dollar values](https://github.com/gdatascience/la-z-boyz/blob/main/R/analysis/player_valuation.R) are computed via a PAR (Points Above Replacement) model with positional scarcity. Replacement level at each position is defined as the (slots × 16 + 1)th ranked player — then the total salary pool ($260 × 16 = $4,160) is distributed proportionally to each player's PAR.

### Keeper NPV (Net Present Value)

[Multi-year keeper surplus](https://github.com/gdatascience/la-z-boyz/blob/main/R/utils/keeper_value.R) is computed as discounted future value minus escalating salary:

```r
# Annual surplus discounted at 0.9 per year
discount_factors <- 0.9 ^ (1:years_ahead)
npv_surplus <- sum((projected_values - projected_salaries) * discount_factors)
```

Young players on minor contracts ($0→$1→$2→$3→+$4/year) generate outsized NPV because their salary stays far below market value through their peak years.

### Statcast Quality-of-Contact Adjustments

When available, [Statcast metrics](https://github.com/gdatascience/la-z-boyz/blob/main/R/analysis/projection_model.R) (exit velocity, barrel rate, xBA, xSLG) are used to adjust projections for batters whose traditional stats under- or over-perform their contact quality. The adjustment is capped at ±10% to avoid over-fitting to noisy batted-ball data.

## Trade Scoring System

Trades are scored 0–100 using three weighted components:

| Component | Default Weight | What it measures |
|-----------|----------------|------------------|
| Immediate Impact | 30% | Pts/wk change (batting + pitching) |
| Value & Keeper | 50% | Surplus change (40%) + 3yr keeper NPV (60%) |
| Strategic Fit | 20% | Cap freed + prospect bonus + positional fit − league dynamic penalties |

Weights shift by competitive mode: contenders weight immediate impact higher (40/40/20), rebuilders weight value higher (15/65/20).

**Grade scale:** A (80+), B+ (65–79), B (56–64), B- (53–55), C+ (48–55), C (40–47), D (below 40), F (below 10)

Usage:
```sh
# Trade evaluation uses evaluate_trade_offer.R — called via Kiro chat
# or source it directly in R:
source("R/analysis/evaluate_trade_offer.R")
result <- evaluate_offer(give = c("Jose Ramirez"), receive = c("Gunnar Henderson", "Braxton Ashcraft"))
print_evaluation(result)
```

## Running Tests

```sh
# Full property-based test suite
Rscript -e "testthat::test_dir('tests/testthat')"

# Single test file
Rscript -e "testthat::test_file('tests/testthat/test-property-scoring.R')"
```

## Requirements

- R (≥ 4.0)
- Packages: `here`, `testthat`, `rvest`, `xml2`, `httr`, `jsonlite`
- No tidyverse dependency in core modules (base R data frames throughout)

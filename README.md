# La-Z-Boyz of Summer

Fantasy baseball analytics tool for the La-Z-Boyz of Summer league — a 16-team CBS Head-to-Head Points keeper league.

## What It Does

- **Player projections** — Marcel-style weighted projections using current + historical stats with aging curves
- **Dollar valuations** — Points Above Replacement converted to auction dollar values with positional scarcity adjustments
- **Keeper analysis** — Multi-year NPV surplus calculations factoring salary escalation
- **Trade evaluation** — Multi-dimensional trade analysis: surplus value, pts/week impact, salary cap, positional fit, prospect upside
- **Waiver recommendations** — FAAB bid suggestions based on surplus value, competition, and season timing
- **Roster optimization** — Lineup slot recommendations for weekly matchups

## Quick Start

```sh
# 1. Refresh player data (no CBS login needed)
Rscript R/ingest/fetch_players.R
Rscript R/ingest/fetch_rules.R

# 2. Update rosters (requires manual HTML export from CBS)
#    Save "All Teams" page as HTML → drop into data/imports/rosters.html
Rscript R/ingest/parse_rosters.R

# 3. Run the full analysis pipeline
Rscript R/pipeline.R
```

## Project Structure

```
R/
├── pipeline.R          # Master orchestrator
├── ingest/             # Data ingestion from CBS (API + HTML parsing)
├── analysis/           # Core analytics (projections, valuations, trades, waivers)
├── utils/              # Shared pure functions (scoring, salary rules, serialization)
└── reports/            # R Markdown report templates

data/
├── imports/            # Raw HTML exports from CBS (gitignored)
├── cache/              # Processed .rds data files (gitignored)
└── league_constitution.rds  # Parsed league rules (committed)

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
| Rosters/salaries | Manual HTML export | Save from CBS → `data/imports/rosters.html` → `Rscript R/ingest/parse_rosters.R` |
| Standings | Manual HTML export | Save from CBS → `data/imports/standings.html` → `Rscript R/ingest/parse_standings.R` |
| Constitution | Manual HTML export | Save from CBS → `data/imports/constitution.html` → `Rscript R/ingest/parse_constitution.R` |

## League Configuration

- **Platform**: CBS Sports Fantasy Baseball
- **Format**: H2H Points (custom scoring weights)
- **Teams**: 16
- **Salary cap**: $260/team (keeper/draft budget), $300 in-season
- **Keeper rules**: Standard contracts +$4/year; minor league track $0→$1→$2→$3 then +$4/year
- **Roster**: C, 1B, 2B, 3B, SS, OF×3, U, SP×5, RP×2 (27-man)
- **FAAB**: $250 budget, $1 minimum bid

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

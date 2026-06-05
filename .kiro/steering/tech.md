# Tech Stack

## Language & Runtime

- R (scripting, not packaged as an R package)
- No DESCRIPTION file — modules are sourced directly via `source()`

## Key Libraries

- `here` — reliable path resolution in tests and scripts
- `testthat` — test framework (v3 edition with snapshot support)
- `rvest` / `xml2` — HTML parsing for CBS page imports
- `httr` / `jsonlite` — API calls to CBS endpoints
- Base R data frames (no tidyverse dependency in core modules)

## Data Format

- Intermediate/cached data: `.rds` files in `data/cache/`
- Metadata attached via R attributes (`source_file`, `parsed_at`, `league_id`)
- Imports: saved HTML pages from CBS dropped into `data/imports/`
- Reports: R Markdown (`.Rmd`) rendered to HTML

## Testing

- Framework: `testthat` (run from project root)
- Property-based tests use manual randomization with `set.seed()` + 100+ iterations (no `quickcheck` dependency currently)
- Test file naming: `test-property-*.R` (in `tests/testthat/`) and `test_*.R` (in `tests/`)

## Git Workflow

- This is a solo personal project — commit and push directly to `main`. No feature branches or PRs.
- Write clear commit messages but don't overthink process.

## Running R Code

- **Always write R code to a temp file and execute it** rather than using `Rscript -e '...'` inline. Shell quoting (single quotes, double quotes, dollar signs, backslashes) breaks multi-line R code when passed through the terminal.
- Pattern: write code to a `.R` file (e.g., `/tmp/analysis.R`), then run `Rscript /tmp/analysis.R`.
- For trivial one-liners that use only double quotes internally, `Rscript -e "..."` is acceptable.
- Never use single-quoted `Rscript -e '...'` with R code that itself contains single quotes, dollar signs, or backslashes.
- **Do NOT create one-off scratch scripts in `exploration/`** for trade analysis, waiver evaluation, or similar tasks. Instead, use the reusable evaluation modules:
  - `R/analysis/evaluate_trade_offer.R` — `evaluate_offer()` handles player lookups, manual overrides, scoring, and formatted output for any trade proposal.
  - `R/analysis/trade_scorer.R` — `score_trade()` for raw scoring when you already have the numbers.
  - `R/ingest/fetch_prospect_rankings.R` — `lookup_prospect()` for prospect tier lookups.
- Only create new exploration scripts for genuinely new functionality (e.g., testing a new API, prototyping a new module). Analysis of specific trades should go through the existing evaluation pipeline.

## Common Commands

```sh
# Run the full analysis pipeline
Rscript R/pipeline.R

# Run all testthat property tests
Rscript -e "testthat::test_dir('tests/testthat')"

# Run a specific test file
Rscript -e "testthat::test_file('tests/testthat/test-property-scoring.R')"

# Run legacy standalone tests
Rscript tests/test_player_valuation.R

# Refresh player data from CBS (no auth needed)
Rscript R/ingest/fetch_players.R

# Refresh league rules from CBS (no auth needed)
Rscript R/ingest/fetch_rules.R
```

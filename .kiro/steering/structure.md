# Project Structure

```
la-z-boyz/
├── R/
│   ├── pipeline.R              # Master orchestrator — runs ingest → project → value
│   ├── ingest/                 # Data ingestion (CBS API calls, HTML parsing)
│   │   ├── fetch_players.R     # Pull player database from CBS
│   │   ├── fetch_stats.R       # Pull batting/pitching stats by season
│   │   ├── fetch_rules.R       # Pull league scoring rules
│   │   ├── fetch_draft.R       # Pull draft results by year
│   │   ├── parse_constitution.R # Parse league constitution HTML
│   │   ├── parse_rosters.R     # Parse saved roster/salary HTML
│   │   └── parse_standings.R   # Parse saved standings HTML
│   ├── analysis/               # Core analytics modules
│   │   ├── projection_model.R  # Statistical projections (Marcel-style)
│   │   ├── player_valuation.R  # Dollar values via PAR + scarcity
│   │   ├── roster_optimizer.R  # Lineup optimization
│   │   ├── trade_analyzer.R    # Trade evaluation engine
│   │   └── waiver_recommender.R # Waiver pickup recommendations
│   ├── utils/                  # Shared pure-function utilities
│   │   ├── scoring.R           # Fantasy point computation
│   │   ├── keeper_value.R      # Salary projection & NPV surplus
│   │   ├── salary_rules.R      # Salary cap validation
│   │   ├── player_linker.R     # Cross-source player matching
│   │   └── serialization.R     # RDS/CSV I/O with metadata
│   └── reports/                # R Markdown report templates
│       ├── trade_report.Rmd
│       ├── valuation_report.Rmd
│       └── weekly_summary.Rmd
├── analysis/                   # Local analysis outputs (gitignored)
│   └── YYYY-MM-DD_*.md        # Trade evals, waiver recs, keeper analyses
├── data/
│   ├── imports/                # Raw HTML saved from CBS (gitignored)
│   ├── cache/                  # Processed .rds files (gitignored)
│   └── league_constitution.rds # Parsed constitution (committed)
├── tests/
│   ├── testthat/               # Property-based tests (primary suite)
│   │   ├── setup.R             # PBT iteration config
│   │   ├── helper-source.R     # Auto-sources all R modules
│   │   └── test-property-*.R   # One file per correctness property
│   ├── fixtures/               # Test HTML/data fixtures
│   └── test_*.R                # Legacy standalone test scripts
└── exploration/                # Scratch scripts and API experiments
```

## Conventions

- **Module pattern**: Each `.R` file under `R/` defines pure functions with roxygen-style `#'` docstrings. No top-level side effects except `pipeline.R`.
- **Data contracts**: The pipeline verifies column presence and types between steps using `verify_data_contract()`.
- **Error handling**: Graceful degradation — missing data files produce warnings and `NULL` returns rather than hard stops.
- **Naming**: Functions use `snake_case`. Files use `snake_case.R`. Test files prefix with `test-property-` (testthat) or `test_` (standalone).
- **No package structure**: Modules are loaded via `source()` with `here::here()` for path resolution. No namespace, no DESCRIPTION.
- **Player name matching**: CBS roster names and Baseball Reference stat names often differ (accents, nicknames like Louie/Louis, suffixes). Always use `normalize_name()` or `fuzzy_match_players()` from `player_linker.R` when comparing player names across data sources — never raw `tolower()` equality.

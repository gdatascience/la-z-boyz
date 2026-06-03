# Data Directory

This folder holds league data that is NOT committed to git. The actual data files are gitignored because they contain private league information.

## Structure

```
data/
├── imports/          # Drop saved HTML files here from CBS
│   └── rosters.html  # Save from: l-z-bs.baseball.cbssports.com/teams/all
├── cache/            # Auto-generated cached data (RDS format)
│   ├── players.rds   # Full CBS player database
│   ├── league_rules.rds  # Parsed league rules
│   └── rosters.rds   # Parsed roster/salary data
```

## How to refresh data

1. **Rosters & Salaries**: Log into CBS → go to your league → click "Teams" → "All Teams" → Save page as HTML (Cmd+S) → drop into `data/imports/`
2. **Player database**: Run `Rscript R/ingest/fetch_players.R` (no login needed)
3. **League rules**: Run `Rscript R/ingest/fetch_rules.R` (no login needed)

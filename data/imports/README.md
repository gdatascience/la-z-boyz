# Imports

Drop CBS export files here.

## To refresh roster/salary data:

1. Go to https://l-z-bs.baseball.cbssports.com/teams/roster-overview/all
2. Select the current period from the dropdown
3. Click the CSV export button (spreadsheet icon)
4. Save the downloaded file in this folder (filename like `roster-overview-all-12-20260608.csv`)
5. Run: `Rscript R/ingest/parse_rosters.R`

The parser auto-detects the most recent `roster-overview-all-*.csv` file by filename.

## To refresh standings:

1. Go to https://l-z-bs.baseball.cbssports.com/standings/overall
2. Click the CSV export button (spreadsheet icon)
3. Save the downloaded file as `overall.csv` in this folder
4. Run: `Rscript R/ingest/parse_standings.R`

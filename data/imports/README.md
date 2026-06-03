# Imports

Drop saved HTML files from CBS here.

## To refresh roster/salary data:

1. Go to https://l-z-bs.baseball.cbssports.com/teams/all
2. Save the page (Cmd+S / Ctrl+S) as "Webpage, Complete"
3. Save it as `rosters.html` in this folder (overwrite the old one)
4. Run: `Rscript R/ingest/parse_rosters.R`

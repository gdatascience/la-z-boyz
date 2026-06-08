# Test Fixtures

Minimal HTML fixtures for testing the CBS HTML parsing logic.

## Files

- **roster_sample.html** — Minimal CBS "All Teams" roster page with 2 teams (5 batters, 3 pitchers). Tests roster parsing including player names, positions, salaries, minor league contracts (asterisk), and fantasy points.

- **standings_sample.csv** — Minimal CBS standings CSV with 2 divisions and 4 teams. Tests division grouping, win/loss/tie parsing, points totals, and derived fields (games_played, ppg).

## Usage

Access fixtures in tests via the `fixtures_path()` helper defined in `helper-source.R`:

```r
html_file <- fixtures_path("roster_sample.html")
page <- rvest::read_html(html_file)
```

## Notes

- These are intentionally small to keep tests fast and focused.
- They mirror the structure of real CBS HTML but with fictional/test data.
- The roster fixture includes: standard contracts (`$25`), minor league contracts (`$2*`), accented names (`Félix`, `José`), and multiple eligible positions.

# Implementation Plan: Fantasy Baseball Decision Helper

## Overview

This plan builds the Fantasy Baseball Decision Helper from the ground up following the layered pipeline architecture: Ingest → Utils → Model → Decide → Report. Since the ingest scripts already exist, the first phase formalizes them with proper error handling and metadata. Subsequent phases build the utility layer, projection model, valuation engine, decision modules, and reporting layer. Property-based tests are interspersed close to each module for early error detection.

## Tasks

- [x] 1. Formalize ingest layer with error handling and metadata
  - [x] 1.1 Add error handling and caching fallback to fetch_players.R and fetch_rules.R
    - Add 60-second timeout to CBS API calls
    - On error/timeout, log with timestamp and return cached RDS if available
    - Add metadata attributes (source_file, parsed_at, league_id) to saved RDS files
    - Validate player count (~8,400) and rules structure after fetch
    - _Requirements: 1.1, 1.2, 1.5, 1.6_

  - [x] 1.2 Add error handling and metadata to fetch_stats.R and fetch_draft.R
    - Add exponential backoff retry (2s, 4s, 8s, max 3 retries) for Baseball Reference
    - Fallback to cached RDS on failure with cache-age warning
    - Store stats as season-specific RDS files (stats_batting_{year}.rds, stats_pitching_{year}.rds)
    - Support retrieval for at least 3 prior seasons
    - Add FanGraphs 403 silent fallback to Baseball Reference
    - _Requirements: 3.1, 3.2, 3.6, 3.7, 3.8, 3.9, 3.10, 3.11, 3.12_

  - [x] 1.3 Add error handling and validation to parse_rosters.R and parse_standings.R
    - If HTML import file is missing, display instructions with exact CBS URL and file path
    - Validate 16 teams found and player count between 350-450
    - Parse all required fields (team, player_type, roster_position, salary, minor contract flag, etc.)
    - Add metadata attributes to saved RDS
    - _Requirements: 1.3, 1.4, 1.5, 1.7, 1.8_

  - [x] 1.4 Add constitution validation to parse_constitution.R
    - Implement validation function that checks all required fields are present
    - Return TRUE for valid constitution, FALSE with missing field names for invalid
    - Display error if constitution RDS is missing or corrupted on load
    - _Requirements: 2.1, 2.10_

- [x] 2. Build utility layer — scoring and player linker
  - [x] 2.1 Implement R/utils/scoring.R
    - Implement `compute_batting_points(stats, weights)` using league scoring weights
    - Implement `compute_pitching_points(stats, weights)` using league scoring weights
    - Implement `points_per_week(total_points, games_played, games_per_week)`
    - Default weights from league_constitution.rds (1B=1, 2B=2, 3B=3, HR=4, grand_slam=2, cycle=5, R=1, RBI=1, BB=1, HBP=1, SB=2, CS=-1, K=-0.5; pitching: IP=3, K=0.5, W=5, L=-5, SV=7, HLD=5, QS=3, CG=2, NH=5, PG=5, H=-1, ER=-1, BB=-1, IBB=+1)
    - _Requirements: 2.2, 4.6, 5.1_

  - [x] 2.2 Write property test for scoring computation (Property 5)
    - **Property 5: Scoring Computation Correctness**
    - For any valid batting stat line, compute_batting_points shall equal the sum of each stat × its weight
    - Same additive property for pitching points
    - Use {quickcheck} to generate random stat lines within realistic bounds
    - **Validates: Requirements 4.6, 5.1**

  - [x] 2.3 Implement R/utils/player_linker.R
    - Implement `link_roster_to_stats(roster_df, stats_df)` using name + team + position matching
    - Implement `fuzzy_match_players(name_a, name_b, team_a, team_b)` with confidence scores
    - Implement `get_mlbam_id(player_name)` using baseballr::playerid_lookup()
    - Handle name variations: Jr./III suffixes, accents, abbreviated first names
    - _Requirements: 3.4, 3.5_

  - [x] 2.4 Write property test for player fuzzy matching (Property 4)
    - **Property 4: Player Fuzzy Matching Correctness**
    - For any player present in both CBS and BRef with matching MLB team, fuzzy match confidence ≥ 0.8
    - Generate name variations (suffixes, accents, abbreviations) and verify matching
    - **Validates: Requirements 3.5**

- [x] 3. Build utility layer — keeper value and salary rules
  - [x] 3.1 Implement R/utils/keeper_value.R
    - Implement `project_keeper_salary(current_salary, is_minor_contract, minor_track_year, years_ahead)`
    - Standard contract: salary + $4×N per year
    - Minor league track: $0→$1→$2→$3→then +$4/year from $3 base
    - Implement `compute_keeper_surplus(projected_values, projected_salaries, discount_rate)`
    - Return annual_surplus, total_surplus, npv_surplus
    - _Requirements: 2.3, 2.5, 2.6, 5.6, 5.7_

  - [x] 3.2 Write property test for keeper salary projection (Property 13)
    - **Property 13: Keeper Salary Projection**
    - For any standard contract with salary S, year N salary = S + 4×N
    - For minor league track at year Y, verify full salary progression
    - Keeper surplus for year N = (projected_value_N − projected_salary_N) × discount_rate^N
    - **Validates: Requirements 5.6, 5.7, 6.4**

  - [x] 3.3 Implement R/utils/salary_rules.R
    - Implement `check_salary_cap(roster_df, cap_type)` for auction/in_season/keeper caps
    - Cap values: auction=$260, in_season=$300, keeper=$80
    - Implement `compute_salary_impact(players_in, players_out, current_total)`
    - Return net_change, new_total, cap_compliant
    - _Requirements: 2.3, 6.3, 7.4_

- [x] 4. Checkpoint — Utilities complete
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 5. Build projection model
  - [ ] 5.1 Implement R/analysis/projection_model.R — core projection engine
    - Implement `marcel_project(seasons, weights, regression_pct)` with 5/4/3 weighting
    - Implement `apply_age_curve(projected_stats, age, position)` with peak at 27 (batters) / 28 (pitchers)
    - Implement `generate_projections(current_stats, historical, player_info)` for all MLB players
    - Handle players with < 3 seasons: use available data, mark as "limited" or "rookie" confidence
    - Floor negative counting stats at zero
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.9_

  - [ ] 5.2 Implement projection confidence intervals and fantasy points conversion
    - Implement `project_fantasy_points(projections, scoring_weights)` adding pts_per_week column
    - Generate confidence intervals (80% CI) based on data availability
    - Wider CI for players with fewer seasons of data
    - Incorporate Statcast batted-ball metrics when available
    - _Requirements: 4.5, 4.6, 4.7, 4.8_

  - [ ] 5.3 Write property tests for projection model (Properties 6, 7, 8, 9)
    - **Property 6: Projection Recency Weighting** — projection closer to recent season than oldest when difference > 20%
    - **Property 7: Age Curve Monotonicity** — age 35 produces lower stats than age 27 for same input
    - **Property 8: Confidence Interval Invariant** — lo ≤ projected ≤ hi, both non-negative
    - **Property 9: Confidence Width vs Data Availability** — 1 season data → wider CI than 3 seasons
    - **Validates: Requirements 4.2, 4.3, 4.4, 4.7**

- [ ] 6. Build player valuation engine
  - [ ] 6.1 Implement R/analysis/player_valuation.R
    - Implement `compute_replacement_level(projections, roster_slots)` — (N×16 + 1)th player at each position
    - Implement `assign_dollar_values(projections, replacement_levels, total_salary_pool)` — distribute $4,160 proportional to PAR
    - Implement `adjust_positional_scarcity(values, roster_slots)` — premium for thin positions (C, SS)
    - Compute surplus_value, keeper_value_3yr, keeper_value_5yr for all players
    - Exclude players with no projections from rankings
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8_

  - [ ] 6.2 Write property tests for valuation (Properties 10, 11, 12)
    - **Property 10: Dollar Value Conservation** — sum of positive dollar values = $4,160 ± $1
    - **Property 11: Replacement Level Definition** — replacement = (N×16+1)th player at position
    - **Property 12: Positional Scarcity Premium** — scarcer position gets higher value for same PAR
    - **Validates: Requirements 5.2, 5.3, 5.4**

- [ ] 7. Checkpoint — Projection and valuation complete
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 8. Build trade analyzer
  - [ ] 8.1 Implement R/analysis/trade_analyzer.R — core trade evaluation
    - Implement `analyze_trade(give, receive, my_team, valuations, rosters, standings)`
    - Compute net surplus value difference between sides
    - Compute projected pts/week change (batting + pitching breakdown)
    - Compute salary cap impact (net change, new total, compliance check vs $300)
    - Implement `assess_competitive_context(team_name, standings)` — contend/rebuild/middle mode
    - Reject trade if player not found in roster data
    - _Requirements: 6.1, 6.2, 6.3, 6.6, 6.10_

  - [ ] 8.2 Implement trade analyzer — keeper and positional analysis
    - Compute keeper implications: salary escalation path, 3-year keeper value, keeper eligibility
    - Assess positional impact (slots unfilled, over-filled, or improved)
    - Minor league player assessment: draft pedigree, salary track, prospect profile
    - Flag lopsided trades (>20% net value difference of total trade value)
    - Generate structured recommendation (accept/reject/counter) with justification
    - _Requirements: 6.4, 6.5, 6.7, 6.8, 6.9_

  - [ ] 8.3 Write property tests for trade analyzer (Properties 14, 15)
    - **Property 14: Trade Arithmetic Correctness** — net surplus, points change, salary change computed correctly
    - **Property 15: Trade Lopsided Detection** — flag when |net_surplus| > 20% of total trade value
    - **Validates: Requirements 6.1, 6.2, 6.3, 6.9**

- [ ] 9. Build waiver recommender
  - [ ] 9.1 Implement R/analysis/waiver_recommender.R
    - Implement `recommend_faab(my_team, valuations, rosters, remaining_faab, weeks_remaining)`
    - Rank free agents by surplus value, return top 10
    - Identify drop candidates (lowest surplus on roster)
    - Implement `suggest_bid(player_surplus, remaining_budget, weeks_remaining, competition_factor)`
    - Enforce $1 minimum bid, bid ≤ remaining budget
    - Filter by position eligibility, roster constraints (27-man), and $300 salary cap
    - Weight recent 30-day performance alongside ROS projections
    - Flag players approaching 130 AB or 50 IP minor league promotion threshold
    - Return empty list if no beneficial moves, or if non-contender during playoffs
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8_

  - [ ] 9.2 Write property tests for waiver recommender (Properties 16, 17, 18, 21)
    - **Property 16: Waiver Recommendations Sorted by Surplus** — targets descending, drops ascending
    - **Property 17: FAAB Bid Bounds and Monotonicity** — bid ≥ $1 and ≤ budget; higher surplus → higher bid
    - **Property 18: Recommendation Constraint Compliance** — position eligibility + salary cap + playoff lock
    - **Property 21: Minor League Promotion Threshold Flagging** — flag at AB≥110 or IP≥40
    - **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.6, 7.8**

- [ ] 10. Build roster optimizer
  - [ ] 10.1 Implement R/analysis/roster_optimizer.R
    - Implement `optimize_lineup(my_team, rosters, projections, injuries, matchup_context)`
    - Fill exactly 16 active slots (C, 1B, 2B, 3B, SS, 3×OF, U, 5×SP, 2×RP)
    - Assign each player to at most one slot; only eligible positions
    - Exclude injured (IL/DTD) players
    - Implement `optimize_pitchers(available_pitchers, projections, schedule)`
    - Prefer 2-start pitchers over 1-start (when quality within 80%)
    - Suggest replacement from reserve for mid-week injuries
    - Notify if no eligible replacement available for a slot
    - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 8.6_

  - [ ] 10.2 Write property tests for roster optimizer (Properties 19, 20)
    - **Property 19: Lineup Validity** — exactly 16 slots, one player per slot, position-eligible, no injured
    - **Property 20: Two-Start Pitcher Preference** — prefer 2-start SP when quality within 80%
    - **Validates: Requirements 8.1, 8.2, 8.5**

- [ ] 11. Checkpoint — Decision modules complete
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 12. Build serialization and reporting layer
  - [ ] 12.1 Implement serialization utilities and RDS/CSV export
    - Implement helper for saving RDS with metadata attributes (source, timestamp, league_id)
    - Implement CSV export with UTF-8 encoding and comma delimiters
    - Handle file system errors: log error, retain in-memory results
    - Handle corrupted RDS: warn user, suggest re-run ingest
    - _Requirements: 9.1, 9.3, 9.4_

  - [ ] 12.2 Write property tests for serialization (Properties 1, 2)
    - **Property 1: RDS Serialization Round-Trip** — saveRDS/readRDS produces identical object including attributes
    - **Property 2: CSV Export Round-Trip** — UTF-8 characters (accented names) preserved through write/read
    - **Validates: Requirements 1.5, 9.1, 9.3**

  - [ ] 12.3 Create R Markdown reports
    - Create R/reports/weekly_summary.Rmd — lineup, FAAB targets, trade opportunities, keeper watchlist
    - Create R/reports/trade_report.Rmd — trade analysis output with all dimensions
    - Create R/reports/valuation_report.Rmd — player valuations, rankings, surplus
    - All reports render to HTML with timestamp, analysis type, and recommendations
    - _Requirements: 9.2, 9.5_

- [ ] 13. Wire everything together and set up test infrastructure
  - [ ] 13.1 Set up {testthat} test infrastructure and fixtures
    - Create tests/testthat.R test runner configuration
    - Create tests/testthat/ directory structure matching design
    - Create tests/fixtures/ with sample HTML files for parsing tests
    - Configure {quickcheck} for property-based tests (minimum 100 iterations)
    - _Requirements: all (testing infrastructure)_

  - [ ] 13.2 Write property test for constitution validation (Property 3)
    - **Property 3: Constitution Validation Completeness**
    - Valid constitution → returns TRUE; missing any required field → returns FALSE with field name
    - **Validates: Requirements 2.10**

  - [ ] 13.3 Wire analysis pipeline end-to-end
    - Create a master pipeline script that runs ingest → project → value → decide in sequence
    - Ensure all modules consume/produce RDS files correctly
    - Verify inter-module data contracts (column names, types match expected schemas)
    - _Requirements: all (integration)_

- [ ] 14. Final checkpoint — All modules integrated
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate the 21 universal correctness properties from the design
- Unit tests validate specific examples and edge cases
- The existing ingest scripts (R/ingest/) are the foundation — Phase 1 enhances rather than rewrites them
- All modules depend on league_constitution.rds which already exists in data/
- The {quickcheck} package is used for property-based testing with minimum 100 iterations per property
- R Markdown reports are the primary output format for human consumption

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1", "1.2", "1.3", "1.4"] },
    { "id": 1, "tasks": ["2.1", "2.3", "3.1", "3.3"] },
    { "id": 2, "tasks": ["2.2", "2.4", "3.2"] },
    { "id": 3, "tasks": ["5.1", "13.1"] },
    { "id": 4, "tasks": ["5.2"] },
    { "id": 5, "tasks": ["5.3", "6.1"] },
    { "id": 6, "tasks": ["6.2", "8.1"] },
    { "id": 7, "tasks": ["8.2", "9.1", "10.1"] },
    { "id": 8, "tasks": ["8.3", "9.2", "10.2"] },
    { "id": 9, "tasks": ["12.1"] },
    { "id": 10, "tasks": ["12.2", "12.3", "13.2"] },
    { "id": 11, "tasks": ["13.3"] }
  ]
}
```

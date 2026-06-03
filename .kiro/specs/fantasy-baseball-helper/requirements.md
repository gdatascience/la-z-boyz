# Requirements Document

## Introduction

The Fantasy Baseball Decision Helper is an R-based analytics platform for the "La-Z-Boyz of Summer" CBS Sports fantasy baseball league. It ingests league-specific data (rosters, salaries, scoring rules, constitution) and public baseball performance data to power valuation models that provide actionable decision support for trades, add/drops, keeper decisions, and roster optimization.

The league runs on CBS Sports with 16 teams across 4 divisions, using H2H Points scoring with a salary cap system. Authentication to the CBS API is unavailable for roster/team data, so the platform uses a hybrid approach: unauthenticated CBS API calls for player lists and rules, browser-saved HTML parsing for roster and standings data, and the {baseballr} package for real MLB performance stats.

## Glossary

- **Helper**: The Fantasy Baseball Decision Helper application built in R
- **CBS_League_Connector**: The module responsible for ingesting data from the CBS Sports fantasy league via API (unauthenticated endpoints) and HTML parsing (authenticated pages saved from browser)
- **Player_Valuation_Engine**: The valuation pipeline that computes player worth based on projected performance, league-specific H2H Points scoring weights, and salary/keeper cost context
- **Trade_Analyzer**: The module that evaluates proposed trades for fairness, strategic value, salary cap impact, and keeper implications
- **Waiver_Recommender**: The module that identifies high-value players available via FAAB bidding and recommends bid amounts
- **Roster_Optimizer**: The module that recommends optimal lineup and roster construction decisions for weekly H2H matchups
- **Stats_Pipeline**: The module that retrieves and processes public baseball statistics from Baseball Reference via {baseballr}, with Statcast pitch-level data and MLB draft/prospect data as supplemental sources
- **Draft_Data**: MLB amateur draft history and prospect rankings from MLB.com, used to assess minor league player pedigree and project MLB arrival timelines
- **League_Context**: The collection of league-specific configuration parsed from the constitution and CBS API, including H2H Points scoring weights, roster slots, salary caps ($260 auction / $300 in-season / $80 keeper), and keeper escalation rules
- **Owner**: A participant in the La-Z-Boyz of Summer fantasy baseball league (one of 16 teams)
- **Constitution**: The league's governing document defining all rules, parsed once from HTML and stored as a committed RDS file
- **FAAB**: Free Agent Acquisition Budget ($250), used for waiver claims processed Tuesday/Friday/Sunday at 11 PM ET
- **Keeper Escalation**: The +$4/year salary increase applied to each kept player annually
- **Minor League Track**: The salary progression $0 (minors) → $1* → $2* → $3* → then enters +$4/year keeper escalation

## Requirements

### Requirement 1: CBS League Data Ingestion

**User Story:** As an owner, I want the helper to ingest my league's data from CBS Sports using the available access methods, so that it has accurate information about rosters, salaries, rules, and available players.

#### Acceptance Criteria

1. WHEN a player database refresh is requested, THE CBS_League_Connector SHALL fetch the full player list from the unauthenticated CBS API endpoint (`/api/players/list?response_format=JSON`), parsing all ~8,400 players with their CBS ID, name, position, eligible positions, MLB team, pro status, age, handedness, and injury status
2. WHEN a league rules refresh is requested, THE CBS_League_Connector SHALL fetch league rules from the unauthenticated CBS API endpoint (`/api/league/rules`) as XML, parsing scoring system type, roster position requirements, transaction policies, FAAB budget, trade deadline, and eligibility rules
3. WHEN roster data is needed, THE CBS_League_Connector SHALL parse a browser-saved HTML file (`data/imports/rosters.html` from the "All Teams" page) using {rvest}, extracting for each rostered player: team name, player type (Batter/Pitcher), roster position, player name, eligible positions, MLB team, CBS ID, salary, minor league contract flag (asterisk), and total fantasy points
4. WHEN standings data is needed, THE CBS_League_Connector SHALL parse a browser-saved HTML file (`data/imports/standings.html`) using {rvest}, extracting for each team: division, wins, losses, ties, winning percentage, games back, streak, division record, magic number, total points, points behind leader, and points against
5. THE CBS_League_Connector SHALL store all parsed data as RDS files in `data/cache/` with metadata attributes recording the source file path, parse timestamp, and league ID
6. IF an API endpoint returns an error or times out after 60 seconds, THEN THE CBS_League_Connector SHALL log the error with timestamp and use previously cached RDS data if available
7. IF a required HTML import file is missing from `data/imports/`, THEN THE CBS_League_Connector SHALL display instructions to the owner specifying the exact CBS URL to save and the expected file path
8. WHEN roster data is parsed, THE CBS_League_Connector SHALL identify all 16 teams and validate that the total player count is reasonable (between 350-450 rostered players across all teams)

### Requirement 2: League Constitution & Context

**User Story:** As an owner, I want the league constitution parsed into structured data, so that all helper recommendations respect our league's specific rules for scoring, salary caps, keepers, and drafts.

#### Acceptance Criteria

1. THE Helper SHALL parse the league constitution from a one-time browser-saved HTML file (`data/imports/constitution.html`) into a structured R list and store it as `data/league_constitution.rds`, committed to version control since the constitution rarely changes
2. THE League_Context SHALL encode the H2H Points scoring weights exactly as defined in the constitution: batting (1B=1, 2B=2, 3B=3, HR=4, grand slam bonus=2, cycle=5, R=1, RBI=1, BB=1, HBP=1, SB=2, CS=-1, K=-0.5) and pitching (IP=3, K=0.5, W=5, L=-5, SV=7, HLD=5, QS=3, CG=2, NH=5, PG=5, H=-1, ER=-1, BB=-1, IBB=+1)
3. THE League_Context SHALL encode salary cap rules: $260 auction budget, $300 in-season salary cap, $80 keeper salary cap, +$4/year keeper escalation, and the minor league salary track ($0 → $1* → $2* → $3* → then +$4/year)
4. THE League_Context SHALL encode roster construction: 16 active (C, 1B, 2B, 3B, SS, 3×OF, U, 5×SP, 2×RP), 6 reserve, 5 minor league slots, 27 total per team
5. THE League_Context SHALL encode keeper rules: unlimited keepers (total salary ≤ $80), keeper deadline March 17 annually, players acquired after trade deadline (July 31) via FAAB are NOT keeper-eligible, free agent keeper salary = current salary + $4
6. THE League_Context SHALL encode minor league rules: 5 roster spots, players MUST be promoted (or dropped) within 5 days of reaching 130 AB or 50 IP, salary on promotion = $1*, cannot return to minors once promoted
7. THE League_Context SHALL encode playoff structure: 23-week regular season, 3-week playoffs (weeks 24-26), 6 qualifiers (4 division winners + 2 wildcards by highest total points), top 2 seeds get bye
8. THE League_Context SHALL encode FAAB rules: $250 budget, $1 minimum bid, no $0 bids allowed, processing Tuesday/Friday/Sunday at 11 PM ET, non-contenders locked from FAAB acquisitions during playoffs
9. THE League_Context SHALL encode draft structure: live salary cap auction ($260 budget, 22 players, $1 minimum), 5-round minor league draft (worst record first), 3-round June amateur draft (current MLB draft picks only)
10. WHEN league context is loaded, THE Helper SHALL validate that all required fields are present and display an error if the constitution RDS is missing or corrupted

### Requirement 3: Real Performance Stats Integration

**User Story:** As an owner, I want the helper to pull current baseball statistics from reliable public sources, so that player valuations and recommendations are grounded in actual performance data.

#### Acceptance Criteria

1. THE Stats_Pipeline SHALL retrieve current-season batting stats from Baseball Reference via `baseballr::bref_daily_batter()`, covering the season start through the current date, returning ~500+ qualified batters with standard stats (BA, OBP, SLG, OPS, HR, RBI, SB, BB, K, etc.)
2. THE Stats_Pipeline SHALL retrieve current-season pitching stats from Baseball Reference via `baseballr::bref_daily_pitcher()`, covering the season start through the current date, returning ~600+ pitchers with standard stats (ERA, WHIP, K, BB, IP, W, L, SV, etc.)
3. WHEN detailed pitch-level data is needed for player analysis, THE Stats_Pipeline SHALL retrieve Statcast data via `baseballr::statcast_search_batters()` or `baseballr::statcast_search_pitchers()` for specific date ranges
4. THE Stats_Pipeline SHALL use `baseballr::playerid_lookup()` as the crosswalk between player name/identifiers and MLBAM IDs, enabling linkage between CBS player data and Baseball Reference/Statcast stats
5. THE Stats_Pipeline SHALL match CBS roster players to Baseball Reference stats using a combination of player name, MLB team, and position, with fuzzy matching for name discrepancies
6. IF Baseball Reference returns an error or is rate-limited, THEN THE Stats_Pipeline SHALL use cached RDS data and log a warning with the cache age
7. IF FanGraphs endpoints return 403 errors, THE Stats_Pipeline SHALL fall back to Baseball Reference as the primary stats source without failing
8. THE Stats_Pipeline SHALL store fetched stats as season-specific RDS files (`data/cache/stats_batting_{year}.rds`, `data/cache/stats_pitching_{year}.rds`) with fetch timestamp metadata
9. THE Stats_Pipeline SHALL support historical data retrieval for at least 3 prior seasons to enable trend analysis and projection inputs
10. THE Stats_Pipeline SHALL retrieve MLB amateur draft results via `baseballr::mlb_draft(year)` for at least the 3 most recent draft years, storing pick number, round, team, school, signing bonus, and scouting report for each drafted player
11. THE Stats_Pipeline SHALL retrieve prospect rankings via `baseballr::mlb_draft_prospects(year)`, providing scouting reports and rankings for pre-draft and current minor leaguers
12. THE Stats_Pipeline SHALL retrieve player biographical data via `baseballr::mlb_people(person_ids)` for any minor league player on a fantasy roster, returning age, position, birth date, handedness, and current organization
13. WHEN a minor league player is being evaluated for trade or keeper decisions, THE Stats_Pipeline SHALL compile a prospect profile including: MLB draft position, signing bonus, age, current organization, and available scouting report text
14. WHEN FanGraphs MiLB endpoints become accessible, THE Stats_Pipeline SHALL retrieve minor league game logs via `baseballr::fg_milb_batter_game_logs()` or `fg_milb_pitcher_game_logs()` for prospect performance analysis

### Requirement 4: Player Projection Model

**User Story:** As an owner, I want data-driven player projections, so that I can make decisions based on expected future performance rather than past stats alone.

#### Acceptance Criteria

1. THE Projection_Model SHALL generate rest-of-season projections for all MLB players on active rosters in each statistical category relevant to the league's H2H Points scoring (batting: 1B, 2B, 3B, HR, R, RBI, BB, HBP, SB, CS, K; pitching: IP, K, W, L, SV, HLD, QS, CG, H, ER, BB)
2. THE Projection_Model SHALL incorporate at least three seasons of historical data when generating projections, weighting recent seasons more heavily
3. IF a player has fewer than three seasons of historical data, THEN THE Projection_Model SHALL generate projections using available data and indicate reduced confidence
4. THE Projection_Model SHALL account for player age curves, injury history, and playing time trends as regression factors
5. WHEN Statcast pitch-level data is available, THE Projection_Model SHALL incorporate batted ball metrics (exit velocity, launch angle, barrel rate) and pitch characteristics (spin rate, movement) as quality-of-contact indicators
6. THE Projection_Model SHALL convert projected counting stats into projected H2H Points per week using the league's exact scoring weights, enabling direct comparison between players
7. THE Projection_Model SHALL provide confidence intervals for projected stats to distinguish high-floor players from high-variance ones
8. WHEN new stats data is ingested, THE Projection_Model SHALL update projections to reflect the latest performance trends
9. THE Projection_Model SHALL be implemented using R statistical modeling packages (e.g., {tidymodels}, {mgcv}, or equivalent)

### Requirement 5: League-Specific Player Valuation

**User Story:** As an owner, I want player values computed according to my league's H2H Points scoring and salary structure, so that I understand each player's worth in my specific context.

#### Acceptance Criteria

1. THE Player_Valuation_Engine SHALL compute a fantasy points per week projection for each player by applying the league's exact H2H Points scoring weights to projected stats
2. THE Player_Valuation_Engine SHALL compute a dollar value for each player by distributing the league's total salary pool across all players proportional to their projected points above replacement level
3. THE Player_Valuation_Engine SHALL define replacement level at each position based on the league's roster construction (C=1, 1B=1, 2B=1, 3B=1, SS=1, OF=3, U=1, SP=5, RP=2 per team × 16 teams)
4. THE Player_Valuation_Engine SHALL adjust values by positional scarcity such that thin positions (C, SS) receive premium over deep positions (OF, SP)
5. WHEN salary information is available from roster parsing, THE Player_Valuation_Engine SHALL compute surplus value as (projected dollar value − current salary), accounting for the $300 in-season cap
6. THE Player_Valuation_Engine SHALL compute keeper value by projecting multi-year surplus: (projected value year N) − (current salary + $4×N) for years 1-5, discounting future years
7. THE Player_Valuation_Engine SHALL flag players on minor league contracts with their salary track position and projected value upon promotion
8. IF projections are unavailable for a player, THEN THE Player_Valuation_Engine SHALL exclude that player from valuation rankings and indicate projections are pending

### Requirement 6: Trade Analysis

**User Story:** As an owner, I want to evaluate proposed trades for fairness, strategic fit, salary implications, and keeper value, so that I can make informed trade decisions.

#### Acceptance Criteria

1. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL compute the net surplus value difference between the players exchanged using current dollar valuations from the Player_Valuation_Engine
2. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL compute projected H2H Points per week change for the owner's team, broken down by batting and pitching contributions
3. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL compute the salary cap impact: net salary change, resulting total salary vs. the $300 in-season cap, and whether the trade keeps the owner compliant
4. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL compute keeper implications: for each player involved, show current salary, keeper escalation path (+$4/year), projected keeper value over 3 years, and whether the player is keeper-eligible (not acquired via post-deadline FAAB)
5. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL assess positional impact: positions left unfilled, over-filled, or improved, relative to the league's roster slot requirements
6. WHEN a trade proposal is submitted, THE Trade_Analyzer SHALL incorporate standings context: the owner's current record, division standing, playoff probability, and whether the team profile suggests contending (maximize current points) vs. rebuilding (maximize keeper value)
7. WHEN a trade involves minor league players, THE Trade_Analyzer SHALL assess prospect value including: MLB draft position and signing bonus, current salary track position ($0/$1*/$2*/$3*), age, scouting report text if available, and projected keeper value over 5 years at the minor league salary track
8. WHEN a trade involves minor league players, THE Trade_Analyzer SHALL retrieve draft pedigree via `mlb_draft()` and player bio via `mlb_people()` to contextualize the prospect's ceiling (e.g., "1st round pick, $3.1M bonus" vs. "5th round college arm")
8. THE Trade_Analyzer SHALL flag trades where the net value difference exceeds 20% of total trade value as potentially lopsided
9. THE Trade_Analyzer SHALL provide a structured recommendation (accept/reject/counter) with justification covering: net value, points impact, salary impact, keeper value, and strategic fit
10. IF a trade proposal contains a player not found in current roster data, THEN THE Trade_Analyzer SHALL reject the proposal and indicate which player could not be resolved

### Requirement 7: FAAB & Waiver Recommendations

**User Story:** As an owner, I want recommendations on which free agents to target via FAAB bidding and which players to drop, so that I can continuously improve my roster within budget.

#### Acceptance Criteria

1. WHEN the owner requests waiver recommendations, THE Waiver_Recommender SHALL rank available free agents by projected surplus value (projected dollar value − likely FAAB cost) and return the top 10 candidates
2. WHEN the owner requests waiver recommendations, THE Waiver_Recommender SHALL identify drop candidates on the owner's roster ranked by lowest projected surplus value, considering salary implications of dropping (salary freed vs. value lost)
3. WHEN recommending a FAAB bid, THE Waiver_Recommender SHALL suggest a bid amount based on the player's projected surplus value, remaining FAAB budget ($250 total), time remaining in season, and competitive context
4. THE Waiver_Recommender SHALL filter recommendations by position eligibility, roster constraints (27-man roster, 16 active, 6 reserve, 5 minors), and the $300 in-season salary cap
5. THE Waiver_Recommender SHALL weight recent performance (last 30 days) alongside rest-of-season projections when prioritizing recommendations, to capture hot streaks and breakouts
6. THE Waiver_Recommender SHALL flag players approaching the 130 AB or 50 IP minor league promotion thresholds as potential targets (may be dropped by other teams)
7. IF no available free agent has higher projected surplus value than any rostered player after accounting for FAAB cost, THEN THE Waiver_Recommender SHALL return an empty recommendation list with a message indicating no beneficial moves are available
8. THE Waiver_Recommender SHALL respect the rule that non-contenders are locked from FAAB acquisitions during playoffs

### Requirement 8: Roster Optimization

**User Story:** As an owner, I want optimal lineup recommendations for each weekly H2H matchup, so that I maximize my team's scoring potential.

#### Acceptance Criteria

1. WHEN a scoring period begins (Monday), THE Roster_Optimizer SHALL recommend a starting lineup filling all 16 active slots (C, 1B, 2B, 3B, SS, 3×OF, U, 5×SP, 2×RP) with eligible players projected to maximize total H2H Points for that week
2. WHEN generating a lineup recommendation, THE Roster_Optimizer SHALL exclude any player whose status is injured (IL), day-to-day, or who has a scheduled off-day, using CBS injury data and MLB schedule information
3. THE Roster_Optimizer SHALL consider the weekly matchup context: if facing a strong opponent, maximize upside; if heavily favored, prefer high-floor players
4. IF a player's status changes mid-week to injured or day-to-day, THEN THE Roster_Optimizer SHALL suggest a replacement from reserve by selecting the eligible player with the highest projected value for the remaining days in the scoring period
5. THE Roster_Optimizer SHALL optimize pitcher selection (5 SP, 2 RP) based on weekly start schedules, considering 2-start pitchers as more valuable than 1-start pitchers (more IP = more counting stats)
6. IF a player's status changes and no eligible reserve player is available for that roster slot, THEN THE Roster_Optimizer SHALL notify the owner that no replacement is available and identify the affected roster slot

### Requirement 9: Data Serialization and Reporting

**User Story:** As an owner, I want to export analysis results and view formatted reports, so that I can review recommendations and share analysis with league-mates.

#### Acceptance Criteria

1. THE Helper SHALL serialize all analysis results to RDS format in `data/cache/`, preserving all R data types and metadata attributes, with results retrievable via `readRDS()` producing identical structures
2. THE Helper SHALL generate reports in R Markdown rendered to HTML, summarizing player valuations, trade analyses, FAAB recommendations, and roster optimization, including timestamp, analysis type, and resulting recommendations
3. THE Helper SHALL support export to CSV format using UTF-8 encoding and comma delimiters for sharing data with non-R tools or league-mates
4. IF serialization or export fails due to file system errors, THEN THE Helper SHALL log the error and notify the owner without losing in-memory analysis results
5. THE Helper SHALL generate a weekly decision summary report combining: recommended lineup, FAAB targets with suggested bids, trade opportunities, and keeper value watchlist


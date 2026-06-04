# Product Overview

La-Z-Boyz of Summer is a fantasy baseball analytics tool for a 16-team CBS H2H Points keeper league ("l-z-bs"). It ingests league data from the CBS platform, generates player projections, computes dollar valuations, and produces decision-support reports for trades, waivers, and keeper selection.

## Key Capabilities

- Player projection using weighted historical stats and aging curves
- Dollar valuation via Points Above Replacement (PAR) with positional scarcity adjustments
- Keeper value analysis with multi-year NPV surplus calculations
- Trade evaluation comparing net value exchange
- Waiver wire recommendations based on surplus value
- Roster optimization for lineup construction

## Owner Context

- My team: **Fightin' Irish**
- When I say "my roster", "my team", etc., always refer to the Fightin' Irish roster data.

## League Context

- Platform: CBS Sports Fantasy Baseball
- Format: Head-to-Head Points (custom scoring weights defined in league constitution)
- Teams: 16
- Auction/draft cap: $260 per team
- In-season salary cap: $300 per team
- Keeper cap: $80 max kept salary per team
- Keeper rules: Standard contracts escalate +$4/year
- Minor league system: Players at $0 are "Minor Leaguers" — they stay at $0 and don't count toward the active roster as long as they remain below promotion thresholds (130 AB or 50 IP). Once promoted (or threshold exceeded), they enter a rookie contract at $1* and escalate $1→$2→$3 before switching to the standard +$4/year track. Dropped minor leaguers lose their salary track.
- Roster slots: C, 1B, 2B, 3B, SS, OF×3, U, SP×5, RP×2
- Roster limits: 27 total max (22 active + 5 minor league). Active and minor league slots are independent caps — a team can't use an empty minor league slot for an active player or vice versa.

## Analysis Outputs

- Trade evaluations, waiver analyses, and keeper recommendations are saved to `analysis/` (gitignored, local only).
- Naming convention: `analysis/YYYY-MM-DD_<description>.md`

## Known Data Gaps

- **Prospect rankings**: The system has no external prospect ranking data (FanGraphs FV, MLB Pipeline Top 100, Just Baseball). Players without MLB stats (e.g., Thomas White, Zyhir Hope) get $0 valuations. Manual web research is needed to assess prospect value until a ranking ingestion module is built.
- **Limited-sample players**: Players with <50 games of current-season data (IL returnees, recent call-ups) are undervalued by the projection model. Use real-world scouting + prior-year track records to override.
- **Pitcher pts/wk disparity**: Even elite SP produce ~7-8 pts/wk vs. batters at 15-18. This is a structural feature of H2H points scoring, not a model bug — but it means pitcher-heavy trade packages look worse in raw scoring comparisons than they really are.

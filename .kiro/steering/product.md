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

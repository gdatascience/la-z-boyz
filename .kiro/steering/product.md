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
- Salary cap: $260 per team ($4,160 total pool)
- Keeper rules: Standard contracts escalate +$4/year; minor league contracts follow a $0→$1→$2→$3 track then +$4/year
- Roster slots: C, 1B, 2B, 3B, SS, OF×3, U, SP×5, RP×2

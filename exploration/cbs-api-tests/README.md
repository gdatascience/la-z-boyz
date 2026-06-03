# CBS Sports Fantasy API Exploration

These scripts probe the CBS Sports fantasy API to determine what data access is available for the "La-Z-Boyz of Summer" league (`l-z-bs`).

## Key Findings (2025-07-28)

### The API is alive

The CBS Sports fantasy API at `api.cbssports.com` is operational. The league subdomain API at `l-z-bs.baseball.cbssports.com/api/` is also functional.

### What works WITHOUT authentication

| Endpoint | Returns | Notes |
|----------|---------|-------|
| `/api/league/rules` | XML (5.6 KB) | Full league rules: roster slots, scoring, transactions, FAAB, trade policy |
| `/api/players/list` | JSON (3 MB) | Complete MLB player database with names, positions, teams, status, injury info |
| `/api/players/search` | JSON (3 MB) | Same as above |
| `api.cbssports.com/fantasy/sports` | JSON | Lists available sports (baseball, football, hockey, basketball, college football) |

### What requires authentication (returns "User not signed in")

| Endpoint | Needs |
|----------|-------|
| `/api/league/rosters` | Auth + `league_id` |
| `/api/league/teams` | Auth + `league_id` |
| `/api/league/details` | Auth + `league_id` |
| `/api/league/owners` | Auth + `league_id` |

### Authentication mechanism

The OAuth endpoint exists at `api.cbssports.com/general/oauth/access_token` and requires:
- `client_id` — an app developer key (from the now-defunct developer portal)
- `client_secret` — an app developer secret
- `request_token` — a temporary token from a prior auth step

This is an OAuth 1.0 three-legged flow. The developer portal (`developer.cbssports.com`) no longer resolves, so new app registrations are impossible.

**However**, the league site also accepts a `Bearer` token via `Authorization` header — passing an invalid token returns "Failed Authentication: error - invalid access token" rather than "Missing league_id". This suggests there may be a way to obtain a valid token through the web login flow (cookie/session-based).

### League data already retrieved

From `/api/league/rules` (no auth required):
- **Scoring**: Head-to-Head, Points-based
- **Roster**: C, 1B, 2B, 3B, SS, 3×OF, U, 5×SP, 2×RP (16 active, 6 reserve, 5 minors, 27 total)
- **Transactions**: FAAB ($250 budget), waivers run Sun/Tue/Fri at 11 PM ET
- **Trades**: Commissioner approval, deadline 7/31/2026
- **Entry fee**: $100
- **Player pool**: AL + NL

## Scripts

| Script | Purpose |
|--------|---------|
| `test_cbs_api.R` | Initial probe of API endpoints and league homepage |
| `test_cbs_auth.R` | First auth endpoint discovery |
| `test_cbs_auth2.R` | OAuth endpoint discovery (found `/general/oauth/access_token`) |
| `test_cbs_oauth.R` | OAuth parameter testing (discovered required fields) |
| `test_cbs_league_api.R` | League subdomain API endpoint mapping |
| `test_cbs_data.R` | Successful data retrieval from public endpoints |

## Next Steps

1. **Try session-based auth**: Log in via browser, capture cookies/tokens, and test if those work with the API endpoints
2. **Explore the web login flow**: The login redirects to `cbssports.com/login?product_abbrev=mgmt&master_product=41271` — intercepting the session cookie after login may unlock the roster/team endpoints
3. **If API auth fails entirely**: Build a structured CSV ingestion system using the publicly available player list + manual roster/salary data

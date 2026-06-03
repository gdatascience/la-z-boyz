#!/usr/bin/env Rscript
# Test script to probe CBS Sports Fantasy API endpoints
# Goal: Determine if the deprecated API is still responding

library(httr2)
library(jsonlite)

cat("=== CBS Sports Fantasy API Probe ===\n\n")

league_id <- "l-z-bs"

# --- Test 1: Check if api.cbssports.com resolves and responds ---
cat("Test 1: Probing api.cbssports.com base endpoint...\n")
tryCatch({
  resp <- request("http://api.cbssports.com/fantasy/sports") |>
    req_url_query(response_format = "JSON") |>
    req_timeout(15) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  cat("  Content-Type:", resp_content_type(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response (first 500 chars):\n")
  cat("  ", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 2: Try the league details endpoint (may need auth) ---
cat("Test 2: Probing league details endpoint...\n")
tryCatch({
  resp <- request(paste0("http://api.cbssports.com/fantasy/league/details")) |>
    req_url_query(
      response_format = "JSON",
      league_id = paste0(league_id, ".baseball")
    ) |>
    req_timeout(15) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response (first 500 chars):\n")
  cat("  ", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 3: Try alternate URL patterns ---
cat("Test 3: Probing fantasy-api.cbssports.com...\n")
tryCatch({
  resp <- request("https://fantasy-api.cbssports.com/api/league/details") |>
    req_url_query(
      response_format = "JSON",
      league_id = paste0(league_id, ".baseball")
    ) |>
    req_timeout(15) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response (first 500 chars):\n")
  cat("  ", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 4: Try the format used in the old token fetcher gem ---
# The gem used: POST to https://auth.cbssports.com/token/get
cat("Test 4: Probing auth.cbssports.com...\n")
tryCatch({
  resp <- request("https://auth.cbssports.com/token/get") |>
    req_method("POST") |>
    req_body_form(
      response_format = "JSON",
      dummy = "test"
    ) |>
    req_timeout(15) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response (first 500 chars):\n")
  cat("  ", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 5: Try fetching public league page to understand structure ---
cat("Test 5: Probing league homepage for data structure...\n")
tryCatch({
  resp <- request(paste0("https://", league_id, ".baseball.cbssports.com/")) |>
    req_timeout(15) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  cat("  Content-Type:", resp_content_type(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Page size:", nchar(body), "chars\n")
  # Look for API-related patterns in the HTML
  api_refs <- regmatches(body, gregexpr("api[^\"'\\s]{5,50}", body))[[1]]
  if (length(api_refs) > 0) {
    cat("  Found API references in page:\n")
    cat("  ", paste(unique(head(api_refs, 10)), collapse = "\n   "), "\n")
  }
  cat("\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 6: Check if there's a JSON endpoint on the league site ---
cat("Test 6: Checking for JSON data endpoints on league site...\n")
endpoints_to_try <- c(

  paste0("https://", league_id, ".baseball.cbssports.com/api/league/rosters"),
  paste0("https://", league_id, ".baseball.cbssports.com/rosters?print=json"),
  paste0("http://api.cbssports.com/fantasy/league/rosters?response_format=JSON&league_id=", league_id, ".baseball")
)

for (url in endpoints_to_try) {
  cat("  Trying:", url, "\n")
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()
    cat("    Status:", resp_status(resp), "\n")
    body <- resp_body_string(resp)
    cat("    Response (first 200 chars):", substr(body, 1, 200), "\n")
  }, error = function(e) {
    cat("    ERROR:", conditionMessage(e), "\n")
  })
  cat("\n")
}

cat("\n=== Probe Complete ===\n")
cat("If any endpoints returned JSON with league data, the API is alive.\n")
cat("If all returned errors/redirects, we'll need to explore scraping or CSV approaches.\n")

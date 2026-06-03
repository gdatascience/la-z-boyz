#!/usr/bin/env Rscript
# Test CBS Sports Fantasy API authentication
# The base API is alive (api.cbssports.com returns JSON)
# Now we need to find the auth mechanism

library(httr2)
library(jsonlite)

cat("=== CBS Sports Fantasy API Auth Testing ===\n\n")

league_id <- "l-z-bs"
sport <- "baseball"

# --- Test 1: Try the login endpoint pattern from the old API docs ---
cat("Test 1: Trying api.cbssports.com login endpoint...\n")
tryCatch({
  resp <- request("http://api.cbssports.com/fantasy/login") |>
    req_url_query(response_format = "JSON") |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response (first 500 chars):\n")
  cat("  ", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 2: Check what endpoints are available on the API ---
cat("Test 2: Trying to list available API methods...\n")
api_paths <- c(
  "/fantasy/league/teams",
  "/fantasy/league/rules",
  "/fantasy/league/scoring",
  "/fantasy/league/free-agents",
  "/fantasy/league/transactions",
  "/fantasy/players/list",
  "/fantasy/league/owners"
)

for (path in api_paths) {
  url <- paste0("http://api.cbssports.com", path)
  tryCatch({
    resp <- request(url) |>
      req_url_query(
        response_format = "JSON",
        league_id = paste0(league_id, ".", sport)
      ) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    # Parse the JSON to check for auth error messages
    if (grepl("\\{", body)) {
      parsed <- tryCatch(fromJSON(body), error = function(e) NULL)
      msg <- if (!is.null(parsed$statusMessage)) parsed$statusMessage else "no message"
      cat(sprintf("  %-35s -> %d (%s)\n", path, status, msg))
      if (status != 200 && !is.null(parsed$statusMessage)) {
        cat("    Detail:", substr(body, 1, 200), "\n")
      }
    } else {
      cat(sprintf("  %-35s -> %d (non-JSON)\n", path, status))
    }
  }, error = function(e) {
    cat(sprintf("  %-35s -> CONN ERROR: %s\n", path, conditionMessage(e)))
  })
}

cat("\n")

# --- Test 3: Try without league_id to see if endpoints exist ---
cat("Test 3: Trying endpoints without league_id...\n")
for (path in api_paths[1:3]) {
  url <- paste0("http://api.cbssports.com", path)
  tryCatch({
    resp <- request(url) |>
      req_url_query(response_format = "JSON") |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    if (grepl("\\{", body)) {
      parsed <- tryCatch(fromJSON(body), error = function(e) NULL)
      msg <- if (!is.null(parsed$statusMessage)) parsed$statusMessage else ""
      cat(sprintf("  %-35s -> %d (%s)\n", path, status, msg))
      if (!is.null(parsed)) {
        cat("    ", substr(body, 1, 300), "\n")
      }
    } else {
      cat(sprintf("  %-35s -> %d\n", path, status))
    }
  }, error = function(e) {
    cat(sprintf("  %-35s -> ERROR: %s\n", path, conditionMessage(e)))
  })
}

cat("\n")

# --- Test 4: Try access_token parameter pattern ---
cat("Test 4: Testing access_token parameter pattern (with dummy token)...\n")
tryCatch({
  resp <- request("http://api.cbssports.com/fantasy/league/teams") |>
    req_url_query(
      response_format = "JSON",
      league_id = paste0(league_id, ".", sport),
      access_token = "test_invalid_token"
    ) |>
    req_timeout(10) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Response:", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 5: Check the league site for XHR/API calls in source ---
cat("Test 5: Checking league page source for internal API patterns...\n")
tryCatch({
  resp <- request(paste0("https://", league_id, ".baseball.cbssports.com/")) |>
    req_timeout(15) |>
    req_perform()
  body <- resp_body_string(resp)
  
  # Look for fetch/XHR patterns, API URLs, or data objects
  patterns <- c(
    "api\\.cbssports",
    "fantasy-api",
    "accessToken",
    "access_token",
    "leagueId",
    "league_id",
    "/api/",
    "fetch\\(",
    "__NEXT_DATA__"
  )
  
  for (pat in patterns) {
    matches <- regmatches(body, gregexpr(paste0(".{0,40}", pat, ".{0,40}"), body))[[1]]
    if (length(matches) > 0) {
      cat(sprintf("  Pattern '%s' found (%d matches):\n", pat, length(matches)))
      for (m in head(matches, 3)) {
        cat("    ...", gsub("\\s+", " ", m), "...\n")
      }
    }
  }
  
  # Check for __NEXT_DATA__ (Next.js apps embed data in page)
  if (grepl("__NEXT_DATA__", body)) {
    cat("\n  Found __NEXT_DATA__ - extracting...\n")
    next_data <- regmatches(body, regexpr('__NEXT_DATA__.*?</script>', body))
    if (length(next_data) > 0) {
      # Extract JSON from the script tag
      json_str <- sub('.*__NEXT_DATA__"\\s*type="application/json">', '', next_data)
      json_str <- sub('</script>.*', '', json_str)
      cat("  __NEXT_DATA__ size:", nchar(json_str), "chars\n")
      cat("  First 300 chars:", substr(json_str, 1, 300), "\n")
    }
  }
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

cat("\n=== Auth Testing Complete ===\n")

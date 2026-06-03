#!/usr/bin/env Rscript
# CBS API is ALIVE! Key endpoints found:
# - /api/league/rules -> 200 (no auth, XML)
# - /api/players/list -> 200 (no auth, 4MB XML)
# - /api/league/rosters -> needs league_id param
# - /api/league/teams -> needs league_id param
# - /api/league/details -> needs league_id param
# - /api/league/owners -> needs league_id param

library(httr2)
library(xml2)

cat("=== CBS Fantasy API - Data Retrieval Test ===\n\n")

league_id <- "l-z-bs"
base <- paste0("https://", league_id, ".baseball.cbssports.com")

# --- Test 1: Get league rules (already confirmed working) ---
cat("Test 1: Fetching league rules...\n")
tryCatch({
  resp <- request(paste0(base, "/api/league/rules")) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Size:", nchar(body), "bytes\n")
  
  # Parse XML
  doc <- read_xml(body)
  cat("  Root element:", xml_name(doc), "\n")
  
  # Show structure
  children <- xml_children(doc)
  cat("  Top-level elements:\n")
  for (child in children[1:min(length(children), 20)]) {
    cat("    <", xml_name(child), ">", substr(xml_text(child), 1, 60), "\n")
  }
  cat("\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 2: Try league endpoints with league_id parameter ---
cat("Test 2: League endpoints with league_id parameter...\n\n")

endpoints <- c("rosters", "teams", "details", "owners")

for (ep in endpoints) {
  url <- paste0(base, "/api/league/", ep)
  
  # Try with league_id as query parameter
  for (lid in c(league_id, paste0(league_id, ".baseball"), "l-z-bs")) {
    tryCatch({
      resp <- request(url) |>
        req_url_query(
          league_id = lid,
          response_format = "JSON"
        ) |>
        req_timeout(15) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()
      
      status <- resp_status(resp)
      body <- resp_body_string(resp)
      ctype <- resp_content_type(resp)
      
      if (status == 200) {
        cat(sprintf("  SUCCESS! %s (league_id=%s) -> %d (%s, %d bytes)\n", 
                    ep, lid, status, ctype, nchar(body)))
        cat("    First 500 chars:", substr(body, 1, 500), "\n\n")
      } else if (nchar(body) < 200) {
        cat(sprintf("  %s (league_id=%s) -> %d: %s\n", ep, lid, status, body))
      } else {
        cat(sprintf("  %s (league_id=%s) -> %d (%d bytes)\n", ep, lid, status, nchar(body)))
      }
    }, error = function(e) {
      cat(sprintf("  %s (league_id=%s) -> ERROR: %s\n", ep, lid, conditionMessage(e)))
    })
  }
  cat("\n")
}

# --- Test 3: Try without response_format (default XML) ---
cat("Test 3: Try endpoints with league_id in XML format...\n\n")

for (ep in endpoints) {
  url <- paste0(base, "/api/league/", ep)
  tryCatch({
    resp <- request(url) |>
      req_url_query(league_id = league_id) |>
      req_timeout(15) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    ctype <- resp_content_type(resp)
    cat(sprintf("  /api/league/%s?league_id=%s -> %d (%s, %d bytes)\n",
                ep, league_id, status, ctype, nchar(body)))
    if (status == 200 && nchar(body) < 1000) {
      cat("    Body:", substr(body, 1, 500), "\n")
    } else if (status == 200) {
      cat("    Body (first 500):", substr(body, 1, 500), "\n")
    } else if (nchar(body) < 200) {
      cat("    Body:", body, "\n")
    }
  }, error = function(e) {
    cat(sprintf("  /api/league/%s -> ERROR: %s\n", ep, conditionMessage(e)))
  })
  cat("\n")
}

# --- Test 4: Sample the players list ---
cat("Test 4: Sample from /api/players/list (first 2000 chars)...\n")
tryCatch({
  resp <- request(paste0(base, "/api/players/list")) |>
    req_url_query(response_format = "JSON") |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  status <- resp_status(resp)
  body <- resp_body_string(resp)
  cat("  Status:", status, "\n")
  cat("  Size:", nchar(body), "bytes\n")
  cat("  First 2000 chars:\n")
  cat("  ", substr(body, 1, 2000), "\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
})

cat("\n=== Data Retrieval Test Complete ===\n")

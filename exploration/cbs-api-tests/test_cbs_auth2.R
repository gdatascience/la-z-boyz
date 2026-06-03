#!/usr/bin/env Rscript
# Test CBS Sports Fantasy API - finding the auth endpoint
# We know: api.cbssports.com is alive and requires access_token
# Need to find: where to get the access_token

library(httr2)
library(jsonlite)

cat("=== CBS Fantasy API - Auth Endpoint Discovery ===\n\n")

league_id <- "l-z-bs"
sport <- "baseball"

# --- Test 1: Try various auth endpoint patterns ---
cat("Test 1: Trying known auth endpoint patterns...\n\n")

auth_urls <- c(
  "https://www.cbssports.com/api/auth/login",
  "https://api.cbssports.com/fantasy/login",
  "https://api.cbssports.com/general/oauth/access-token",
  "https://api.cbssports.com/general/oauth/access_token",
  "http://api.cbssports.com/general/access-token",
  "http://api.cbssports.com/general/access_token/create",
  "http://api.cbssports.com/fantasy/access-token"
)

for (url in auth_urls) {
  tryCatch({
    resp <- request(url) |>
      req_method("POST") |>
      req_body_form(
        response_format = "JSON",
        league_id = paste0(league_id, ".", sport)
      ) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  %-55s -> %d\n", url, status))
    if (nchar(body) > 0 && nchar(body) < 500) {
      cat("    Body:", body, "\n")
    }
  }, error = function(e) {
    cat(sprintf("  %-55s -> ERROR: %s\n", url, conditionMessage(e)))
  })
  cat("\n")
}

# --- Test 2: Check the login page for form action / API endpoint ---
cat("\nTest 2: Examining login page structure...\n")
tryCatch({
  resp <- request("https://www.cbssports.com/user/login") |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  body <- resp_body_string(resp)
  
  # Look for login form actions, auth URLs
  patterns <- c("action=\"[^\"]+\"", "auth[^\"'\\s]+", "login[^\"'\\s]+api",
                "token[^\"'\\s]+", "oauth[^\"'\\s]+")
  for (pat in patterns) {
    matches <- regmatches(body, gregexpr(pat, body, ignore.case = TRUE))[[1]]
    if (length(matches) > 0) {
      cat(sprintf("  Pattern '%s':\n", pat))
      for (m in head(unique(matches), 5)) {
        cat("    ", m, "\n")
      }
    }
  }
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
})

# --- Test 3: Check commissioner league page login ---
cat("\nTest 3: Checking commissioner league login...\n")
tryCatch({
  resp <- request(paste0("https://", league_id, ".baseball.cbssports.com/login")) |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  cat("  Final URL:", resp_url(resp) %||% "unknown", "\n")
  if (resp_status(resp) == 200) {
    body <- resp_body_string(resp)
    cat("  Page size:", nchar(body), "\n")
    # Look for form actions
    forms <- regmatches(body, gregexpr('<form[^>]*action="[^"]*"[^>]*>', body))[[1]]
    if (length(forms) > 0) {
      cat("  Forms found:\n")
      for (f in head(forms, 5)) cat("    ", f, "\n")
    }
  }
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
})

# --- Test 4: Try the pattern from the Ruby gem (adjusted) ---
# Original gem used POST to auth.cbssports.com/token/get with:
# userid, password, league_abbreviation, product_abbreviation, response_format
cat("\nTest 4: Trying adapted gem auth patterns...\n")

adapted_urls <- c(
  "http://api.cbssports.com/fantasy/token/get",
  "http://api.cbssports.com/token/get",
  "http://api.cbssports.com/auth/token",
  "http://api.cbssports.com/fantasy/auth/access-token"
)

for (url in adapted_urls) {
  tryCatch({
    resp <- request(url) |>
      req_method("POST") |>
      req_body_form(
        response_format = "JSON",
        league_abbreviation = league_id,
        product_abbreviation = sport
      ) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  %-55s -> %d\n", url, status))
    if (nchar(body) > 0 && nchar(body) < 500) {
      cat("    Body:", substr(body, 1, 300), "\n")
    }
  }, error = function(e) {
    cat(sprintf("  %-55s -> ERROR: %s\n", url, conditionMessage(e)))
  })
  cat("\n")
}

# --- Test 5: Test if league endpoints work with just league_id in URL ---
cat("\nTest 5: Try alternate league endpoint patterns...\n")
alt_patterns <- c(
  paste0("http://api.cbssports.com/fantasy/", sport, "/league/teams?response_format=JSON&league_id=", league_id),
  paste0("http://api.cbssports.com/fantasy/league/teams?response_format=JSON&ESSION_ID=test&league_id=", league_id, ".", sport),
  paste0("http://api.cbssports.com/fantasy/league/rosters?response_format=JSON&league_id=", league_id, ".", sport)
)

for (url in alt_patterns) {
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    short_url <- substr(url, 1, 80)
    cat(sprintf("  %s... -> %d\n", short_url, status))
    if (nchar(body) > 0 && nchar(body) < 500 && grepl("\\{", body)) {
      cat("    ", substr(body, 1, 300), "\n")
    }
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  })
  cat("\n")
}

cat("\n=== Auth Discovery Complete ===\n")

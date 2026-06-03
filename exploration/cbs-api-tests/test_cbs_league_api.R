#!/usr/bin/env Rscript
# Test the league subdomain API (l-z-bs.baseball.cbssports.com/api/...)
# This returned 400 Bad Request (not 404!) meaning the endpoint exists

library(httr2)
library(jsonlite)

cat("=== CBS League Subdomain API Testing ===\n\n")

league_id <- "l-z-bs"
base <- paste0("https://", league_id, ".baseball.cbssports.com")

# --- Test 1: Probe /api/* endpoints ---
cat("Test 1: Probing league subdomain /api/* endpoints...\n\n")

api_paths <- c(
  "/api/league/rosters",
  "/api/league/teams",
  "/api/league/rules",
  "/api/league/scoring",
  "/api/league/owners",
  "/api/league/free-agents",
  "/api/league/transactions",
  "/api/league/standings",
  "/api/league/details",
  "/api/league/draft-results",
  "/api/league/players",
  "/api/players/list",
  "/api/players/search",
  "/api/rosters",
  "/api/teams",
  "/api/standings"
)

for (path in api_paths) {
  url <- paste0(base, path)
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    ctype <- resp_content_type(resp)
    body <- resp_body_string(resp)
    size <- nchar(body)
    
    # Only show interesting results (not 404 with huge HTML pages)
    if (status != 404 || size < 1000) {
      cat(sprintf("  %-40s -> %d (%s, %d bytes)\n", path, status, ctype, size))
      if (size < 500 && size > 0) {
        cat("    Body:", substr(body, 1, 300), "\n")
      }
    } else {
      cat(sprintf("  %-40s -> %d (HTML page)\n", path, status))
    }
  }, error = function(e) {
    cat(sprintf("  %-40s -> ERROR: %s\n", path, conditionMessage(e)))
  })
}

cat("\n")

# --- Test 2: Try with response_format parameter ---
cat("Test 2: /api/league/rosters with response_format...\n\n")

tryCatch({
  resp <- request(paste0(base, "/api/league/rosters")) |>
    req_url_query(response_format = "JSON") |>
    req_timeout(10) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("  Status:", resp_status(resp), "\n")
  cat("  Content-Type:", resp_content_type(resp), "\n")
  body <- resp_body_string(resp)
  cat("  Body size:", nchar(body), "\n")
  cat("  Body:", substr(body, 1, 500), "\n\n")
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n\n")
})

# --- Test 3: Try with cookies from a browser session ---
# Check what the /api/league/rosters endpoint actually wants
cat("Test 3: Testing various auth headers on /api/league/rosters...\n\n")

header_combos <- list(
  list(),  # no extra headers
  list(Accept = "application/json"),
  list(Accept = "application/json", `X-Requested-With` = "XMLHttpRequest"),
  list(Accept = "application/json", Authorization = "Bearer test")
)

for (headers in header_combos) {
  tryCatch({
    req <- request(paste0(base, "/api/league/rosters")) |>
      req_url_query(response_format = "JSON") |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE)
    
    for (nm in names(headers)) {
      req <- req |> req_headers(!!nm := headers[[nm]])
    }
    
    resp <- req |> req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  Headers: %s -> %d (%d bytes)\n",
                if(length(headers) == 0) "none" else paste(names(headers), collapse=","),
                status, nchar(body)))
    if (nchar(body) < 500) cat("    ", substr(body, 1, 200), "\n")
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
  })
  cat("\n")
}

# --- Test 4: Check the old-style API with the league subdomain format ---
cat("Test 4: Trying api.cbssports.com with session cookie approach...\n\n")

# The old API used access_token parameter. But maybe we can use a web session cookie.
# Let's check what cookies the league site sets
tryCatch({
  resp <- request(paste0(base, "/")) |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  # Check response headers for Set-Cookie
  headers <- resp_headers(resp)
  cookie_headers <- headers[grepl("set-cookie", names(headers), ignore.case = TRUE)]
  if (length(cookie_headers) > 0) {
    cat("  Cookies set by league homepage:\n")
    for (c in cookie_headers) {
      cat("    ", substr(c, 1, 100), "\n")
    }
  } else {
    cat("  No cookies set by league homepage\n")
  }
  
  # Also check for any data in the page that reveals the internal API structure
  body <- resp_body_string(resp)
  
  # Look for JSON data embedded in the page
  if (grepl("window\\.__", body)) {
    cat("\n  Found window.__ data objects:\n")
    matches <- regmatches(body, gregexpr("window\\.__[A-Z_]+", body))[[1]]
    for (m in unique(matches)) cat("    ", m, "\n")
  }
  
  # Look for CBSI session patterns
  session_patterns <- regmatches(body, gregexpr("[A-Z_]*SESSION[A-Z_]*", body))[[1]]
  if (length(session_patterns) > 0) {
    cat("\n  Session-related tokens:\n")
    for (s in unique(head(session_patterns, 10))) cat("    ", s, "\n")
  }
  
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
})

# --- Test 5: Check if there's an accessible roster page we can parse ---
cat("\n\nTest 5: Checking accessible pages (may require login)...\n\n")
pages_to_check <- c(
  paste0(base, "/rosters"),
  paste0(base, "/standings"),
  paste0(base, "/players"),
  paste0(base, "/transactions")
)

for (url in pages_to_check) {
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    final_url <- resp_url(resp) %||% url
    body <- resp_body_string(resp)
    is_login_redirect <- grepl("login", final_url, ignore.case = TRUE) || 
                         grepl("sign.in|login", body, ignore.case = TRUE)
    cat(sprintf("  %-50s -> %d %s\n", 
                gsub(base, "", url),
                status,
                if(is_login_redirect) "(login required)" else "(accessible)"))
  }, error = function(e) {
    cat(sprintf("  %-50s -> ERROR\n", gsub(base, "", url)))
  })
}

cat("\n=== League API Testing Complete ===\n")

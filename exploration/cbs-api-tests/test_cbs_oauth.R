#!/usr/bin/env Rscript
# Test CBS Sports OAuth endpoint (found: /general/oauth/access_token returns 500 not 404)
# This means it exists but needs proper parameters

library(httr2)
library(jsonlite)

cat("=== CBS Fantasy API - OAuth Token Endpoint ===\n\n")

league_id <- "l-z-bs"
sport <- "baseball"

# --- Test 1: Try GET with various param combinations ---
cat("Test 1: /general/oauth/access_token with various params (GET)...\n\n")

param_combos <- list(
  list(response_format = "JSON"),
  list(response_format = "JSON", league_id = paste0(league_id, ".", sport)),
  list(response_format = "JSON", user_id = "test", password = "test"),
  list(response_format = "JSON", client_id = "test", client_secret = "test"),
  list(response_format = "JSON", grant_type = "password")
)

for (params in param_combos) {
  tryCatch({
    req <- request("http://api.cbssports.com/general/oauth/access_token") |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE)
    
    for (nm in names(params)) {
      req <- req |> req_url_query(!!nm := params[[nm]])
    }
    
    resp <- req |> req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  GET params=%s\n", paste(names(params), collapse=",")))
    cat(sprintf("    -> %d: %s\n\n", status, substr(body, 1, 300)))
  }, error = function(e) {
    cat(sprintf("  GET params=%s -> ERROR: %s\n\n", 
                paste(names(params), collapse=","), conditionMessage(e)))
  })
}

# --- Test 2: Try POST with various param combinations ---
cat("Test 2: /general/oauth/access_token with POST...\n\n")

for (params in param_combos) {
  tryCatch({
    resp <- request("http://api.cbssports.com/general/oauth/access_token") |>
      req_method("POST") |>
      req_body_form(!!!params) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  POST params=%s\n", paste(names(params), collapse=",")))
    cat(sprintf("    -> %d: %s\n\n", status, substr(body, 1, 300)))
  }, error = function(e) {
    cat(sprintf("  POST params=%s -> ERROR: %s\n\n", 
                paste(names(params), collapse=","), conditionMessage(e)))
  })
}

# --- Test 3: Try the league page's own internal API ---
cat("Test 3: Probing league site's Next.js API routes...\n\n")

nextjs_routes <- c(
  paste0("https://", league_id, ".baseball.cbssports.com/api/auth/session"),
  paste0("https://", league_id, ".baseball.cbssports.com/api/league/rosters"),
  paste0("https://", league_id, ".baseball.cbssports.com/_next/data"),
  paste0("https://www.cbssports.com/api/auth/login"),
  paste0("https://www.cbssports.com/api/user/session")
)

for (url in nextjs_routes) {
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status <- resp_status(resp)
    body <- resp_body_string(resp)
    cat(sprintf("  %s\n    -> %d (%d chars)\n", url, status, nchar(body)))
    if (status == 200 && grepl("\\{", body)) {
      cat("    JSON:", substr(body, 1, 200), "\n")
    }
  }, error = function(e) {
    cat(sprintf("  %s\n    -> ERROR: %s\n", url, conditionMessage(e)))
  })
  cat("\n")
}

# --- Test 4: Check if we can grab cookies from a login flow ---
cat("Test 4: Examining login form submission target...\n\n")
tryCatch({
  # First get the login page
  resp <- request(paste0("https://www.cbssports.com/login?product_abbrev=mgmt&xurl=https%3A%2F%2F",
                         league_id, ".baseball.cbssports.com%2Flogin&master_product=41271")) |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  body <- resp_body_string(resp)
  
  # Look for the login form's actual submission URL in JavaScript
  js_patterns <- c(
    "login.*url.*[\"'][^\"']+[\"']",
    "fetch\\([\"'][^\"']+[\"']",
    "axios\\.[a-z]+\\([\"'][^\"']+[\"']",
    "/api/[a-z]+/[a-z]+",
    "signin",
    "authenticate"
  )
  
  for (pat in js_patterns) {
    matches <- regmatches(body, gregexpr(pat, body, ignore.case = TRUE))[[1]]
    if (length(matches) > 0) {
      cat(sprintf("  Pattern '%s':\n", pat))
      for (m in head(unique(matches), 5)) {
        cat("    ", substr(m, 1, 100), "\n")
      }
      cat("\n")
    }
  }
  
  # Also look for script tags with src containing auth/login
  scripts <- regmatches(body, gregexpr('src="[^"]*"', body))[[1]]
  auth_scripts <- scripts[grepl("auth|login|user", scripts, ignore.case = TRUE)]
  if (length(auth_scripts) > 0) {
    cat("  Auth-related scripts:\n")
    for (s in head(auth_scripts, 5)) cat("    ", s, "\n")
  }
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
})

cat("\n=== OAuth Testing Complete ===\n")

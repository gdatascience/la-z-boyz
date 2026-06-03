# Quick validation of salary_rules.R logic
source("R/utils/salary_rules.R")

# --- Test check_salary_cap ---

# Test 1: Compliant in-season roster
roster <- data.frame(
  player_name = c("Player A", "Player B", "Player C"),
  salary = c(50, 100, 140)
)
result <- check_salary_cap(roster, "in_season")
stopifnot(result$compliant == TRUE)
stopifnot(result$total_salary == 290)
stopifnot(result$remaining_cap == 10)
cat("PASS: in_season compliant roster\n")

# Test 2: Non-compliant in-season roster
roster2 <- data.frame(
  player_name = c("Player A", "Player B"),
  salary = c(200, 150)
)
result2 <- check_salary_cap(roster2, "in_season")
stopifnot(result2$compliant == FALSE)
stopifnot(result2$total_salary == 350)
stopifnot(result2$remaining_cap == -50)
cat("PASS: in_season non-compliant roster\n")

# Test 3: Auction cap
roster3 <- data.frame(player_name = "P1", salary = 260)
result3 <- check_salary_cap(roster3, "auction")
stopifnot(result3$compliant == TRUE)
stopifnot(result3$total_salary == 260)
stopifnot(result3$remaining_cap == 0)
cat("PASS: auction exactly at cap\n")

# Test 4: Keeper cap
roster4 <- data.frame(player_name = c("K1", "K2"), salary = c(40, 41))
result4 <- check_salary_cap(roster4, "keeper")
stopifnot(result4$compliant == FALSE)
stopifnot(result4$total_salary == 81)
stopifnot(result4$remaining_cap == -1)
cat("PASS: keeper cap exceeded\n")

# Test 5: Invalid cap_type
tryCatch(
  check_salary_cap(roster, "invalid"),
  error = function(e) cat("PASS: invalid cap_type error caught\n")
)

# Test 6: Missing salary column
tryCatch(
  check_salary_cap(data.frame(name = "X"), "in_season"),
  error = function(e) cat("PASS: missing salary column error caught\n")
)

# --- Test compute_salary_impact ---

# Test 7: Basic transaction
incoming <- data.frame(player_name = "New", salary = 15)
outgoing <- data.frame(player_name = "Old", salary = 5)
result7 <- compute_salary_impact(incoming, outgoing, current_total = 250)
stopifnot(result7$net_change == 10)
stopifnot(result7$new_total == 260)
stopifnot(result7$cap_compliant == TRUE)
cat("PASS: basic transaction impact\n")

# Test 8: Transaction that breaks cap
incoming2 <- data.frame(player_name = "Expensive", salary = 60)
outgoing2 <- data.frame(player_name = "Cheap", salary = 5)
result8 <- compute_salary_impact(incoming2, outgoing2, current_total = 280)
stopifnot(result8$net_change == 55)
stopifnot(result8$new_total == 335)
stopifnot(result8$cap_compliant == FALSE)
cat("PASS: cap-breaking transaction\n")

# Test 9: NULL players_in
result9 <- compute_salary_impact(NULL, outgoing, current_total = 200)
stopifnot(result9$net_change == -5)
stopifnot(result9$new_total == 195)
stopifnot(result9$cap_compliant == TRUE)
cat("PASS: NULL players_in (drop only)\n")

# Test 10: NULL players_out
result10 <- compute_salary_impact(incoming, NULL, current_total = 200)
stopifnot(result10$net_change == 15)
stopifnot(result10$new_total == 215)
stopifnot(result10$cap_compliant == TRUE)
cat("PASS: NULL players_out (add only)\n")

# Test 11: Empty data frames
result11 <- compute_salary_impact(
  data.frame(player_name = character(0), salary = numeric(0)),
  data.frame(player_name = character(0), salary = numeric(0)),
  current_total = 275
)
stopifnot(result11$net_change == 0)
stopifnot(result11$new_total == 275)
stopifnot(result11$cap_compliant == TRUE)
cat("PASS: empty data frames\n")

# Test 12: Exactly at cap boundary
result12 <- compute_salary_impact(incoming, outgoing, current_total = 290)
stopifnot(result12$new_total == 300)
stopifnot(result12$cap_compliant == TRUE)
cat("PASS: exactly at $300 cap boundary\n")

cat("\nAll salary_rules.R tests passed!\n")

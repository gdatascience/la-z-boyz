# Test runner for the La-Z-Boyz Fantasy Baseball Helper
#
# This is NOT an R package — it's a collection of scripts.
# We use testthat directly by sourcing our modules via the helper file.
#
# Run all tests: Rscript tests/testthat.R
# Or from R console: testthat::test_dir("tests/testthat")

library(testthat)

# Source all project modules before running tests
source(file.path("tests", "testthat", "helper-source.R"))

# Run all tests in the testthat directory
test_dir("tests/testthat", reporter = "summary")
